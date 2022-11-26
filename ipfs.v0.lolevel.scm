(module ipfs.v0.lolevel
  (
   *scheme*
   *host*
   *port*

   rpc-call
   call-uri
   call-request
   make-uri
   make-request
   http-api-path

   reader/json
   reader/json+
   reader/plain
   writer/internal
   writer/internal*
   writer/directory
   writer/file
   writer/filesystem
   writer/directory*
   writer/file*

   %nothing%
   yes
   no

   Bool
   Int
   UInt
   String
   Array

   make-rpc-lambda
   export-rpc-call
   )

  (import
    scheme
    (only chicken.base
          alist-update
          assert
          constantly
          cute
          define-constant
          foldl
          make-parameter
          o
          open-input-string
          parameterize
          receive)
    (only chicken.file
          directory-exists?
          file-exists?
          find-files)
    (only chicken.io read-string)
    (only chicken.module export)
    (only chicken.pathname
          absolute-pathname?
          decompose-directory
          make-absolute-pathname
          make-pathname
          normalize-pathname)
    (only chicken.process-context current-directory)
    (only chicken.string ->string string-split)
    chicken.type)

  (import
    (only http-client with-input-from-request)
    (rename (only intarweb make-request)
            (make-request request:make))
    ; NOTE: I'm not too comfortable with medea deserializing objects into
    ;       alists with symbols as keys, but I assume no one will use this to
    ;       communicate with an IPFS node they don't own, and thus possibly
    ;       malicious...
    ; TODO: Change the reader to use strings instead?
    (rename (only medea read-json)
            (read-json json:read))
    (rename (only uri-common
                  make-uri
                  uri-encode-string
                  form-urlencoded-separator)
            (make-uri uri:make)
            (uri-encode-string uri:encode-string)))

  (import
    (only srfi-1
          filter
          filter-map
          last)
    (only srfi-13
          string-drop
          string-join
          string-prefix-length
          string-trim-right)
    (only srfi-189
          just
          just?
          maybe->values
          maybe-bind
          maybe-map
          maybe?
          nothing
          nothing?)
    (rename
      (only srfi-197
            chain
            chain-lambda)
      (chain =>)
      (chain-lambda ->)))

  (define-constant %api-base% "api")
  (define-constant %version% "v0")
  (define %nothing% (nothing))

  ; NOTE: HTTP rather than HTTPS because I expect using the library to
  ;       communicate with a locally running IPFS instance will be the norm.
  (define *scheme* (make-parameter 'http))
  (define *host* (make-parameter "localhost"))
  (define *port* (make-parameter 5001))

  ;; @brief Compute the HTTP path for some endpoint
  ;; @param endpoint-path List of symbols representing the path after the
  ;;   "/api/v0/". Example: '(commands completion bash) for the final HTTP path
  ;;   "/api/v0/commands/completion/bash"
  ;; @returns A list suitable to be given to uri-common.make-uri as the #:path
  ;;   parameter. Example: '(/ "api" "v0" "commands" "completion" "bash")
  (: http-api-path ((list-of (or symbol string)) --> (pair symbol (list-of string))))
  (define (http-api-path endpoint-path)
    `(/ ,%api-base% ,%version% ,@(map ->string endpoint-path)))

  ; TODO: Add the Abspath header
  (define (writer/internal* path #!optional name (headers '()))
    (let ((path (if path `(#:file ,path) '()))
          (name (if name `(#:filename ,(uri:encode-string name)) '()))
          (headers (if (and (list? headers) (not (null? headers)))
                       `(#:headers ,headers)
                       '())))
      `(file ,@path ,@name ,@headers)))

  (define (writer/internal path #!optional name (headers '()))
    (list (writer/internal* path name headers)))

  (: path-components (string --> (or false (list-of string))))
  (define (path-components path)
    (receive (_ _ components) (decompose-directory path)
      components))

  (: absolute-path-relative-to-directory ((or false string) string --> string))
  (define (absolute-path-relative-to-directory base-dir-components path)
    (=> path
        (normalize-pathname _)
        (path-components _)
        (make-absolute-pathname _ "")
        (make-absolute-pathname base-dir-components _)
        (normalize-pathname _)))

  (: longest-common-prefix ((list-of string) --> integer))
  (define (longest-common-prefix strs)
    (let ((s1 (car strs))
          (strs (cdr strs)))
      (foldl (lambda (l s2)
               (let ((l (min l (string-length s2))))
                 (string-prefix-length s1 s2 0 l 0 l)))
             (string-length s1)
             strs)))

  (: insert-sorted (procedure 'a (list-of 'a) --> (list-of 'a)))
  (define (insert-sorted <? elem lst)
    (cond
      ((null? lst) `(,elem))
      ((<? elem (car lst)) (cons elem lst))
      (else (cons (car lst) (insert-sorted <? elem (cdr lst))))))

  ;; @brief Compute a suitable name for a file or directory given its path.
  ;;
  ;; Only useful for single files or directories, not lists of them, because it
  ;; doesn't have enough context to compute a more meaninful (and unique)
  ;; name.
  (: path->name (string #!optional string --> string))
  (define (path->name path #!optional (cwd (current-directory)))
    (let* ((cwd (path-components cwd)))
      (=> path
          (absolute-path-relative-to-directory cwd _)
          (path-components _)
          (last _))))

  ;; @see `uri-common`'s `make-uri`
  (define (make-uri path query)
    (parameterize ((form-urlencoded-separator "&"))
      (uri:make #:scheme (*scheme*) #:host (*host*) #:port (*port*) #:path path #:query query)))

  ;; @see `intarweb`'s `make-request`
  (define (make-request uri)
    (request:make #:method 'POST #:uri uri))

  ;; @brief Wrapper around `http-client`'s `with-input-from-request`.
  ;; @see `http-client`'s `with-input-from-request`.
  (define (call-request request reader writer)
    (with-input-from-request request writer reader))

  ;; @brief Thin wrapper around `call-request`.
  ;; @see `call-request`
  (define (call-uri uri reader writer)
    (call-request (make-request uri) reader writer))

  ;; @brief Process the arguments and flags given to the procedure and create
  ;;   the query alist used in `make-uri`.
  ;; @param arguments The alist of the (key . maybe) pairs of each argument and
  ;;   flag.
  ;; @returns The final alist of (key . value) pairs used in `make-uri`.
  (define make-query
    ;        (K, Maybe V) -> Maybe (K, V)
    ;        (match-lambda ((k . v) (maybe-map (cute cons k <>) v)))
    (-> (map (lambda (kv) (maybe-map (cute cons (car kv) <>) (cdr kv))) _)
        (filter just? _)
        (map maybe->values _)))

  (define (->maybe x)
    (if (maybe? x)
        x
        (just x)))

  (define (->list x) (if (list? x) x (list x)))
  (define ((list-wrapper type-cast) name value)
    (->list (type-cast name value)))
  (define ((type-wrapper type-cast) argname value)
    (=> value
        (->maybe _)
        (maybe-bind _ (cute type-cast (->string argname) <>))))

  (define (*->bool name value) (just (if value "true" "false")))
  (define (*->string name value)
    (assert (not (not value))
            (string-append name " must not be false"))
    (just (->string value)))

  (define (*->int name n)
    (assert (integer? n)
            (string-append name " must be an integer"))
    (just n))
  (define (*->uint name n)
    (assert (and (integer? n) (positive? n))
            (string-append name " must be a positive integer"))
    (just n))

  (define ((*->array Type) name lst)
    (assert (list? lst)
            (string-append name " must be a list"))
    (let ((elem-name (string-append "element of " name)))
      (=> lst
          (map (o maybe->values (cute Type elem-name <>)) _)
          (string-join _ ",")
          (string-append "[" _ "]")
          (just _))))

  ;; TODO: Find the difference between the integer types for the API.
  ;; @see https://docs.ipfs.io/reference/http/api
  (define Bool (type-wrapper *->bool))
  (define Int (type-wrapper *->int))
  (define UInt (type-wrapper *->uint))
  (define String (type-wrapper *->string))
  (define (Array type)
    (type-wrapper (*->array type)))


  (define (rpc-call path arguments reader writer)
    (=> (make-query arguments)
        (make-uri path _)
        (call-uri _ reader writer)))

  (: yes (symbol 'a -> (or 'a noreturn)))
  (define (yes argname value)
    (assert (not (nothing? value))
            (string-append (symbol->string argname) " is required"))
    value)
  (: no (symbol 'a --> 'a))
  (define (no argname value) value)


  ;; @brief Read a reply as string
  ;; @returns A string with the reply's content
  ;; @see read-string
  (define reader/plain read-string)

  ;; @brief Read a JSON message
  ;; @returns A Scheme object representation of the JSON message
  ;;
  ;; Sets the consume-trailing-whitespace parameter to #f so that it may be
  ;;   used to read more than one JSON message from the same port.
  ;;
  ;; @see read-json from medea
  (define (reader/json #!optional (port (current-input-port)))
    ; NOTE: Apparently `#:consume-trailing-whitespace #f` is needed to be able
    ;       to read several JSON messages from the same port?
    (json:read port #:consume-trailing-whitespace #f))

  ;; @brief Read one or more JSON messages
  ;; @returns A list of Scheme object representations of the JSON messages
  ;; @see reader/json
  (define (reader/json+ #!optional (port (current-input-port)))
    (let loop ((ret '()))
      (let ((obj (reader/json port)))
        (if obj
            (loop (cons obj ret))
            ret))))

  (define (writer/file* path #!key name (headers '()))
    (writer/internal* path (or name (path->name path)) headers))

  ;; @brief Compute the appropriate body for a single file.
  ;; @param path The file's path -- read by the client, not the IPFS node.
  ;; @param name The name sent in the request; used as is.
  ;; @param headers Extra headers sent in the request for this file.
  ;; @returns An alist that can be used by `with-input-from-request` as the
  ;;   writer.
  ;;
  ;; If @a name is not given, `http-client` uses the basename of @a path.
  (define (writer/file path #!key name (headers '()))
    (list (writer/file* path #:name name #:headers headers)))

  (define (writer/directory* path #!key name (headers '()))
    (let ((name (or name (path->name path)))
          (headers (alist-update 'content-type '(application/x-directory) headers))
          (file (open-input-string "")))
      (writer/internal* file name headers)))

  ;; @brief Compute the appropriate body for a single directory.
  ;; @param path The directory's path -- read by the client, not the IPFS node.
  ;; @param name The name sent in the request; used as is.
  ;; @param headers Extra headers sent in the request for this directory.
  ;; @returns An alist that can be used by `with-input-from-request` as the
  ;;   writer.
  ;;
  ;; Always adds the Content-Type header set to application/x-directory. Sends
  ;; the *directory only*, not the files inside it! For that @see
  ;;   writer/filesystem.
  ;;
  ;; @see path->name
  ;;
  ;; TODO: Still not 100% satisfied with the behaviour...
  (define (writer/directory path #!key name (headers '()))
    (list (writer/directory* path #:name name #:headers headers)))

  ;; @brief Compute the appropriate body for the files of a filesystem tree.
  ;; @returns An alist that can be used by `with-input-from-request` as the
  ;;   writer.
  ;;
  ;; Wrapper around chicken.file.find-files. Except for `action` and `seed`
  ;;   takes all the same arguments with the same semantics.
  ;;
  ;; @a path is read by the client, not the IPFS node.
  ;;
  ;; The resulting files and directories will be sent to the server with the
  ;;   shortest possible names. That is, the longest common prefix of all the
  ;;   paths found will be cut from all of the paths to form the name of each
  ;;   file or directory entry.
  ;;
  ;; If @a path is relative, an attempt is made to transform it into an absolute path.
  ;;
  ;; @see chicken.file.find-files
  (define (writer/filesystem path #!key test limit dotfiles follow-symlinks)
    (assert (directory-exists? path) "path must be a directory")

    (define cwd-components (path-components (current-directory)))
    (define (abs-path-rel-to-cwd path)
      (if (absolute-pathname? path)
          path
          (absolute-path-relative-to-directory cwd-components path)))
    (define abs-path (abs-path-rel-to-cwd path))

    (define (shorten-by-longest-prefix paths)
      (let ((prefix-length (longest-common-prefix paths))
            (root (last (path-components abs-path))))
        (map (lambda (path)
               (=> path
                   (string-drop _ prefix-length)
                   (make-pathname root _)
                   (string-trim-right _ #\/)
                   (cons _ path)))
             paths)))

    (define (writer shortened-path/full-path)
      (let ((shortened-path (car shortened-path/full-path))
            (full-path (cdr shortened-path/full-path)))
        ((cond
           ((directory-exists? full-path) writer/directory*)
           ((file-exists? full-path) writer/file*)
           (else (constantly #f)))
         full-path #:name shortened-path)))

    (=> (find-files
          abs-path
          #:test (or test (constantly #t))
          #:limit limit
          #:dotfiles dotfiles
          #:follow-symlinks follow-symlinks
          #:seed '()
          #:action (cute insert-sorted string<? <> <>))
        (cons abs-path _)
        ; file-exists? returns #t for directories too
        (filter file-exists? _)
        (shorten-by-longest-prefix _)
        (filter-map writer _)))


  ;; @brief Creates a procedure that can be used to make an RPC call.
  ;;
  ;; @param default-reader/writer The default reader & writer thunks given to
  ;;   with-input-from-request, if none is given at the tiem of the call. Must
  ;;   be a list, of up to 2 elements, of the form (reader writer). If not
  ;;   given, reader defaults to reader/json and writer defaults to #f.
  ;;
  ;; @param path A list of the form (component ...) that denotes the path of
  ;;   the endpoint.
  ;;
  ;; @param arguments A list of the form ((atype argument required?) ...) that
  ;;   specifies the list of arguments. `argument` is the argument's name, used
  ;;   as the keyword argument in the defined procedure. `atype` is the type
  ;;   procedure that corresponds to the expected type. `required?` is `yes` or
  ;;   `no` according to whether the argument is required or not. Arguments are
  ;;   always sent to the server in the query string with the key `arg`.
  ;;
  ;; @param flags A list of the form ((ftype flag) ...) that specifies the list
  ;;   of flags. `flag` is the flag's name, used for both the keyword argument
  ;;   in the defined procedure, as well as the key in the query string sent to
  ;;   the server. `ftype` is the type procedure that corresponds to the
  ;;   expected type.
  ;;
  ;; Used in the form (make-rpc-lambda path arguments flags), that is:
  ;;
  ;; ((make-rpc-lambda
  ;;    (default-reader default-writer)
  ;;    (component ...)
  ;;    ((atype argument required?) ...)
  ;;    ((ftype flag) ...))
  ;;
  ;; `required?` can be either `yes` or `no`. The type procedures are `Bool`,
  ;;   `Int`, `String`, and `(Array Type)`.
  ;;
  ;; @see export-rpc-call
  (define-syntax make-rpc-lambda
    (syntax-rules ()
      ((make-rpc-lambda ()            name arguments flags)
       (make-rpc-lambda (reader/json) name arguments flags))

      ((make-rpc-lambda (default-reader)    name arguments flags)
       (make-rpc-lambda (default-reader #f) name arguments flags))

      ((make-rpc-lambda
         (default-reader default-writer)
         name
         ((atype argument required?) ...)
         ((ftype flag) ...))

       (let ((path (http-api-path (string-split (symbol->string name) "/" #f))))
         (lambda (#!key
                  (reader default-reader)
                  (writer default-writer)
                  (argument %nothing%) ...
                  (flag %nothing%) ...)
           (rpc-call path
                     `((arg . ,(atype 'argument (required? 'argument argument)))
                       ...
                       (flag . ,(ftype 'flag flag))
                       ...)
                     reader writer))))))

  ;; @brief Defines and exports an RPC procedure created with make-rpc-lambda.
  ;;
  ;; Used in the form
  ;;   (export-rpc-call (default-reader default-writer) (path . arguments) . flags)
  ;; or in more detail
  ;;   (export-rpc-call
  ;;     (default-reader default-writer)
  ;;     ((component ...)
  ;;      (atype argument required?) ...)
  ;;     (ftype flag) ...)
  ;;
  ;; @see make-rpc-lambda
  (define-syntax export-rpc-call
    (syntax-rules ()
      ((export-rpc-call reader/writer (name arguments ...) flags ...)
       (begin
         (export name)
         (define name
	   (make-rpc-lambda
	    reader/writer 'name (arguments ...)
	    ((Bool offline) (String timeout) flags ...)))))))
  )
