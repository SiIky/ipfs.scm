(module ipfs.v0
  (
   *scheme*
   *host*
   *port*

   rpc-call
   call-uri
   call-request
   make-uri
   make-request

   reader/json
   reader/plain
   writer/file
   writer/filesystem

   make-rpc-lambda
   export-rpc-call
   )

  (import
    (except scheme
            apply
            force
            truncate
            write)
    (only chicken.base
          assert
          cute
          define-constant
          make-parameter
          o)
    (only chicken.io read-string)
    (only chicken.module export)
    (only chicken.string ->string))

  (import
    (rename (only medea read-json)
            (read-json json:read))
    (rename (only uri-common make-uri)
            (make-uri uri:make))
    (rename (only intarweb make-request)
            (make-request request:make))
    (only http-client with-input-from-request)
    matchable)

  (import
    (only srfi-1
          append-map
          filter)
    (only srfi-189
          just
          just?
          maybe->values
          maybe-map
          maybe?
          nothing
          nothing?)

    (only srfi-197
          chain
          chain-lambda))

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
  (define (http-api-path endpoint-path)
    `(/ ,%api-base% ,%version% ,@(map symbol->string endpoint-path)))

  (define reader/json json:read)
  (define reader/plain read-string)

  (define (writer/file path #!key filename headers)
    (let ((filename (or filename path))
          (filename-entry (if filename `(#:filename ,filename) '()))
          (headers-entry (if headers `(#:headers ,headers) '())))
      `((,filename #:file ,path ,@filename-entry ,@headers-entry))))

  (define (writer/filesystem)
    ; TODO: Implement reading files & traversing a file system to write the
    ;       request body.
    "")

  (define (make-uri #!key (scheme (*scheme*)) (host (*host*)) (port (*port*)) path query)
    (uri:make
      #:scheme scheme
      #:host host
      #:port port
      #:path path
      #:query query))

  (define (make-request uri)
    (request:make
      #:method 'POST
      #:uri uri))

  (define (call-request request #!key reader writer)
    (with-input-from-request request writer reader))

  (define (call-uri uri #!key reader writer)
    (call-request (make-request uri) #:reader reader #:writer writer))


  (define make-query
    (chain-lambda
      (map
        ; (K, Maybe V) -> Maybe (K, V)
        (match-lambda ((k . v) (maybe-map (cute cons k <>) v)))
        _)
      (filter just? _)
      (append-map
        (o
          ; (K, V) -> [(K, V)]
          list
          ; (K, [V]) -> [(K, V)]
          ;((match-lambda ((k . v) (map (cute cons k <>) v))) _)
          ; Just (K, V) -> (K, V)
          maybe->values)
        _)))


  (define (->maybe x)
    (if (maybe? x)
      x
      (just x)))

  (define (->list x) (if (list? x) x (list x)))
  (define ((list-wrapper type-cast) name value)
    (->list (type-cast name value)))
  (define ((type-wrapper type-cast) argname value)
    (chain value
           (->maybe _)
           ; TODO: Use maybe-bind instead so that type functions may fail
           (maybe-map (cute type-cast argname <>) _)))

  (define (*->bool name value) (if value "true" "false"))
  (define (*->string name value) (->string value))
  (define (*->number name n)
    (assert (number? n) (string-append (symbol->string name) " must be an integer"))
    n)

  (define (*->array name lst)
    (assert (and (list? lst) (every string? lst))
            (string-append (symbol->string name) " must be list of strings"))
    (string-append "[" (string-join lst ",") "]"))

  ;; NOTE: The only types listed on the official documentation, as of now, are:
  ;;   * Bool
  ;;   * Int (int, uint, int64)
  ;;   * String
  ;;   * Array
  ;; @see https://docs.ipfs.io/reference/http/api
  ;; TODO: Find the difference between the integer types for the API
  (define Bool (type-wrapper *->bool))
  (define Int (type-wrapper *->number))
  (define String (type-wrapper *->string))

  ; TODO
  (define (Array type)
    (type-wrapper *->array))


  (define (rpc-call path arguments flags #!key reader writer)
    (chain (append (make-query arguments)
                   (make-query flags))
           (make-uri #:path path #:query _)
           (call-uri _ #:reader reader #:writer writer)))

  (define (yes argname value)
    (assert (not (nothing? value))
            (string-append (symbol->string argname) " is required"))
    value)
  (define (no argname value) value)

  ;;;
  ;;; Helper macros
  ;;;

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
  ;; @param arguments A list of the form ((argument type required?) ...) that
  ;;   specifies the list of arguments. `argument` is the argument's name, used
  ;;   as the keyword argument in the defined procedure. `type` is the type
  ;;   procedure that corresponds to the expected type. `required?` is `yes` or
  ;;   `no` according to whether the argument is required or not. These are
  ;;   sent to the server in the query string with the key `arg`.
  ;;
  ;; @param flags A list of the form ((flag type) ...) that specifies the list
  ;;   of flags. `flag` is the name of the flag, used for both the keyword
  ;;   argument in the defined procedure, as well as the key in the query
  ;;   string sent to the server. `type` is the type procedure that corresponds
  ;;   to the expected type.
  ;;
  ;; Used in the form (make-rpc-lambda path arguments flags), that is:
  ;;
  ;; (make-rpc-lambda
  ;;   (default-reader default-writer)
  ;;   (component ...)
  ;;   ((argument atype required?) ...)
  ;;   ((flag ftype) ...))
  ;;
  ;; `required?` can be either `yes` or `no`. The type procedures are `Bool`,
  ;;   `Int`, `String`, and `Array` (TBI).
  ;;
  ;; @see export-rpc-call
  (define-syntax make-rpc-lambda
    (syntax-rules ()
      ((make-rpc-lambda _ () _ _)
       (syntax-error "The endpoint path must not be empty"))

      ((make-rpc-lambda ()            path arguments flags)
       (make-rpc-lambda (reader/json) path arguments flags))

      ((make-rpc-lambda (default-reader)    path arguments flags)
       (make-rpc-lambda (default-reader #f) path arguments flags))

      ((make-rpc-lambda
         (default-reader default-writer)
         (component ...)
         ((argument atype required?) ...)
         ((flag ftype) ...))

       (let ((path (http-api-path '(component ...))))
         (lambda (#!key
                  (reader default-reader)
                  (writer default-writer)
                  (argument %nothing%) ...
                  (flag %nothing%) ...)
           (rpc-call path
                     `((arg . ,(atype 'argument (required? 'argument argument))) ...)
                     `((flag . ,(ftype 'flag flag)) ...)
                     #:reader reader
                     #:writer writer))))))

  (import procedural-macros)
  (import-for-syntax
    bindings
    (only chicken.syntax strip-syntax)
    (only srfi-13 string-join)
    (only srfi-197 chain))

  ;; @brief Defines and exports an RPC procedure created with make-rpc-lambda.
  ;;
  ;; Used in the form
  ;;   (export-rpc-call (default-reader default-writer) (path . arguments) . flags)
  ;; or in more detail
  ;;   (export-rpc-call
  ;;     (default-reader default-writer)
  ;;     ((component ...)
  ;;      (argument type required?) ...)
  ;;     (flag type) ...)
  ;;
  ;; @see make-rpc-lambda
  (define-macro
    (export-rpc-call reader/writer (path . arguments) . flags)
    (with-implicit-renaming
      (=? %name)
      (let* ((%name
               (chain path
                      (map (o symbol->string strip-syntax) _)
                      (string-join _ "/")
                      (string->symbol _))))
        `(begin
           (export ,%name)
           (define ,%name (make-rpc-lambda ,reader/writer ,path ,arguments ,flags))))))

  ;;;
  ;;; Enpoint procedures
  ;;;

  ; List of all the endpoints still not defined according to the docs.
  (export-rpc-call () ((bitswap ledger)))
  (export-rpc-call () ((bitswap reprovide)))
  (export-rpc-call () ((bitswap stat)))
  (export-rpc-call () ((bitswap wantlist)))
  (export-rpc-call () ((block get)))
  (export-rpc-call () ((block put)))
  (export-rpc-call () ((block rm)))
  (export-rpc-call () ((block stat)))
  (export-rpc-call () ((bootstrap)))
  (export-rpc-call () ((bootstrap add)))
  (export-rpc-call () ((bootstrap add default)))
  (export-rpc-call () ((bootstrap list)))
  (export-rpc-call () ((bootstrap rm)))
  (export-rpc-call () ((bootstrap rm all)))
  (export-rpc-call () ((cid base32)))
  (export-rpc-call () ((cid bases)))
  (export-rpc-call () ((cid format)))
  (export-rpc-call () ((cid hashes)))
  (export-rpc-call () ((dag export)))
  (export-rpc-call () ((dag get)))
  (export-rpc-call () ((dag import)))
  (export-rpc-call () ((dag put)))
  (export-rpc-call () ((dag resolve)))
  (export-rpc-call () ((dag stat)))
  (export-rpc-call () ((dht findpeer)))
  (export-rpc-call () ((dht findprovs)))
  (export-rpc-call () ((dht get)))
  (export-rpc-call () ((dht provide)))
  (export-rpc-call () ((dht put)))
  (export-rpc-call () ((dht query)))
  (export-rpc-call () ((diag cmds)))
  (export-rpc-call () ((diag cmds clear)))
  (export-rpc-call () ((diag cmds set-time)))
  (export-rpc-call () ((diag profile)))
  (export-rpc-call () ((diag sys)))
  (export-rpc-call () ((dns)))
  (export-rpc-call () ((filestore dups)))
  (export-rpc-call () ((filestore ls)))
  (export-rpc-call () ((filestore verify)))
  (export-rpc-call () ((key export)))
  (export-rpc-call () ((key gen)))
  (export-rpc-call () ((key import)))
  (export-rpc-call () ((key list)))
  (export-rpc-call () ((key rename)))
  (export-rpc-call () ((key rm)))
  (export-rpc-call () ((key rotate)))
  (export-rpc-call () ((log level)))
  (export-rpc-call () ((log ls)))
  (export-rpc-call () ((log tail)))
  (export-rpc-call () ((ls)))
  (export-rpc-call () ((mount)))
  (export-rpc-call () ((multibase decode)))
  (export-rpc-call () ((multibase encode)))
  (export-rpc-call () ((multibase list)))
  (export-rpc-call () ((multibase transcode)))
  (export-rpc-call () ((object data)))
  (export-rpc-call () ((object diff)))
  (export-rpc-call () ((object get)))
  (export-rpc-call () ((object links)))
  (export-rpc-call () ((object new)))
  (export-rpc-call () ((object patch add-link)))
  (export-rpc-call () ((object patch append-data)))
  (export-rpc-call () ((object patch rm-link)))
  (export-rpc-call () ((object patch set-data)))
  (export-rpc-call () ((object put)))
  (export-rpc-call () ((object stat)))
  (export-rpc-call () ((p2p close)))
  (export-rpc-call () ((p2p forward)))
  (export-rpc-call () ((p2p listen)))
  (export-rpc-call () ((p2p ls)))
  (export-rpc-call () ((p2p stream close)))
  (export-rpc-call () ((p2p stream ls)))
  (export-rpc-call () ((pubsub ls)))
  (export-rpc-call () ((pubsub peers)))
  (export-rpc-call () ((pubsub pub)))
  (export-rpc-call () ((pubsub sub)))
  (export-rpc-call () ((refs)))
  (export-rpc-call () ((refs local)))
  (export-rpc-call () ((repo fsck)))
  (export-rpc-call () ((repo gc)))
  (export-rpc-call () ((repo stat)))
  (export-rpc-call () ((repo verify)))
  (export-rpc-call () ((repo version)))
  (export-rpc-call () ((shutdown)))
  (export-rpc-call () ((stats bitswap)))
  (export-rpc-call () ((stats bw)))
  (export-rpc-call () ((stats dht)))
  (export-rpc-call () ((stats provide)))
  (export-rpc-call () ((stats repo)))
  (export-rpc-call () ((swarm addrs)))
  (export-rpc-call () ((swarm addrs listen)))
  (export-rpc-call () ((swarm addrs local)))
  (export-rpc-call () ((swarm connect)))
  (export-rpc-call () ((swarm disconnect)))
  (export-rpc-call () ((swarm filters)))
  (export-rpc-call () ((swarm filters add)))
  (export-rpc-call () ((swarm filters rm)))
  (export-rpc-call () ((swarm peering add)))
  (export-rpc-call () ((swarm peering ls)))
  (export-rpc-call () ((swarm peering rm)))
  (export-rpc-call () ((swarm peers)))
  (export-rpc-call () ((tar add)))
  (export-rpc-call () ((tar cat)))
  (export-rpc-call () ((update)))
  (export-rpc-call () ((urlstore add)))
  (export-rpc-call () ((version)))
  (export-rpc-call () ((version deps)))


  ;; The docs seem to suggest that some CLI commands don't have a corresponding
  ;;   HTTP endpoint. Endpoints that give HTTP 404:
  ;(export-rpc-call (reader/plain) ((commands completion bash)))
  ;(export-rpc-call (reader/plain) ((config edit)))


  (export-rpc-call
    ()
    ((add))
    (quiet Bool)
    (quieter Bool)
    (silent Bool)
    (progress Bool)
    (trickle Bool)
    (only-hash Bool)
    (wrap-with-directory Bool)
    (chunker String)
    (pin Bool)
    (raw-leaves Bool)
    (nocopy Bool)
    (fscache Bool)
    (cid-version Int)
    (hash String)
    (inline Bool)
    (inline-limit Int))

  (export-rpc-call () ((bitswap ledger) (peer String yes)))
  (export-rpc-call () ((bitswap reprovide)))
  (export-rpc-call () ((bitswap stat)) (verbose Bool) (human Bool))
  (export-rpc-call () ((bitswap wantlist)) (peer String))

  (export-rpc-call (reader/plain) ((cat) (path String yes)) (offset Int) (length Int))

  (export-rpc-call () ((cid codecs)))

  (export-rpc-call () ((commands)) (flags Bool))

  (export-rpc-call ()             ((config) (key String yes) (value String no)) (bool Bool) (json Bool))
  (export-rpc-call ()             ((config profile apply) (profile String yes)) (dry-run Bool))
  (export-rpc-call (reader/plain) ((config replace)))
  (export-rpc-call ()             ((config show)))

  (export-rpc-call (reader/plain) ((files chcid) (path String no)) (cid-version Int) (hash String))
  (export-rpc-call (reader/plain) ((files cp) (from String yes) (to String yes)) (parents Bool))
  (export-rpc-call ()             ((files flush) (path String no)))
  (export-rpc-call ()             ((files ls) (path String no)) (long Bool) (U Bool))
  (export-rpc-call (reader/plain) ((files mkdir) (path String yes)) (parents Bool) (cid-version Int) (hash String))
  (export-rpc-call (reader/plain) ((files mv) (from String yes) (to String yes)))
  (export-rpc-call (reader/plain) ((files read) (path String yes)) (offset Int) (count Int))
  (export-rpc-call (reader/plain) ((files rm) (path String yes)) (recursive Bool) (force Bool))
  (export-rpc-call ()             ((files stat) (path String yes)) (format String) (hash Bool) (size Bool) (with-local Bool))
  (export-rpc-call (reader/plain) ((files write) (path String yes)) (offset Int) (create Bool) (parents Bool) (truncate Bool) (count Int) (raw-leaves Bool) (cid-version Int) (hash String))

  (export-rpc-call (reader/plain) ((get) (path String yes)) (output String) (archive Bool) (compress Bool) (compression-level Int))

  (export-rpc-call () ((id) (id String no)) (format String) (peerid-base String))

  (export-rpc-call () ((name publish) (path String yes)) (resolve Bool) (lifetime String) (allow-offline Bool) (ttl String) (key String) (quieter Bool) (ipns-base String))
  (export-rpc-call () ((name pubsub cancel) (path String yes)))
  (export-rpc-call () ((name pubsub state)))
  (export-rpc-call () ((name pubsub subs)) (ipns-base String))
  (export-rpc-call () ((name resolve) (name String no)) (recursive Bool) (nocache Bool) (dht-record-count Int) (dht-timeout String) (stream Bool))

  (export-rpc-call ()             ((pin add) (path String yes)) (recursive Bool) (progress Bool))
  (export-rpc-call ()             ((pin ls) (path String no)) (type String) (quiet Bool) (stream Bool))
  (export-rpc-call ()             ((pin rm) (path String yes)) (recursive Bool))
  (export-rpc-call ()             ((pin update) (old-path String yes) (new-path String yes)) (unpin Bool))
  (export-rpc-call ()             ((pin verify)) (verbose Bool) (quiet Bool))
  (export-rpc-call ()             ((pin remote add) (path String yes)) (service String) (name String) (background Bool))
  (export-rpc-call ()             ((pin remote ls)) (service String) (name String) (cid Array) (status Array))
  (export-rpc-call (reader/plain) ((pin remote rm)) (service String) (name String) (cid Array) (force Bool))
  (export-rpc-call (reader/plain) ((pin remote service add) (name String yes) (endpoint String yes) (key String yes)))
  (export-rpc-call ()             ((pin remote service ls)) (stat Bool))
  (export-rpc-call (reader/plain) ((pin remote service rm) (name String yes)))

  (export-rpc-call () ((ping) (id String yes)) (count Int))

  (export-rpc-call () ((resolve) (name String no)) (recursive Bool) (nocache Bool) (dht-record-count Int) (dht-timeout String) (stream Bool))
  )
