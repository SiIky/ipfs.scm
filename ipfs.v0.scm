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
   http-api-path

   reader/json
   reader/plain
   writer/file
   writer/filesystem

   Bool
   Int
   String
   Array

   make-rpc-lambda
   export-rpc-call
   )

  (import
    (except scheme
            apply
            force
            log
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
    openssl
    matchable)

  (import
    (only srfi-1
          append-map
          every
          filter)
    (only srfi-13
          string-join)
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
  (define (http-api-path endpoint-path)
    `(/ ,%api-base% ,%version% ,@(map symbol->string endpoint-path)))

  (define reader/json json:read)
  (define reader/plain read-string)

  ;; @brief Compute the appropriate body for a single file.
  ;; @param path The file's path.
  ;; @param filename The filename sent in the request.
  ;; @param headers Extra headers sent in the request for this file.
  ;; @returns An alist that can be used by `with-input-from-request` as the
  ;;   writer.
  (define (writer/file path #!key filename headers)
    (let ((filename (or filename path))
          (filename-entry (if filename `(#:filename ,filename) '()))
          (headers-entry (if headers `(#:headers ,headers) '())))
      `((,filename #:file ,path ,@filename-entry ,@headers-entry))))

  ; TODO: Implement reading files & traversing a file system to write the
  ;       request body.
  (define (writer/filesystem path)
    "")

  ;; @see `uri-common`'s `make-uri`
  (define (make-uri #!key (scheme (*scheme*)) (host (*host*)) (port (*port*)) path query)
    (uri:make #:scheme scheme #:host host #:port port #:path path #:query query))

  ;; @see `intarweb`'s `make-request`
  (define (make-request uri)
    (request:make
      #:method 'POST
      #:uri uri))

  ;; @brief Wrapper around `http-client`'s `with-input-from-request`.
  ;; @see `http-client`'s `with-input-from-request`.
  (define (call-request request #!key reader writer)
    (with-input-from-request request writer reader))

  ;; @brief Thin wrapper around `call-request`.
  ;; @see `call-request`
  (define (call-uri uri #!key reader writer)
    (call-request (make-request uri) #:reader reader #:writer writer))


  ;; @brief Process the arguments and flags given to the procedure and create
  ;;   the query alist used in `make-uri`.
  ;; @param arguments The alist of the (key . maybe) pairs of each argument and
  ;;   flag.
  ;; @returns The final alist of (key . value) pairs used in `make-uri`.
  (define make-query
    (-> (map
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
    (=> value
        (->maybe _)
        ; TODO: Use maybe-bind instead so that type functions may fail
        (maybe-bind _ (cute type-cast (->string argname) <>))))

  (define (*->bool name value) (just (if value "true" "false")))
  (define (*->string name value)
    (assert (not (not value)) (string-append name "must not be false"))
    (just (->string value)))
  (define (*->number name n)
    (assert (number? n) (string-append name " must be an integer"))
    (just n))

  (define ((*->array Type) name lst)
    (assert (list? lst) (string-append name " must be a list"))
    (let ((elem-name (string-append "element of " name)))
      (=> lst
          (map (o maybe->values (cute Type elem-name <>)) _)
          (string-join _ ",")
          (string-append "[" _ "]")
          (just _))))

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

  (define (Array type)
    (type-wrapper (*->array type)))


  (define (rpc-call path arguments #!key reader writer)
    (=> (make-query arguments)
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
      ((make-rpc-lambda _ () _ _)
       (syntax-error "The endpoint path must not be empty"))

      ((make-rpc-lambda ()            path arguments flags)
       (make-rpc-lambda (reader/json) path arguments flags))

      ((make-rpc-lambda (default-reader)    path arguments flags)
       (make-rpc-lambda (default-reader #f) path arguments flags))

      ((make-rpc-lambda
         (default-reader default-writer)
         (component ...)
         ((atype argument required?) ...)
         ((ftype flag) ...))

       (let ((path (http-api-path '(component ...))))
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
                     #:reader reader
                     #:writer writer))))))

  (import procedural-macros)
  (import-for-syntax
    bindings
    (only chicken.syntax strip-syntax)
    (only srfi-13 string-join)
    (rename (only srfi-197 chain)
            (chain =>)))

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
  (define-macro
    (export-rpc-call reader/writer (path . arguments) . flags)
    (with-implicit-renaming
      (=? %name)
      (let ((%name
              (=> path
                  (map (o symbol->string strip-syntax) _)
                  (string-join _ "/")
                  (string->symbol _))))
        `(begin
           (export ,%name)
           (define ,%name (make-rpc-lambda ,reader/writer ,path ,arguments ,flags))))))

  ;;;
  ;;; Enpoint procedures
  ;;;

  ;; The docs seem to suggest that some CLI commands don't have a corresponding
  ;;   HTTP endpoint. Endpoints that give HTTP 404:
  ;(export-rpc-call (reader/plain) ((commands completion bash)))
  ;(export-rpc-call (reader/plain) ((config edit)))


  (export-rpc-call
    ()
    ((add))
    (Bool quiet)
    (Bool quieter)
    (Bool silent)
    (Bool progress)
    (Bool trickle)
    (Bool only-hash)
    (Bool wrap-with-directory)
    (String chunker)
    (Bool pin)
    (Bool raw-leaves)
    (Bool nocopy)
    (Bool fscache)
    (Int cid-version)
    (String hash)
    (Bool inline)
    (Int inline-limit))

  (export-rpc-call
    ()
    ((bitswap ledger)
     (String peer yes)))

  (export-rpc-call
    (reader/plain)
    ((bitswap reprovide)))

  (export-rpc-call
    ()
    ((bitswap stat))
    (Bool verbose)
    (Bool human))

  (export-rpc-call
    ()
    ((bitswap wantlist))
    (String peer))

  (export-rpc-call
    (reader/plain)
    ((block get)
     (String hash yes)))

  (export-rpc-call
    ()
    ((block put))
    (String format)
    (String mhtype)
    (Int mhlen)
    (Bool pin))

  (export-rpc-call
    ()
    ((block rm)
     (String hash yes))
    (Bool force)
    (Bool quiet))

  (export-rpc-call
    ()
    ((block stat)
     (String hash yes)))

  (export-rpc-call
    ()
    ((bootstrap)))

  (export-rpc-call
    ()
    ((bootstrap add)
     (String peer no))
    (Bool default))

  (export-rpc-call
    ()
    ((bootstrap add default)))

  (export-rpc-call
    ()
    ((bootstrap list)))

  (export-rpc-call
    ()
    ((bootstrap rm)
     (String peer no)))

  (export-rpc-call
    ()
    ((bootstrap rm all)))

  (export-rpc-call
    (reader/plain)
    ((cat)
     (String path yes))
    (Int offset)
    (Int length))

  (export-rpc-call
    ()
    ((cid base32)
     (String cid yes)))

  (export-rpc-call
    ()
    ((cid bases))
    (Bool prefix)
    (Bool numeric))

  (export-rpc-call
    ()
    ((cid codecs))
    (Bool numeric))

  (export-rpc-call
    ()
    ((cid format)
     (String cid yes))
    (String f)
    (String v)
    (String codec)
    (String b))

  (export-rpc-call
    ()
    ((cid hashes))
    (Bool numeric))

  (export-rpc-call
    ()
    ((commands))
    (Bool flags))

  (export-rpc-call
    ()
    ((config)
     (String key yes)
     (String value no))
    (Bool bool)
    (Bool json))

  (export-rpc-call
    ()
    ((config profile apply)
     (String profile yes))
    (Bool dry-run))

  (export-rpc-call
    (reader/plain)
    ((config replace)))

  (export-rpc-call
    ()
    ((config show)))

  (export-rpc-call
    (reader/plain)
    ((dag export)
     (String cid yes))
    (Bool progress))

  (export-rpc-call
    (reader/plain)
    ((dag get)
     (String object yes))
    (String output-codec))

  (export-rpc-call
    ()
    ((dag import))
    (Bool pin-roots)
    (Bool silent)
    (Bool stats))

  (export-rpc-call
    ()
    ((dag put))
    (String store-codec)
    (String input-codec)
    (Bool pin)
    (String hash))

  (export-rpc-call
    ()
    ((dag resolve)
     (String path yes)))

  (export-rpc-call
    ()
    ((dag stat)
     (String cid yes))
    (Bool progress))

  (export-rpc-call
    ()
    ((dht findpeer)
     (String peer yes))
    (Bool verbose))

  (export-rpc-call
    ()
    ((dht findprovs)
     (String key yes))
    (Bool verbose)
    (Int num-providers))

  (export-rpc-call
    ()
    ((dht get)
     (String key yes))
    (Bool verbose))

  (export-rpc-call
    ()
    ((dht provide)
     (String key yes))
    (Bool verbose)
    (Bool recursive))

  (export-rpc-call
    ()
    ((dht put)
     (String key yes))
    (Bool verbose))

  (export-rpc-call
    ()
    ((dht query)
     (String peer yes))
    (Bool verbose))

  (export-rpc-call
    ()
    ((diag cmds))
    (Bool verbose))

  (export-rpc-call
    (reader/plain)
    ((diag cmds clear)))

  (export-rpc-call
    (reader/plain)
    ((diag cmds set-time)
     (String time yes)))

  (export-rpc-call
    (reader/plain)
    ((diag profile))
    (String output)
    (String cpu-profile-time))

  (export-rpc-call
    (reader/plain)
    ((diag sys)))

  (export-rpc-call
    ()
    ((dns)
     (String domain yes))
    (Bool recursive))

  (export-rpc-call
    (reader/plain)
    ((files chcid)
     (String path no))
    (Int cid-version)
    (String hash))

  (export-rpc-call
    (reader/plain)
    ((files cp)
     (String from yes)
     (String to yes))
    (Bool parents))

  (export-rpc-call
    ()
    ((files flush)
     (String path no)))

  (export-rpc-call
    ()
    ((files ls)
     (String path no))
    (Bool long)
    (Bool U))

  (export-rpc-call
    (reader/plain)
    ((files mkdir)
     (String path yes))
    (Bool parents)
    (Int cid-version)
    (String hash))

  (export-rpc-call
    (reader/plain)
    ((files mv)
     (String from yes)
     (String to yes)))

  (export-rpc-call
    (reader/plain)
    ((files read)
     (String path yes))
    (Int offset)
    (Int count))

  (export-rpc-call
    (reader/plain)
    ((files rm)
     (String path yes))
    (Bool recursive)
    (Bool force))

  (export-rpc-call
    ()
    ((files stat)
     (String path yes))
    (String format)
    (Bool hash)
    (Bool size)
    (Bool with-local))

  (export-rpc-call
    (reader/plain)
    ((files write)
     (String path yes))
    (Int offset)
    (Bool create)
    (Bool parents)
    (Bool truncate)
    (Int count)
    (Bool raw-leaves)
    (Int cid-version)
    (String hash))

  (export-rpc-call
    ()
    ((filestore dups)))

  (export-rpc-call
    ()
    ((filestore ls)
     (String cid no))
    (Bool file-order))

  (export-rpc-call
    ()
    ((filestore verify)
     (String cid no))
    (Bool file-order))

  (export-rpc-call
    (reader/plain)
    ((get)
     (String path yes))
    (String output)
    (Bool archive)
    (Bool compress)
    (Int compression-level))

  (export-rpc-call
    ()
    ((id)
     (String peer no))
    (String format)
    (String peerid-base))

  (export-rpc-call
    (reader/plain)
    ((key export)
     (String key yes))
    (String output))

  (export-rpc-call
    ()
    ((key gen)
     (String name yes))
    (String type)
    (Int size)
    (String ipns-base))

  (export-rpc-call
    ()
    ((key import)
     (String name yes))
    (String ipns-base))

  (export-rpc-call
    ()
    ((key list))
    (Bool l)
    (String ipns-base))

  (export-rpc-call
    ()
    ((key rename)
     (String old-name yes)
     (String new-name yes))
    (Bool force)
    (Bool ipns-base))

  (export-rpc-call
    ()
    ((key rm)
     (String name yes))
    (Bool l)
    (String ipns-base))

  (export-rpc-call
    (reader/plain)
    ((key rotate))
    (String oldkey)
    (String type)
    (Int size))

  (export-rpc-call
    ()
    ((log level)
     (String subsystem yes)
     (String level yes)))

  (export-rpc-call
    ()
    ((log ls)))

  (export-rpc-call
    (reader/plain)
    ((log tail)))

  (export-rpc-call
    ()
    ((ls)
     (String path yes))
    (Bool headers)
    (Bool resolve-type)
    (Bool size)
    (Bool stream))

  (export-rpc-call
    ()
    ((mount))
    (String ipfs-path)
    (String ipns-path))

  (export-rpc-call
    (reader/plain)
    ((multibase decode)))

  (export-rpc-call
    (reader/plain)
    ((multibase encode))
    (String b))

  (export-rpc-call
    ()
    ((multibase list))
    (Bool prefix)
    (Bool numeric))

  (export-rpc-call
    (reader/plain)
    ((multibase transcode))
    (String b))

  (export-rpc-call
    ()
    ((name publish)
     (String path yes))
    (Bool resolve)
    (String lifetime)
    (Bool allow-offline)
    (String ttl)
    (String key)
    (Bool quieter)
    (String ipns-base))

  (export-rpc-call
    ()
    ((name pubsub cancel)
     (String path yes)))

  (export-rpc-call
    ()
    ((name pubsub state)))

  (export-rpc-call
    ()
    ((name pubsub subs))
    (String ipns-base))

  (export-rpc-call
    ()
    ((name resolve)
     (String name no))
    (Bool recursive)
    (Bool nocache)
    (Int dht-record-count)
    (String dht-timeout)
    (Bool stream))

  (export-rpc-call
    (reader/plain)
    ((object data)
     (String key yes)))

  ; TODO: Maybe change `obj1` & `obj2` to better names.
  (export-rpc-call
    ()
    ((object diff)
     (String obj1 yes)
     (String obj2 yes))
    (Bool verbose))

  ; Deprecated, use `dag/get` instead.
  ; TODO: Find all the deprecated endpoints.
  (export-rpc-call
    ()
    ((object get)
     (String key yes))
    (String data-encoding))

  (export-rpc-call
    ()
    ((object links)
     (String key yes))
    (Bool headers))

  (export-rpc-call
    ()
    ((object new)
     (String template no)))

  (export-rpc-call
    ()
    ((object patch add-link)
     (String hash yes)
     (String name yes)
     (String object yes))
    (Bool create))

  (export-rpc-call
    ()
    ((object patch append-data)
     (String hash yes)))

  (export-rpc-call
    ()
    ((object patch rm-link)
     (String hash yes)
     (String name yes)))

  (export-rpc-call
    ()
    ((object patch set-data)
     (String hash yes)))

  (export-rpc-call
    ()
    ((object put))
    (String inputenc)
    (String datafieldenc)
    (Bool pin)
    (Bool quiet))

  (export-rpc-call
    ()
    ((object stat)
     (String key yes))
    (Bool human))

  ; TODO: Didn't understand the example response of the docs; try to get an
  ;       actual example.
  (export-rpc-call
    ()
    ((p2p close))
    (Bool all)
    (String protocol)
    (String listen-address)
    (String target-address))

  (export-rpc-call
    (reader/plain)
    ((p2p forward)
     (String protocol yes)
     (String listen-endpoint yes)
     (String target-endpoint yes))
    (Bool allow-custom-protocol))

  (export-rpc-call
    (reader/plain)
    ((p2p listen)
     (String protocol yes)
     (String target-endpoint yes))
    (Bool allow-custom-protocol)
    (Bool report-peer-id))

  (export-rpc-call
    ()
    ((p2p ls))
    (Bool headers))

  (export-rpc-call
    (reader/plain)
    ((p2p stream close)
     (String stream no))
    (Bool all))

  (export-rpc-call
    ()
    ((p2p stream ls))
    (Bool headers))

  (export-rpc-call
    ()
    ((pin add)
     (String path yes))
    (Bool recursive)
    (Bool progress))

  (export-rpc-call
    ()
    ((pin ls)
     (String path no))
    (String type)
    (Bool quiet)
    (Bool stream))

  (export-rpc-call
    ()
    ((pin rm)
     (String path yes))
    (Bool recursive))

  (export-rpc-call
    ()
    ((pin update)
     (String old-path yes)
     (String new-path yes))
    (Bool unpin))

  (export-rpc-call
    ()
    ((pin verify))
    (Bool verbose)
    (Bool quiet))

  (export-rpc-call
    ()
    ((pin remote add)
     (String path yes))
    (String service)
    (String name)
    (Bool background))

  (export-rpc-call
    ()
    ((pin remote ls))
    (String service)
    (String name)
    ((Array String) cid)
    ((Array String) status))

  (export-rpc-call
    (reader/plain)
    ((pin remote rm))
    (String service)
    (String name)
    ((Array String) cid)
    ((Array String) status)
    (Bool force))

  (export-rpc-call
    (reader/plain)
    ((pin remote service add)
     (String name yes)
     (String endpoint yes)
     (String key yes)))

  (export-rpc-call
    ()
    ((pin remote service ls))
    (Bool stat))

  (export-rpc-call
    (reader/plain)
    ((pin remote service rm)
     (String name yes)))

  (export-rpc-call
    ()
    ((ping)
     (String peer yes))
    (Int count))

  (export-rpc-call
    ()
    ((pubsub ls)))

  (export-rpc-call
    ()
    ((pubsub peers)
     (String topic no)))

  (export-rpc-call
    (reader/plain)
    ((pubsub pub)
     (String topic yes)))

  (export-rpc-call
    ()
    ((pubsub sub)
     (String topic yes)))

  (export-rpc-call
    ()
    ((refs)
     (String path yes))
    (String format)
    (Bool edges)
    (Bool unique)
    (Bool recursive)
    (Int max-depth))

  (export-rpc-call
    ()
    ((refs local)))

  (export-rpc-call
    ()
    ((repo fsck)))

  (export-rpc-call
    ()
    ((repo gc))
    (Bool stream-errors)
    (Bool quiet))

  (export-rpc-call
    ()
    ((repo stat))
    (Bool size-only)
    (Bool human))

  (export-rpc-call
    ()
    ((repo verify)))

  (export-rpc-call
    ()
    ((repo version))
    (Bool quiet))

  (export-rpc-call
    ()
    ((resolve)
     (String name no))
    (Bool recursive)
    (Bool nocache)
    (Int dht-record-count)
    (String dht-timeout)
    (Bool stream))

  (export-rpc-call
    (reader/plain)
    ((shutdown)))

  (export-rpc-call
    ()
    ((stats bitswap))
    (Bool verbose)
    (Bool human))

  ; Polling hangs? Kinda makes sense but...
  (export-rpc-call
    ()
    ((stats bw))
    (String peer)
    (String proto)
    (Bool poll)
    (String interval))

  (export-rpc-call
    ()
    ((stats dht)
     (String dht no)))

  (export-rpc-call
    ()
    ((stats provide)))

  (export-rpc-call
    ()
    ((stats repo))
    (Bool size-only)
    (Bool human))

  (export-rpc-call
    ()
    ((swarm addrs)))

  (export-rpc-call
    ()
    ((swarm addrs listen)))

  (export-rpc-call
    ()
    ((swarm addrs local))
    (Bool id))

  (export-rpc-call
    ()
    ((swarm connect)
     (String peer yes)))

  (export-rpc-call
    ()
    ((swarm disconnect)
     (String peer yes)))

  (export-rpc-call
    ()
    ((swarm filters)))

  (export-rpc-call
    ()
    ((swarm filters add)
     (String filter yes)))

  (export-rpc-call
    ()
    ((swarm filters rm)
     (String filter yes)))

  (export-rpc-call
    ()
    ((swarm peering add)
     (String peer yes)))

  (export-rpc-call
    ()
    ((swarm peering ls)))

  (export-rpc-call
    ()
    ((swarm peering rm)
     (String peer yes)))

  (export-rpc-call
    ()
    ((swarm peers))
    (Bool verbose)
    (Bool streams)
    (Bool latency)
    (Bool directio))

  (export-rpc-call
    ()
    ((tar add)))

  (export-rpc-call
    (reader/plain)
    ((tar cat)
     (String path yes)))

  (export-rpc-call
    (reader/plain)
    ((update)
     (String arguments no)))

  (export-rpc-call
    ()
    ((urlstore add)
     (String url yes))
    (Bool trickle)
    (Bool pin))

  (export-rpc-call
    ()
    ((version))
    (Bool number)
    (Bool commit)
    (Bool repo)
    (Bool all))

  (export-rpc-call
    ()
    ((version deps)))
  )
