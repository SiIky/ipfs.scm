;; The `silent` flag seems to be the equivalent of the `ipfs` command's
;;   `--silent`/`--quieter` flags -- the difference between these two is only
;;   relevant to the CLI. The `silent` flag of the HTTP API, if given and true,
;;   asks the server to give back only one CID. In that case, for convenience,
;;   the reader may be `reader/json` instead of `reader/json+`.
;; When adding some filesystem tree, this CID seems to correspond to the
;;   "top-level" directory.
(export-rpc-call
  (reader/json+)
  (add)
  (Bool silent)
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
  (Int inline-limit)
  (String to-files))

(export-rpc-call
  ()
  (bitswap/ledger
    (String peer yes)))

(export-rpc-call
  (reader/plain)
  (bitswap/reprovide))

(export-rpc-call
  ()
  (bitswap/stat)
  (Bool verbose)
  (Bool human))

(export-rpc-call
  ()
  (bitswap/wantlist)
  (String peer))

(export-rpc-call
  (reader/plain)
  (block/get
    (String hash yes)))

(export-rpc-call
  ()
  (block/put)
  (String cid-codec)
  (String mhtype)
  (Int mhlen)
  (Bool pin)
  (Bool allow-big-block))

(export-rpc-call
  ()
  (block/rm
    (String cid yes))
  (Bool force)
  (Bool quiet))

(export-rpc-call
  ()
  (block/stat
    (String cid yes)))

(export-rpc-call
  ()
  (bootstrap))

(export-rpc-call
  ()
  (bootstrap/add
    (String peer no)))

(export-rpc-call
  ()
  (bootstrap/add/default))

(export-rpc-call
  ()
  (bootstrap/list))

(export-rpc-call
  ()
  (bootstrap/rm
    (String peer no)))

(export-rpc-call
  ()
  (bootstrap/rm/all))

(export-rpc-call
  (reader/plain)
  (cat
    (String path yes))
  (Int offset)
  (Int length))

(export-rpc-call
  ()
  (cid/base32
    (String cid yes)))

(export-rpc-call
  ()
  (cid/bases)
  (Bool prefix)
  (Bool numeric))

(export-rpc-call
  ()
  (cid/codecs)
  (Bool numeric)
  (Bool supported))

(export-rpc-call
  ()
  (cid/format
    (String cid yes))
  (String f)
  (String v)
  (String mc)
  (String b))

(export-rpc-call
  ()
  (cid/hashes)
  (Bool numeric)
  (Bool supported))

(export-rpc-call
  ()
  (commands)
  (Bool flags))

(export-rpc-call
  ()
  (config
    (String key yes)
    (String value no))
  (Bool bool)
  (Bool json))

(export-rpc-call
  ()
  (config/profile/apply
    (String profile yes))
  (Bool dry-run))

(export-rpc-call
  (reader/plain)
  (config/replace))

(export-rpc-call
  ()
  (config/show))

(export-rpc-call
  (reader/plain)
  (dag/export
    (String cid yes)))

(export-rpc-call
  (reader/plain)
  (dag/get
    (String object yes))
  (String output-codec))

(export-rpc-call
  ()
  (dag/import)
  (Bool pin-roots)
  (Bool silent)
  (Bool stats)
  (Bool allow-big-block))

(export-rpc-call
  ()
  (dag/put)
  (String store-codec)
  (String input-codec)
  (Bool pin)
  (String hash)
  (Bool allow-big-block))

(export-rpc-call
  ()
  (dag/resolve
    (String path yes)))

(export-rpc-call
  ()
  (dag/stat
    (String cid yes)))

(export-rpc-call
  ()
  (dht/query
    (String peer yes))
  (Bool verbose))

(export-rpc-call
  ()
  (diag/cmds)
  (Bool verbose))

(export-rpc-call
  (reader/plain)
  (diag/cmds/clear))

(export-rpc-call
  (reader/plain)
  (diag/cmds/set-time
    (String time yes)))

(export-rpc-call
  (reader/plain)
  (diag/profile)
  (String output)
  ((Array String) collectors)
  (String profile-time)
  (Int mutex-profile-fraction)
  (String block-profile-rate))

(export-rpc-call
  (reader/plain)
  (diag/sys))

(export-rpc-call
  (reader/plain)
  (files/chcid
    (String path no))
  (Int cid-version)
  (String hash))

(export-rpc-call
  (reader/plain)
  (files/cp
    (String from yes)
    (String to yes))
  (Bool parents))

(export-rpc-call
  ()
  (files/flush
    (String path no)))

(export-rpc-call
  ()
  (files/ls
    (String path no))
  (Bool long)
  (Bool U))

(export-rpc-call
  (reader/plain)
  (files/mkdir
    (String path yes))
  (Bool parents)
  (Int cid-version)
  (String hash))

(export-rpc-call
  (reader/plain)
  (files/mv
    (String from yes)
    (String to yes)))

(export-rpc-call
  (reader/plain)
  (files/read
    (String path yes))
  (Int offset)
  (Int count))

(export-rpc-call
  (reader/plain)
  (files/rm
    (String path yes))
  (Bool recursive)
  (Bool force))

(export-rpc-call
  ()
  (files/stat
    (String path yes))
  (Bool with-local))

(export-rpc-call
  (reader/plain)
  (files/write
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
  (filestore/dups))

(export-rpc-call
  ()
  (filestore/ls
    (String cid no))
  (Bool file-order))

(export-rpc-call
  ()
  (filestore/verify
    (String cid no))
  (Bool file-order))

(export-rpc-call
  (reader/plain)
  (get
    (String path yes))
  (String output)
  (Bool archive)
  (Bool compress)
  (Int compression-level))

(export-rpc-call
  ()
  (id
    (String peer no))
  (String format)
  (String peerid-base))

(export-rpc-call
  ()
  (key/gen
    (String name yes))
  (String type)
  (Int size)
  (String ipns-base))

(export-rpc-call
  ()
  (key/import
    (String name yes))
  (String ipns-base)
  (String format)
  (Bool allow-any-key-type))

(export-rpc-call
  ()
  (key/list)
  (Bool l)
  (String ipns-base))

(export-rpc-call
  ()
  (key/rename
    (String old-name yes)
    (String new-name yes))
  (Bool force)
  (Bool ipns-base))

(export-rpc-call
  ()
  (key/rm
    (String name yes))
  (Bool l)
  (String ipns-base))

(export-rpc-call
  ()
  (log/level
    (String subsystem yes)
    (String level yes)))

(export-rpc-call
  ()
  (log/ls))

(export-rpc-call
  (reader/plain)
  (log/tail))

(export-rpc-call
  ()
  (ls
    (String path yes))
  (Bool resolve-type)
  (Bool size)
  (Bool stream))

(export-rpc-call
  ()
  (mount)
  (String ipfs-path)
  (String ipns-path))

(export-rpc-call
  (reader/plain)
  (multibase/decode))

(export-rpc-call
  (reader/plain)
  (multibase/encode)
  (String b))

(export-rpc-call
  ()
  (multibase/list)
  (Bool prefix)
  (Bool numeric))

(export-rpc-call
  (reader/plain)
  (multibase/transcode)
  (String b))

(export-rpc-call
  ()
  (name/inspect)
  (String verify))

(export-rpc-call
  ()
  (name/publish
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
  (name/pubsub/cancel
    (String path yes)))

(export-rpc-call
  ()
  (name/pubsub/state))

(export-rpc-call
  ()
  (name/pubsub/subs)
  (String ipns-base))

(export-rpc-call
  ()
  (name/resolve
    (String name no))
  (Bool recursive)
  (Bool nocache)
  (UInt dht-record-count)
  (String dht-timeout)
  (Bool stream))

; TODO: Didn't understand the example response of the docs; try to get an
;       actual example.
(export-rpc-call
  ()
  (p2p/close)
  (Bool all)
  (String protocol)
  (String listen-address)
  (String target-address))

(export-rpc-call
  (reader/plain)
  (p2p/forward
    (String protocol yes)
    (String listen-endpoint yes)
    (String target-endpoint yes))
  (Bool allow-custom-protocol))

(export-rpc-call
  (reader/plain)
  (p2p/listen
    (String protocol yes)
    (String target-endpoint yes))
  (Bool allow-custom-protocol)
  (Bool report-peer-id))

(export-rpc-call
  ()
  (p2p/ls)
  (Bool headers))

(export-rpc-call
  (reader/plain)
  (p2p/stream/close
    (String stream no))
  (Bool all))

(export-rpc-call
  ()
  (p2p/stream/ls)
  (Bool headers))

(export-rpc-call
  ()
  (pin/add
    (String path yes))
  (Bool recursive))

(export-rpc-call
  ()
  (pin/ls
    (String path no))
  (String type)
  (Bool quiet)
  (Bool stream))

(export-rpc-call
  ()
  (pin/rm
    (String path yes))
  (Bool recursive))

(export-rpc-call
  ()
  (pin/update
    (String old-path yes)
    (String new-path yes))
  (Bool unpin))

(export-rpc-call
  ()
  (pin/verify)
  (Bool verbose)
  (Bool quiet))

(export-rpc-call
  ()
  (pin/remote/add
    (String path yes))
  (String service)
  (String name)
  (Bool background))

(export-rpc-call
  ()
  (pin/remote/ls)
  (String service)
  (String name)
  ((Array String) cid)
  ((Array String) status))

(export-rpc-call
  (reader/plain)
  (pin/remote/rm)
  (String service)
  (String name)
  ((Array String) cid)
  ((Array String) status)
  (Bool force))

(export-rpc-call
  (reader/plain)
  (pin/remote/service/add
    (String name yes)
    (String endpoint yes)
    (String key yes)))

(export-rpc-call
  ()
  (pin/remote/service/ls)
  (Bool stat))

(export-rpc-call
  (reader/plain)
  (pin/remote/service/rm
    (String name yes)))

(export-rpc-call
  ()
  (ping
    (String peer yes))
  (Int count))

(export-rpc-call
  (reader/json+)
  (refs
    (String path yes))
  (String format)
  (Bool edges)
  (Bool unique)
  (Bool recursive)
  (Int max-depth))

(export-rpc-call
  (reader/json+)
  (refs/local))

(export-rpc-call
  ()
  (repo/gc)
  (Bool stream-errors)
  (Bool quiet))

(export-rpc-call
  ()
  (repo/ls))

(export-rpc-call
  ()
  (repo/stat)
  (Bool size-only)
  (Bool human))

(export-rpc-call
  ()
  (repo/verify))

(export-rpc-call
  ()
  (repo/version)
  (Bool quiet))

(export-rpc-call
  ()
  (resolve
    (String name yes))
  (Bool recursive)
  (Int dht-record-count)
  (String dht-timeout))

(export-rpc-call
  ()
  (routing/findpeer
    (String peer yes))
  (Bool verbose))

(export-rpc-call
  ()
  (routing/findprovs
    (String key yes))
  (Bool verbose)
  (Int num-providers))

(export-rpc-call
  ()
  (routing/get
    (String key yes)))

(export-rpc-call
  ()
  (routing/provide
    (String key yes))
  (Bool verbose)
  (Bool recursive))

(export-rpc-call
  ()
  (routing/put
    (String key yes)))

(export-rpc-call
  (reader/plain)
  (shutdown))

(export-rpc-call
  ()
  (stats/bitswap)
  (Bool verbose)
  (Bool human))

; Polling hangs? Kinda makes sense but...
(export-rpc-call
  ()
  (stats/bw)
  (String peer)
  (String proto)
  (Bool poll)
  (String interval))

(export-rpc-call
  ()
  (stats/dht
    (String dht no)))

(export-rpc-call
  ()
  (stats/provide))

(export-rpc-call
  ()
  (stats/repo)
  (Bool size-only)
  (Bool human))

(export-rpc-call
  ()
  (swarm/addrs))

(export-rpc-call
  ()
  (swarm/addrs/listen))

(export-rpc-call
  ()
  (swarm/addrs/local)
  (Bool id))

(export-rpc-call
  ()
  (swarm/connect
    (String peer yes)))

(export-rpc-call
  ()
  (swarm/disconnect
    (String peer yes)))

(export-rpc-call
  ()
  (swarm/filters))

(export-rpc-call
  ()
  (swarm/filters/add
    (String filter yes)))

(export-rpc-call
  ()
  (swarm/filters/rm
    (String filter yes)))

(export-rpc-call
  ()
  (swarm/peering/add
    (String peer yes)))

(export-rpc-call
  ()
  (swarm/peering/ls))

(export-rpc-call
  ()
  (swarm/peering/rm
    (String peer yes)))

(export-rpc-call
  ()
  (swarm/peers)
  (Bool verbose)
  (Bool streams)
  (Bool latency)
  (Bool direction)
  (Bool identify))

(export-rpc-call
 ()
 (swarm/resources))

(export-rpc-call
  ()
  (version)
  (Bool number)
  (Bool commit)
  (Bool repo)
  (Bool all))

(export-rpc-call
  ()
  (version/deps))
