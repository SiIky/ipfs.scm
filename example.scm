(import chicken.base chicken.pretty-print)

(import
  intarweb
  uri-common)

(import ipfs)

(receive (res uri rep) (add #:writer (writer/file "ipfs.scm") #:pin #f #:cid-version 1)
  (pp res)
  (pp (uri->string uri))
  (pp rep))
