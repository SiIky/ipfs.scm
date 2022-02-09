(import chicken.base)

(import
  intarweb
  uri-common)

(import ipfs)

(receive (res uri rep) (add #:writer (writer/file "ipfs.scm") #:pin #f #:cid-version 1)
  (print res)
  (print (uri->string uri))
  (print rep))
