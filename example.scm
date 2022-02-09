(import chicken.base chicken.pretty-print)

(import
  intarweb
  uri-common)

(import ipfs)

(receive (res uri rep) (add #:writer (writer/file "ipfs.scm") #:pin #f #:cid-version 1)
  (pp res)
  (pp (uri->string uri))
  (pp rep)
  (let ((cid (alist-ref 'Hash res)))
    (receive (res uri rep) (pin/ls #:path cid)
      (pp res)
      (pp (uri->string uri))
      (pp rep))))
