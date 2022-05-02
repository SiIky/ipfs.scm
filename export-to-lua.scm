; This file is a redefinition of export-rpc-call to export the procedures
; defined here to the equivalent Lua definitions for ipfs.lua.

(import chicken.irregex)

(define (endpoint->proc-name endpoint)
  (irregex-replace/all "[-/]" endpoint "_"))

(define (reader/writer->lua reader/writer)
  (if (not reader/writer)
      "nil"
      (case reader/writer
        ((reader/json) "reader_json")
        ((reader/json+) "reader_json")
        ((reader/plain) "reader_plain")
        (else (error "Unexpected `reader/writer`" reader/writer)))))

(define (required?->lua required?)
  (case required?
    ((yes) "Yes")
    ((no) "No")
    (else (error "Unexpected `required?`"))))

(define (ftype->lua ftype)
  (if (list? ftype)
      (string-append (symbol->string (car ftype)) "(" (symbol->string (cadr ftype)) ")")
      (symbol->string ftype)))


(define-syntax export-rpc-call
  (syntax-rules ()
    ((export-rpc-call "internal" ()            name arguments flags)
     (export-rpc-call "internal" (reader/json) name arguments flags))

    ((export-rpc-call "internal" (default-reader)    name arguments flags)
     (export-rpc-call "internal" (default-reader #f) name arguments flags))

    ((export-rpc-call
       "internal"
       (default-reader default-writer)
       name
       ((atype required?) ...)
       ((ftype flag) ...))

     (print
       "  IPFS." (endpoint->proc-name name) " = make_ipfs_endpoint(\"" name "\",\n"
       "    " (reader/writer->lua 'default-reader) ",\n"
       ;"    " (reader/writer->lua 'default-writer) ",\n"

       ; Arguments
       "    {" (string-append "{" atype ", " required? "},") ... "},\n"

       ; Parameters
       "    {\n"
       (string-append "      [\"" flag "\"]=" ftype ",\n")
       ...
       "    }\n"
       "  )\n"
       ))

    ((export-rpc-call reader/writer (name (atype argument required?) ...) (ftype flag) ...)
     (export-rpc-call "internal"
                      reader/writer (symbol->string 'name)
                      (((symbol->string 'atype) (required?->lua 'required?)) ...)
                      (((ftype->lua 'ftype) (symbol->string 'flag)) ...)))))


(print
  "return function(tbl)\n"
  "  local Array = tbl.Array\n"
  "  local Bool = tbl.Bool\n"
  "  local Int = tbl.Int\n"
  "  local No = tbl.No\n"
  "  local String = tbl.String\n"
  "  local UInt = tbl.UInt\n"
  "  local Yes = tbl.Yes\n"
  "  local make_ipfs_endpoint = tbl.make_ipfs_endpoint\n"
  "  local reader_json = tbl.reader_json\n"
  "  local reader_plain = tbl.reader_plain\n"
  "\n"
  "  local IPFS = {}\n"
  )
(include "ipfs.v0.endpoints.scm")
(print
  "  return IPFS\n"
  "end")
