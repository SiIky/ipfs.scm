(module ipfs.v0
  (
   *scheme*
   *host*
   *port*

   reader/json
   reader/json+
   reader/plain
   writer/directory
   writer/file
   writer/filesystem
   writer/directory*
   writer/file*
   )

  (import scheme chicken.base ipfs.v0.lolevel)

  (include "ipfs.v0.endpoints.scm")
  )
