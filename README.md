# ipfs.scm

[CHICKEN Scheme] library for the [IPFS] HTTP API

The [official docs] define some clear terminology at the top of the page that
will be useful to know, even though they aren't themselves consistent
throughout that page (IMO):

 * **Arguments** (henceforth called **API arguments**) are positional arguments
   (that correspond to positional arguments of the related CLI command as well)
   given through the `arg` key on the query string of the request.

 * **Flags** (henceforth called **API flags**) are optional arguments (that
   correspond to flags/options of the related CLI command) given through their
   respective key on the query string of the request.

The procedures are named after the endpoint they're supposed to represent.
E.g., `add` for `/api/v0/add`, and `bitswap/ledger` for
`/api/v0/bitswap/ledger`.

API flags are given to the procedures as keyword arguments, with the name
defined in the API docs. E.g., the `cid-version` flag of `add` is given as the
`#:cid-version` keyword argument.

API arguments are given to the procedures as keyword arguments as well, with a
declarative name. E.g., the one and only argument to `bitswap/ledger` is called
`peer` in this library, even though it is sent to the server as `arg` in the
query string. Since only API arguments may be required, and it's not a PITA to
specify them, procedures check for required arguments. The only exception (that
I know of as of now) is the body of `add`, which must be given as the
`#:writer` keyword argument, but if not given the endpoint will be called
anyway.

All procedures have, on top of their arguments and flags, two extra keyword
arguments: `#:reader` & `#:writer`. These correspond, respectively, to the
`reader-thunk` & `writer-thunk` of [`with-input-from-request`] from
[`http-client`], and may be given on a per-RPC-call basis (for whatever reason;
e.g. performance maybe?). Unless noted otherwise (for some exceptions) all
procedures have a sane default value according to their corresponding endpoint
-- in general: `reader/json` for the reader and `#f` for the writer.
`reader/json` is just a rename of [`read-json`] from [`medea`].

The body of `add` (and maybe others) must be given as the `#:writer` keyword
argument, probably using the already defined writers `writer/file` &
`writer/filesystem`.

[CHICKEN Scheme]: https://call-cc.org/
[IPFS]: https://ipfs.io
[`http-client`]: https://wiki.call-cc.org/eggref/5/http-client
[`medea`]: https://wiki.call-cc.org/eggref/5/medea
[`read-json`]: https://api.call-cc.org/5/doc/medea/read-json
[`with-input-from-request`]: https://api.call-cc.org/5/doc/http-client/with-input-from-request
[official docs]: https://docs.ipfs.io/reference/http/api
