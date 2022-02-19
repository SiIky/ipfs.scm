# ipfs.scm

**[CHICKEN Scheme] library for the [IPFS] [HTTP API].**

## Overview

The official docs define some clear terminology at the top of the page that
will be useful to know, even though sometimes they aren't themselves consistent
throughout that page (IMO):

 * **Arguments** (henceforth called **API arguments**) are positional arguments
   (that correspond to positional arguments of the related CLI command as well)
   given through the `arg` key on the query string of the request.

 * **Flags** (henceforth called **API flags**) are optional arguments (that
   correspond to flags/options of the related CLI command) given through their
   respective key on the query string of the request.

---

The exported procedures are named after the endpoint they're supposed to
represent. E.g., `add` for `/api/v0/add`, and `bitswap/ledger` for
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
`reader/json` is just a rename of [`read-json`] from [`medea`]. **NOTE:**
`medea` by default deserializes object keys as symbols, which isn't great.
There's the possibility to change the JSON reader to use strings instead, but
for now be warned: you shouldn't use this library with an untrusted IPFS node,
it may crash your program.

The body of `add` (and others) must be given as the `#:writer` keyword
argument, using for example the already defined writers `writer/file`,
`writer/directory`, or `writer/filesystem`.

## Examples & Low-level differences

Mainly documenting for myself, to compare the library and the `ipfs` command,
but also serve to show some uses of the library and the equivalents of the
`ipfs` command.

Results are in the `requests/` directory, named `<cmd>.<impl>.txt`, where
`<cmd>` is the name of the command and `<impl>` is either `ipfs` (the `ipfs`
command) or `chicken` (this library).

Just used netcat for this, as is suggested by [IPFS API Implementation Doc]:

```sh
nc -q 1 -l 5006

# And in another terminal
ipfs --api /ip4/127.0.0.1/tcp/5002 # ...
```

And for this library:

```scm
(parameterize ((*port* 5006))
  ; ...
  )
```

### `add` (`add`)

```sh
ipfs add --pin=false --cid-version=1 ipfs.scm
```

```scm
(add #:writer (writer/file "ipfs.scm") #:pin #f #:cid-version 1)
```

### `pin ls` (`pin_ls`)

The file `ipfs.scm` has (as of now) CIDv1
`bafkreicd47ynzf6it5ssdwryvhksy6ylm4s42jlwzjbxhv5zlkw6mwbooq`.

```sh
ipfs pin ls bafkreicd47ynzf6it5ssdwryvhksy6ylm4s42jlwzjbxhv5zlkw6mwbooq
```

```scm
(pin/ls #:path "bafkreicd47ynzf6it5ssdwryvhksy6ylm4s42jlwzjbxhv5zlkw6mwbooq")
```

## Missing endpoints

You may find that some endpoints aren't implemented. There are two
possibilities: the endpoint is new and it hasn't been implemented yet; or, the
endpoint has been deprecated or calling it gives HTTP 404. For the former,
issues/PRs are very welcome! For the latter, if they're gone, they're gone. If
you really need them, you can search the Git log. E.g. `git log -1 -p -S'(key
rotate)' -- ipfs.v0.scm` will probably show you [3a32380].

[3a32380]: https://github.com/siiky/ipfs.scm/commit/3a3238049c6b484a1a8dc926e441cd454ddc7d0c
[CHICKEN Scheme]: https://call-cc.org
[HTTP API]: https://docs.ipfs.io/reference/http/api
[IPFS API Implementation Doc]: https://github.com/ipfs/go-ipfs/blob/a494f48a9dd0a66bd682651b89e7503b5500ac2a/docs/implement-api-bindings.md
[IPFS]: https://ipfs.io
[`http-client`]: https://wiki.call-cc.org/eggref/5/http-client
[`medea`]: https://wiki.call-cc.org/eggref/5/medea
[`read-json`]: https://api.call-cc.org/5/doc/medea/read-json
[`with-input-from-request`]: https://api.call-cc.org/5/doc/http-client/with-input-from-request
