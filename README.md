# ipfs.scm

**[CHICKEN Scheme] library for the [IPFS] [HTTP API].**

This document will serve as implementation documentation. For user
documentation take a look at the [CHICKEN wiki page]

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
ipfs --api /ip4/127.0.0.1/tcp/5006 # ...
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

[CHICKEN Scheme]: https://call-cc.org
[CHICKEN wiki page]: https://wiki.call-cc.org/eggref/5/ipfs
[HTTP API]: https://docs.ipfs.io/reference/http/api
[IPFS API Implementation Doc]: https://github.com/ipfs/go-ipfs/blob/a494f48a9dd0a66bd682651b89e7503b5500ac2a/docs/implement-api-bindings.md
[IPFS]: https://ipfs.io
