# ipfs.scm

**[CHICKEN Scheme] library for the [Kubo] [RPC API], the reference [IPFS] node
implementation.**

This document will serve as implementation documentation. For user
documentation take a look at the [CHICKEN wiki page]

## Updating endpoints

I'll try to keep this library up-to-date with the latest Kubo release. For most
updates only the endpoints need any changes (adding/removing endpoints and/or
parameters). This section documents the best process I've found so far.

The relevant Kubo source code is on the main repo at
[`/core/commands`](https://github.com/ipfs/kubo/tree/master/core/commands). The
source of the API page is on the docs repo at
[`/docs/reference/kubo/rpc.md`](https://github.com/ipfs/ipfs-docs/blob/main/docs/reference/kubo/rpc.md).

It's helpful to see the diff between versions, so clone the docs repo and
`git log -- docs/reference/kubo/rpc.md`. Try to find the last commit of the
previous version (usually stated on the commit message; `git log --grep` may
help) and then `git diff $COMMIT..main -- docs/reference/kubo/rpc.md`, assuming
`main` already has the docs for the latest release.

Unfortunately, some changes look like deletes and you have to scroll down to
find the related additions. This process is very manual, but is still better
than going over the whole API page.

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

## Exporting endpoints to Lua

This library is used as the "source of truth" for [`ipfs.lua`]. To generate the
Lua endpoints file run `csi -s export-to-lua.scm > /path/to/ipfs-endpoints.lua`
(no need to install any dependencies or compile anything).

[CHICKEN Scheme]: https://call-cc.org
[CHICKEN wiki page]: https://wiki.call-cc.org/eggref/5/ipfs
[IPFS API Implementation Doc]: https://github.com/ipfs/kubo/blob/3902c9be004e0a0e550d57c07a7068c7890deee5/docs/implement-api-bindings.md
[IPFS]: https://ipfs.io
[Kubo]: https://github.com/ipfs/kubo
[RPC API]: https://docs.ipfs.io/reference/kubo/rpc
[`ipfs.lua`]: https://git.sr.ht/~siiky/ipfs.lua
