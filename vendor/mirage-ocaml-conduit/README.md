## conduit -- an OCaml network connection establishment library

[![Build Status](https://travis-ci.org/mirage/ocaml-conduit.svg?branch=master)](https://travis-ci.org/mirage/ocaml-conduit)

The `conduit` library takes care of establishing and listening for 
TCP and SSL/TLS connections for the Lwt and Async libraries.

The reason this library exists is to provide a degree of abstraction
from the precise SSL library used, since there are a variety of ways
to bind to a library (e.g. the C FFI, or the Ctypes library), as well
as well as which library is used (just OpenSSL for now).

By default, OpenSSL is used as the preferred connection library, but
you can force the use of the pure OCaml TLS stack by setting the
environment variable `CONDUIT_TLS=native` when starting your program.

The opam packages available are:

- `conduit`: the main `Conduit` module
- `conduit-lwt`: the portable Lwt implementation
- `conduit-lwt-unix`: the Lwt/Unix implementation
- `conduit-async` the Jane Street Async implementation
- `conduit-mirage`: the MirageOS compatible implementation

### Debugging

Some of the `Lwt_unix`-based modules use a non-empty `CONDUIT_DEBUG`
environment variable to output debugging information to standard error.
Just set this variable when running the program to see what URIs
are being resolved to.

### Further Informartion

* **API Docs:** https://mirage.github.io/ocaml-conduit/
* **WWW:** https://github.com/mirage/ocaml-conduit
* **E-mail:** <mirageos-devel@lists.xenproject.org>
* **Bugs:** https://github.com/mirage/ocaml-conduit/issues
