# Ty's OCaml Monorepo

## Getting Started

The first time you set up the monorepo, run the prepare script.  Re-running
this script will do nothing if it thinks that the current setup is correct.

```bash
$ ./scripts/prepare.sh
```

To _really_ re-run the prepare script, clean all of the `_` directories by
running `./scripts/clean.sh`

## Setting Up Environment 

Every time you want to use the monorepo, you'll want to have the necessary
environment variables set for your process.  In bash, run

```bash
$ eval ./scripts/env.sh
$ nvim # or whatever editor you choose
```

┌──────────────────────────────────────────────────┬────────────┬────────────┐
│ Name                                             │   Time/Run │ Percentage │
├──────────────────────────────────────────────────┼────────────┼────────────┤
│ [lib/braid/bench/benchmarks.ml] tree:braid       │ 1_283.65us │     15.89% │
│ [lib/braid/bench/benchmarks.ml] tree:incremental │ 8_078.81us │    100.00% │
│ [lib/braid/bench/benchmarks.ml] tree:ocaml       │    71.41us │      0.88% │
└──────────────────────────────────────────────────┴────────────┴────────────┘
