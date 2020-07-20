# MinCaml (modified for BacCaml)

A fork of the MinCaml compiler modified for the BacCaml language implementation framework.

# Installation

```shell
$ dune install bac-caml
```

# Usage

## Bytecode generation

```shell
$ bac-caml path/to/file.ml
```

## Execute with BacCaml meta JIT compiler

```shell
$ bac-caml path/to/file.ml | [interp].exe -- path/to/interp.mcml [options]
```
