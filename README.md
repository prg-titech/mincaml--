# MinCaml (modified for BacCaml)

A fork of the MinCaml compiler modified for the BacCaml language implementation framework.

# Installation

```shell
$ dune install mcmm
```

# Usage

## Bytecode generation

```shell
$ mcmm path/to/file.mcml
```

## Execute with BacCaml meta JIT compiler

```shell
$ mcmm path/to/file.ml | [interp].exe -- path/to/interp.mcml [options]
```
