# MinCaml (modified for BacCaml)

A fork of the MinCaml compiler modified for the BacCaml language implementation framework.

# Usage

## Bytecode generation

```shell
$ dune exec bytegen -- path/to/file.ml
```

## Compiling a file written in MinCaml

```shell
$ dune exec min-caml -- path/to/file.ml
$ gcc -m32 -Wall -O2 src/stub.c src/libmincaml/S path/to/file.s -lm -o path/to/file
```
