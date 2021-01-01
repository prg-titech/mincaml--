CC := gcc
CFLAGS := -m32 -g -O2 -Wall
LDFLAGS := -lm -no-pie

MINCAML := dune exec min-caml

PWD = $(shell pwd)
LIBS_X64 = ./external/x64
EXTERNAL_LIBS = -L$(LIBS_X64)/bdwgc/.libs -lgc

TESTS = $(basename $(wildcard test/*.ml))
TRASH = $(TESTS:%=%.s) $(TESTS:%=%.exe) $(TESTS:%=%.res)

default:
	dune build @install

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

reinstall: uninstall reinstall

clean:
	dune clean

cleantest:
	$(RM) $(TRASH)

dotest: $(TESTS:%=%.res)

gentest: $(TESTS:%=%.exe)

test/%.s: test/%.ml
	$(MINCAML) test/$*.ml

test/%.exe: test/%.s src/libmincaml.S src/lib.c src/stub.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

test/%.res: test/%.exe
	$< > $@

.PHONY: default install uninstall reinstall clean cleantest dotest gentest
