CC := gcc
CFLAGS := -g -O2 -Wall
LDFLAGS := -lm -no-pie

TESTS = print sum-tail gcd sum fib ack even-odd \
adder funcomp cls-rec cls-bug cls-bug2 cls-reg-bug \
shuffle spill spill2 spill3 join-stack join-stack2 join-stack3 \
join-reg join-reg2 non-tail-if non-tail-if2 \
inprod inprod-rec inprod-loop matmul matmul-flat \

TRASH = \
	$(TESTS:%=test/%.s) \
	$(TESTS:%=test/%) \
	$(TESTS:%=test/%.res) \
	$(TESTS:%=test/%.ans) \
	$(TESTS:%=test/%.cmp)

default:
	dune build

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

reinstall: uninstall reinstall

clean:
	dune clean
	$(RM) $(TRASH)

do_test: $(TESTS:%=test/%.cmp)

test/%.s: test/%.ml
	dune exec min-caml test/$*
test/%: test/%.s libmincaml.S stub.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)
test/%.res: test/%
	$< > $@
test/%.ans: test/%.ml
	ocaml $< > $@
test/%.cmp: test/%.res test/%.ans
	diff $^ > $@

.PHONY: default install uninstall reinstall clean do_test
