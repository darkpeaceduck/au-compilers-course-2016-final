THIS_FILE=$(lastword $(MAKEFILE_LIST))

MTESTS=$(sort $(basename $(wildcard mtest*.expr)))
#MTESTS=mtest004
#MTESTS=
STESTS=$(sort $(basename $(wildcard test*.expr)))
#STESTS=test029
#STESTS=
TESTS=$(MTESTS) $(STESTS) # all
#TESTS=$(MTESTS) $(sort $(filter-out test030, $(filter-out test029, $(STESTS)))) # strings builtins
#TESTS=$(sort $(filter-out test011, $(basename $(wildcard test*.expr)))) # cycles
#TESTS=$(sort $(filter-out test011, $(basename $(wildcard test0[01][012345678].expr))) test009) # if + while

TESTSI=$(addsuffix .i, $(TESTS))
TESTSS=$(addsuffix .s, $(TESTS))
TESTSO=$(addsuffix .o, $(TESTS))
VALGRIND_ARGS=valgrind --leak-check=full --show-leak-kinds=all --show-reachable=no --error-exitcode=1

DIFLAGS=

.PHONY: byte native check int stack x86 $(TESTS) $(TESTSI) $(TESTSS) $(TESTSO) b n a i s o c

.DEFAULT_GOAL := check

CMP := native

byte: _byte
b: _byte
_byte:
	@: $(eval export CMP=byte)

native: _native
n: _native
_native:
	@: $(eval export CMP=native)

check: _check
a: _check
_check: $(TESTS)

int: _int
i: _int
_int: $(TESTSI)

stack: _stack
s: _stack
_stack: $(TESTSS)

x86: _x86
o: _x86
_x86: $(TESTSO)

$(TESTS): %: %.expr
	@$(MAKE) -f $(THIS_FILE) --no-print-directory $@.i
	@$(MAKE) -f $(THIS_FILE) --no-print-directory $@.s
	@$(MAKE) -f $(THIS_FILE) --no-print-directory $@.o

$(TESTSI): %.i: %.expr
	$(eval FILE:=$(basename $@))
	cat $(FILE).input | ../rc.$(CMP) -i $< > $(FILE).log && diff $(DIFLAGS) $(FILE).log orig/$(FILE).log

$(TESTSS): %.s: %.expr
	$(eval FILE:=$(basename $@))
	cat $(FILE).input | ../rc.$(CMP) -s $< > $(FILE).log && diff $(DIFLAGS) $(FILE).log orig/$(FILE).log

$(TESTSO): export RUNTIME_SRC=../runtime/
$(TESTSO): %.o: %.expr
	$(eval FILE:=$(basename $@))
	../rc.$(CMP) -o $< && cat $(FILE).input | $(VALGRIND_ARGS)  ./$(FILE) > $(FILE).log && diff $(DIFLAGS) $(FILE).log orig/$(FILE).log

clean: _clean
c: _clean
_clean:
	rm -f *.log *.s *~ $(TESTS)
