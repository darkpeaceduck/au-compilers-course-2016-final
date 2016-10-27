THIS_FILE=$(lastword $(MAKEFILE_LIST))

#TESTS=$(sort $(basename $(wildcard test*.expr)))
TESTS=$(sort $(filter-out test011, $(basename $(wildcard test*.expr)))) # cycles
#TESTS=$(sort $(filter-out test011, $(basename $(wildcard test0[01][012345678].expr))) test009) # if + while

TESTSI=$(addsuffix .i, $(TESTS))
TESTSS=$(addsuffix .s, $(TESTS))
TESTSO=$(addsuffix .o, $(TESTS))

.PHONY: byte native check int stack x86 $(TESTS) $(TESTSI) $(TESTSS) $(TESTSO)

.DEFAULT_GOAL := check

CMP := native

byte:
	@: $(eval export CMP=byte)

native:
	@: $(eval export CMP=native)

check: $(TESTS)

int: $(TESTSI)

stack: $(TESTSS)

x86: $(TESTSO)

$(TESTS): %: %.expr
	@$(MAKE) -f $(THIS_FILE) --no-print-directory $@.i
	@$(MAKE) -f $(THIS_FILE) --no-print-directory $@.s
	@$(MAKE) -f $(THIS_FILE) --no-print-directory $@.o

$(TESTSI): %.i: %.expr
	$(eval FILE:=$(basename $@))
	cat $(FILE).input | ../rc.$(CMP)    $< > $(FILE).log && diff $(FILE).log orig/$(FILE).log

$(TESTSS): %.s: %.expr
	$(eval FILE:=$(basename $@))
	cat $(FILE).input | ../rc.$(CMP) -s $< > $(FILE).log && diff $(FILE).log orig/$(FILE).log

$(TESTSO): export RUNTIME_O=../runtime/runtime.o
$(TESTSO): %.o: %.expr
	$(eval FILE:=$(basename $@))
	../rc.$(CMP) -o $< && cat $(FILE).input | ./$(FILE) > $(FILE).log && diff $(FILE).log orig/$(FILE).log

clean:
	rm -f *.log *.s *~ $(TESTS)
