TESTS=$(sort $(basename $(wildcard generated*.expr)))

.PHONY: check $(TESTS)

check: $(TESTS)

$(TESTS): export RUNTIME_O=../../runtime/runtime.o
$(TESTS): %: %.expr
	../../src/rc.byte -o $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log
	cat $@.input | ../../src/rc.byte $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | ../../src/rc.byte -s $< > $@.log && diff $@.log orig/$@.log

clean:
	rm -f *.log *.s *~ $(TESTS)
