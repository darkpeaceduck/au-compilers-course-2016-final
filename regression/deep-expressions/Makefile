TESTS=$(sort $(basename $(wildcard generated*.expr)))

.PHONY: check $(TESTS)

check: $(TESTS)

$(TESTS): export RUNTIME_SRC=../../runtime/
$(TESTS): %: %.expr
	cat $@.input | ../../rc.native -i $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | ../../rc.native -s $< > $@.log && diff $@.log orig/$@.log
	../../rc.native -o $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log

clean:
	rm -f *.log *.s *~ generated[0-9]*[0-9]
