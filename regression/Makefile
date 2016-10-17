#TESTS=$(sort $(basename $(wildcard test*.expr)))
TESTS=test010 test014 test016 test017

.PHONY: check $(TESTS)

check: $(TESTS)

$(TESTS): export RUNTIME_O=../runtime/runtime.o
$(TESTS): %: %.expr
	cat $@.input | ../rc.byte    $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | ../rc.byte -s $< > $@.log && diff $@.log orig/$@.log
	../rc.byte -o $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log

clean:
	rm -f test*.log *.s *~ $(TESTS)
