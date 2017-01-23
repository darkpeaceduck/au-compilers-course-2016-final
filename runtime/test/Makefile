RUNTIME_DIR=../
FLAGS=--std=c++11 -m32 -I$(RUNTIME_DIR) -L$(RUNTIME_DIR)

VALGRIND_ARGS=valgrind --leak-check=full --show-leak-kinds=all --show-reachable=no --error-exitcode=1 --undef-value-errors=no

all:build
	$(VALGRIND_ARGS) ./test_allocator
	
.PHONY:build clean
build:test_allocator

test_allocator:test_allocator.cpp $(RUNTIME_DIR)/libruntime.a
	g++ $(FLAGS) test_allocator.cpp -lruntime -o test_allocator
	
clean:
	rm -f test_allocator
	
	