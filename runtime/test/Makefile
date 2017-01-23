RUNTIME_DIR=../
FLAGS=--std=c++11 -m32 -I$(RUNTIME_DIR) -L$(RUNTIME_DIR)



all:build
	./test_allocator
	
.PHONY:build clean
build:test_allocator

test_allocator:test_allocator.cpp $(RUNTIME_DIR)/libruntime.a
	g++ $(FLAGS) test_allocator.cpp -lruntime -o test_allocator
	
clean:
	rm -f test_allocator
	
	