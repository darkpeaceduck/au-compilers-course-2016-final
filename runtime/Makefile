FLAGS=--std=c++11 -m32

all:builtins.o collector.o collector_s.o allocator.o
	ar rc libruntime.a builtins.o collector.o allocator.o collector_s.o

builtins.o:builtins.cpp
	g++ $(FLAGS) -c builtins.cpp

collector.o:collector.cpp
	g++ $(FLAGS) -c collector.cpp
	
collector_s.o:collector.s
	g++ $(FLAGS) collector.s -o collector_s.o -c

allocator.o:allocator.cpp
	g++ $(FLAGS) -c allocator.cpp 

clean:
	rm -f runtime.a *~ builtins.o collector.o allocator.o collector_s.o libruntime.a

