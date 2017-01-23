FLAGS=--std=c++11 -m32

all:builtins.o collector.o
	ar rc libruntime.a builtins.o collector.o 

builtins.o:builtins.cpp
	g++ $(FLAGS) -c builtins.cpp

collector.o:allocator.cpp collector.cpp
	g++ $(FLAGS) -c collector.cpp allocator.cpp 

clean:
	rm -f runtime.a *~ builtins.o collector.o

