all:builtins.o collector.o
	ar rc libruntime.a builtins.o collector.o 

builtins.o:builtins.cpp
	g++ --std=c++11 -m32 -c builtins.cpp

collector.o:collector.cpp
	g++ --std=c++11 -m32 -c collector.cpp

clean:
	rm -f runtime.a *~ builtins.o collector.o

