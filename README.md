# simple-compiler

- [about](#about)
- [syntax](#syntax)
- [test-task](#test-task)

## about

Implementing a simple language.
What do we have so far:

|                                   | Interepreter | Stack Machine | x86 |
| --------------------------------- | ------------ | ------------- | --- |
| binops                            | [x]          | [x]           | [x] |
| if/while/for/repeat control flows | [x]          | [x]           | [x] |
| funcs                             | [x]          | [x]           | [x] |
| builtins                          | [x]          | [x]           | [x] |
| strings                           | [x]          | [x]           | [x] |
| arrays                            | [x]          | [x]           | [x] |
| garbage collector                 | -            | -             | [x] |

## syntax

### io

	x := read();
	write(x);

### binops

	+ - * / % <= < == != >= > !! &&

### control flows

	if expr then stmt else stmt fi
	while expr do stmt od
	repeat stmt until expr
	for stmt, expr, stmt do stmt od

### funcs

	fun name([args]) begin stmt end

### strings/chars

	"abracadabra" 'a'

### builtins

	n := strlen("abra")

### arrays

	a := {[1, 2], [4, 5]}
	b := {"asd", "asd"}
	c := [1, 2, 3, 4, 5]

### garbage collector

    	a := [1, 2, 3]

## test task

Поддержка сборки мусора со стороны компилятора + Рантайм для сборки мусора
