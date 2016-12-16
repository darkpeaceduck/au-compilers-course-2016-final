# simple-compiler

- [about](#about)
- [todo](#todo)
- [syntax](#syntax)
- [test-task](#test-task)

## about

Implementing a simple language.
What do we have so far:

                                  | Interepreter | Stack Machine | x86 |
--------------------------------- | ------------ | ------------- | --- |
binops                            | [x]          | [x]           | [x] |
if/while/for/repeat control flows | [x]          | [x]           | [x] |
funcs                             | [x]          | [x]           | [x] |
builtins                          | [x]          | [x]           | [x] |
strings                           | [x]          | [x]           | [x] |
arrays                            | [x]          | [x]           | [x] |

## todo

- [ ] переписать !! and && в x86 для ускорения
- [x] избавиться от второго аругмента в call на стадии fdefs (ne)
- [x] чистить стэк после вызова функции как процедуры в ST
- [x] HashMap вместо Map (нет, будут проседания при реаллокации)
- [x] сделать рефакторинг
- [x] перепройти все тесты
- [x] переделать функции в SM правильно? и с классами env
- [x] переделать функции в Int правильно? и с классами env
- [x] сохранять регистры после вызова функции
- [x] добавить регистры в рабочий стек по x86
- [ ] перейти на docker + vagrant, стоит написать нормальную команду в Vagrantfile (?)
- [x] эффективная конкатенация строк
- [ ] тесты в performance (они замеряют в x86)
- [ ] реализовать функции в PrettyPrinter.ml
- [ ] изменить как-нибудь язык в этом репозитории на гит-хабе на ocaml

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

## test task

Поддержка сборки мусора со стороны компилятора.