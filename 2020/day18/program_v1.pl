/******************************************************** 

	--- Day 18: Operation Order --- 
  
********************************************************/

solve(File, Solution):-
	import_expressions(File, Es),
	maplist(is, Vals, Es),
  sum_list(Vals, Solution),
	!.

:- use_module(library(dcg/basics)).
import_expressions(File, Es) :-
	open(File, read, Stream),
	phrase_from_stream(input(Es), Stream),
	close(Stream),
	!.

input(Es) --> lines(Es), input_rest.
input_rest --> "\n", input_rest.
input_rest --> eos.

lines([L|Ls]) --> line(L), line_rest(Ls).
line_rest(Ls) --> lines(Ls).
line_rest([]) --> [].

line(E) --> expr(E), "\n".


expr(E) --> atomic_expr(I), " ", operator(Op), " ", expr_rest(I, Op, E).
expr(I) --> atomic_expr(I).

%++: as before!
expr_rest(Left, +, E) --> 
	atomic_expr(Right),
	" ", 
	"+", 
	" ", 
	expr_rest(Left+Right, +, E).

%+*: as before!
expr_rest(Left, +, E) --> 
	atomic_expr(Right),
	" * ", 
	expr_rest(Left + Right, *, E).

%**: as before!
expr_rest(Left, *, E) --> 
	atomic_expr(Right),
	" ", 
	"*", 
	" ", 
	expr_rest(Left * Right, *, E).


%*+: DIFFERENT! + < *

expr_rest(Left, *, Left * E) --> 
	atomic_expr(Right),
	" ", 
	"+", 
	" ", 
	expr_rest(Right, +, E).

expr_rest(Left, Op_L, E ) --> atomic_expr(Right), {E=..[Op_L, Left, Right]}.


atomic_expr(I) -->  integer(I).
atomic_expr((E)) -->  "(", expr(E), ")".
operator(+) --> "+".
operator(*) --> "*"
