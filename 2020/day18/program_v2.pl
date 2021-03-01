/******************************************************** 

	--- Day 18: Operation Order --- 

implemented by use of precedence classes

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

lines([E|Ls]) --> expr(E), "\n", lines_rest(Ls).
lines_rest(Ls) --> lines(Ls).
lines_rest([]) --> [].


expr(E) --> atomic_expr(Left), expr_rest(Left, E).

expr_rest(Left, E) -->
	" ",
	operator(Prec, Op), 
	" ", 
	atomic_expr(Right),
	expr_rest2(Prec, Left, Op, Right, E).	
expr_rest(Left, Left) --> [].

% case Prec_L =< Prec
expr_rest2(Prec_L, Left, Op_L, Right, E) --> 
	" ",
	operator(Prec, Op), 
	{Prec_L =< Prec},
	" ", 
	atomic_expr(Right2),
	{LeftE =..[Op_L, Left, Right]},					
	expr_rest2(Prec_L, LeftE, Op, Right2, E),
	!.

% case Prec_L < Prec
expr_rest2(_, Left, Op_L,  Right, E) --> 
	" ",
	operator(Prec, Op), 
	" ", 
	atomic_expr(Right2),
	{E=..[Op_L, Left, E2]},
	expr_rest2(Prec, Right, Op, Right2, E2),
	!.

expr_rest2(_, Left, Op_L, Right, E ) --> [],{E=..[Op_L, Left, Right]}.

atomic_expr(I) -->  integer(I).
atomic_expr((E)) -->  "(", expr(E), ")".
operator(0,+) --> "+".
operator(1,*) --> "*".


