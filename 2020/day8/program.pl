/******************************************************** 

	--- Day 8: Handheld Halting --- 

********************************************************/


:- dynamic codeline/3.
:- dynamic visited/1.

solution_part1(File, Solution) :-
	import_code(File, Code),
	retractall(codeline(_, _,_)),
	assert_code(0, Code),
	retractall(visited(_)),
	execute(1, 0,0, Solution),
	!.

solution_part2(File, Solution) :-
	import_code(File, Code),
	retractall(codeline(_, _,_)),
	assert_code(0, Code),
	retractall(changed(_,_,_)),!,
	generate_try,
	retractall(visited(_)),
	execute(2,0,0, Solution),
	!.



:- dynamic changed/3.


change(jmp, nop).
change(nop, jmp).


repair_change_before :- 
	retract(changed(N,Op, Argument)),
	retract(codeline(N,_, _)),
	assertz(codeline(N,Op, Argument)),
	!.

repair_change_before.


generate_try :-
	codeline(N,Op, Arg),
	repair_change_before,
	change(Op, Op2),
	retract(codeline(N,Op, Arg)),
	assertz(changed(N,Op, Arg)),	
	assertz(codeline(N,Op2, Arg)).	



execute(1, Line, Accu, Accu) :-
	visited(Line), 
	!.


execute(2, Line, Accu, Accu) :-
	visited(Line), !,
	fail.
	
execute(P, Line, Accu, Solution) :-
	codeline(Line, nop, _),
	assertz(visited(Line)), 
	Line2 is Line + 1,
	execute(P, Line2, Accu, Solution), 
	!.

execute(P, Line, Accu, Solution) :-
	codeline(Line, acc, Arg),
	assertz(visited(Line)), 
	Line2 is Line + 1,
	Accu2 is Accu + Arg,
	execute(P, Line2, Accu2, Solution), 
	!.

execute(P, Line, Accu, Solution) :-
	codeline(Line, jmp, Arg),
	assertz(visited(Line)), 
	Line2 is Line + Arg,
	execute(P, Line2, Accu, Solution), 
	!.

execute(2, Line, Accu, Accu) :-
	\+codeline(Line, _, _),
	!.


/******************************************************** 

	add lines of code to the database

********************************************************/

assert_code(_,[]).
assert_code(N, [instruction(Op, Arg)|Tail]) :- 
	assertz(codeline(N, Op, Arg)),
	Succ is N+1,
	assert_code(Succ, Tail).

/******************************************************** 

	read code in 

********************************************************/

:- use_module(library(dcg/basics)).

import_code(File, Code) :-
	open(File, read, Stream),
	phrase_from_stream(input(Code), Stream),
	close(Stream),
	!.


/* input */
input(Instructions) 	--> instructions(Instructions), input_rest.

input_rest 		--> "\n", input_rest.
input_rest 		--> eos.

/* instructions */
instructions([I|Is]) 	--> instruction(I), instructions_rest(Is).

instructions_rest([I|Is])	--> "\n", instruction(I), instructions_rest(Is).
instructions_rest([])		--> [].

/* instruction */
instruction(instruction(Op, Argument))  --> 
	operation(Op), 
	" ", 
	argument(Argument).


/* operation */
operation(acc) --> "acc", !.
operation(jmp) --> "jmp", !.
operation(nop) --> "nop", !.

/* argument */
argument(I) --> integer(I).

