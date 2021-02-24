/* Day 5 

  ?- solution_part1(input, Solution).
  Solution = 828.

  ?- solution_part2(input, Solution).
  Solution = 565.

*/

solution_part1(File, Solution):-
	import_seatnumbers(File, Ns),
	max_list(Ns, Solution).

:- dynamic seat/1.

solution_part2(File, Solution):-
	import_seatnumbers(File, Ns),
	retractall(seat(_)),
	assert_seats(Ns),
	seat(Pred),
	Solution is Pred+1,
	\+ seat(Solution),
	Succ is Solution + 1,
	seat(Succ),
	!.
	

assert_seats([]).
assert_seats([Head|Tail]) :-
	assertz(seat(Head)),
	assert_seats(Tail).

/* read in strings as seat numbers */

:- use_module(library(dcg/basics)).

import_seatnumbers(File, Ns) :-
	open(File, read, Stream),
	phrase_from_stream(input(Ns), Stream),
	close(Stream),
	!.

input(Ns) 	--> seatnumbers(Ns), input_rest.
input_rest 	--> eos.
input_rest 	--> "\n", input_rest.

seatnumbers([]) --> [].
seatnumbers([R|Ns]) --> 
	seatnumber(0, R), 
	"\n", 
	seatnumbers(Ns).
seatnumbers([R]) --> 
	seatnumber(0, R).

/*	F,L --> 0 
	B,R --> 1	*/

seatnumber(Result, Result) --> [].
seatnumber(Acc, Result)	--> 
	("F";"L"), 
	{Acc2 is  Acc*2 }, 
	seatnumber(Acc2, Result).
seatnumber(Acc, Result)	--> 
	("B";"R"), 
	{Acc2 is  Acc*2+1 }, 
	seatnumber(Acc2, Result).
