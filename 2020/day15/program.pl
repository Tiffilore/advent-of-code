/******************************************************** 

  --- Day 15: Rambunctious Recitation ---
  
********************************************************/



:- dynamic last_turn/2.
:- dynamic previous/1.

solution(List, Stop, Solution) :-
	retractall(last_turn(_,_)),
	retractall(previous(_)),
	assert_list(List),
	last_turn(Start,_),
	!,
	\+play_the_game(Start, Stop),
	previous(Solution),
	!.


assert_list(List) :- 
	assert_list(1, List),
	length(List, Len),
	last_turn(Len, Previous),
	assertz(previous(Previous)), 
	!.
	
assert_list(_,[]).
assert_list(Turn, [Head|Tail]):-
	asserta(last_turn(Turn, Head)),
	Succ is Turn +1, 
	assert_list(Succ, Tail).


play_the_game(Start, Stop) :-
	End is Stop -1,
	between(Start, End, N),
	play(N).


/* Previous was spoken before */
play(Last_Turn) :-
	previous(Previous), % do not retract here :-)
	retract(last_turn(Turn_Before, Previous)), % retract to save spave in the knowledge base
	!,
	retract(previous(Previous)), % retract to save spave in the knowledge base
	Spoken is Last_Turn - Turn_Before,
	assertz(last_turn(Last_Turn, Previous)),
	assertz(previous(Spoken)),
	!, fail.


/* Previous is new */
play(Last_Turn) :-
	retract(previous(Previous)), % retract to save spave in the knowledge base
	assertz(last_turn(Last_Turn, Previous)),
	assertz(previous(0)),
	!, fail.


/*******************************************************/

export_statistics(Stops, File_out):-
	open(File_out, write, Stream),
	List=[1,0,15,2,10,13], 
	member(Stop,Stops),
	statistics(walltime, [_,_]), 
	solution(List, Stop, Solution),
	findall(_, last_turn(_,_), Bag), 
	length(Bag, L),
	statistics(walltime, [_,ExecutionTime]), 
	write(Stream, Stop), write(Stream, ' '),
	write(Stream, Solution), write(Stream, ' '),
	write(Stream, L), write(Stream, ' '),
	write(Stream, ExecutionTime), write(Stream, '\n'),
	last(Stops, Stop),
	close(Stream).
