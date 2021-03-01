/******************************************************** 

	--- Day 18: Operation Order --- 

********************************************************/

solution(Version, File, Solution):-
	current_op(PP, TP, +),
	current_op(PM, TM, *),
	set_operators(Version),
	read_lines(File, StringLines),
	maplist(term_string, Terms, StringLines),
	maplist(is, Vals, Terms),
	sumlist(Vals, Solution),
	op(PP, TP, +),
	op(PM, TM, *),
	!.

read_lines(File, StringLines) :-
	open(File, read, Stream),
	read_string(Stream, _, Str), 
	split_string(Str, "\n", "\n", StringLines),
	close(Stream).

set_operators(1):- op(1, yfx, +), op(1, yfx, *).
set_operators(2):- op(1, yfx, +), op(2, yfx, *).
