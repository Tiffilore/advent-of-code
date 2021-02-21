/*  day 3 */


/* Tests:

?- solution_part1(input_expl, Solution), Solution = 7.	
?- solution_part2(input_expl, Solution), Solution = 336.	

For each participant, the input is different!

?- solution_part1(input, Solution), Solution = 280.	
?- solution_part2(input, Solution), Solution = 4355551200.
*/


/* Parsing the Input */ 

read_lines(File, Lines) :-
	open(File, read, Stream),
	read_string(Stream, _, Str), 
	split_string(Str, "\n", "\n", StringLines),
	maplist(atom_chars, StringLines, Lines),
	close(Stream).

/* Solving the Puzzle */ 

solution_part1(File, Solution) :-
	read_lines(File, Lines),
	count_trees_slope(Lines, 1, 3, Solution).

solution_part2(File, Solution) :-
	read_lines(File, Lines),
	count_trees_slope(Lines, 1, 1, N1),
	count_trees_slope(Lines, 1, 3, N2),
	count_trees_slope(Lines, 1, 5, N3),
	count_trees_slope(Lines, 1, 7, N4),
	count_trees_slope(Lines, 2, 1, N5),
	Solution is N1*N2*N3*N4*N5.


count_trees_slope(Lines, Down, Right, Treescount) :-
	count_trees_slope_acc(Lines, 0, Down, Right, 0, Treescount).	


count_trees_slope_acc([], _, _, _, N, N).

count_trees_slope_acc([Line|Tail], Linenumber, Down, Right, Acc, N) :-
	hitTreeInThisLine(Line, Linenumber, Down, Right),
	AccNew is Acc + 1,
	SuccLinenumber is Linenumber +1,
	count_trees_slope_acc(Tail, SuccLinenumber, Down, Right, AccNew, N),
	!.

count_trees_slope_acc([_|Tail], Linenumber, Down, Right, Acc, N) :-
	SuccLinenumber is Linenumber +1,
	count_trees_slope_acc(Tail, SuccLinenumber, Down, Right, Acc, N).


hitTreeInThisLine(Line, Linenumber, Down, Right) :-
	0 is Linenumber mod Down, /* if Linenumber does not divide by Down, this line is skipped */
	length(Line, Length),
	Index is (Right * (Linenumber / Down)) mod Length,
	nth0(Index, Line, '#'). 




