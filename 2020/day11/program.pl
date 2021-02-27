/******************************************************** 

	--- Day 11: Seating System ---

********************************************************/

solution_part1(File, Solution) :-
	solution(1, File, Solution).

solution_part2(File, Solution) :-
	solution(2, File, Solution).


/*	solution(+Policy, +File, -Solution) 	

Policy is a code for the set of rules that are applied. 
There are two policies implemented: 
	* 1	for the puzzle's first part
	* 2	for the puzzle's second part
*/

% precondition: all Lines have equal length
solution(Policy, File, Solution) :-
	parse_input(File, Plan),
	test_equal_length(Plan, Columns),
	length(Plan, Rows),
	stabilized(Policy, Plan, Rows, Columns, StabilizedPlan),
	flatten(StabilizedPlan, FlatPlan),
	findall(_, member('#', FlatPlan), Bag),
	length(Bag, Solution), !.


/*	parse_input(+File, -Plan) 	*/

parse_input(File, Plan) :-
	open(File, read, Stream),
	read_string(Stream, _, Str), split_string(Str, "\n", "\n", Lines),
	close(Stream),
	maplist(string_chars, Lines, Plan),
	!.


/*	test_equal_length(+List, -Len).

List is a List consisting of lists.
If all lists have the same length and List is not empty, Len is the length of all 
contained lists. */

test_equal_length([],_).
test_equal_length([H|T], Len) :-
	length(H, Len),
	test_equal_length(T, Len).





/*	stabilized(+Policy, +Rows, +Plan, +Columns, -StabilizedPlan)	*/

stabilized(Policy, Plan, Rows, Columns, StabilizedPlan) :-
	%print_plan(Plan),nl,	
	next_plan(Policy, Plan, Rows, Columns, NextPlan),
	(	
		(	Plan=NextPlan, 
			StabilizedPlan=Plan,
			!
		)
		; 
		stabilized(Policy, NextPlan, Rows, Columns, StabilizedPlan)
	).


/*	next_plan(+Policy, +Plan, +Rows, +Columns, -NextPlan)	next_plan/5	*/

next_plan(Policy, Plan, Rows, Columns, NextPlan) :-
	empty_plan(Rows, Columns, NextPlan),
	next_plan(Policy, Plan, Rows, Columns, 1, 1, NextPlan).

/* 	next_plan(+Policy, +Plan, +Rows, +Columns, +X, +Y, -;NextPlan) 	next_plan/7 	*/

next_plan(_, _, Rows, _, X, _, _) :-
	Rows < X, !.
	
next_plan(Policy, Plan, Rows, Columns, X, Y, NextPlan) :-
	% decide new Seat for (X,Y)
	element1(X,Y,Plan, E),
	next_element(Policy, Plan, Rows, Columns, X, Y, E, NextE),
	element1(X,Y,NextPlan, NextE),
	next_indices(X,Y, Columns, X1, Y1),
	next_plan(Policy, Plan, Rows, Columns, X1, Y1, NextPlan),
	!.

/*  	next_element/8 */ 

next_element(1, Plan, Rows, Columns, X, Y, '#', 'L') :-
	findall(_, sees(1,Plan, Rows, Columns, X, Y, '#'), Bag),
	length(Bag, Len),
	Len >=4,
	!.

next_element(2, Plan, Rows, Columns, X, Y, '#', 'L') :-
	findall(_, sees(2, Plan, Rows, Columns, X, Y, '#'), Bag),
	length(Bag, Len),
	Len >=5,
	!.

next_element(Policy, Plan, Rows, Columns, X, Y, 'L', '#') :-
	findall(_, sees(Policy,Plan, Rows, Columns, X, Y, '#'), []),
	!.
	
next_element(_, _, _, _, _, _, E, E).	% catch-all 

/* 	sees/7	*/

sees(1, Plan, Rows, Columns, X, Y, E) :-
	adjacent(_, Rows, Columns, X, Y, X1, Y1), 
	element1(X1,Y1,Plan, E).

sees(2, Plan, Rows, Columns, X, Y, E) :-
	member(Direction, [lb,b,rb,l,r,la,a,ra]),
	sees_dir(Direction, Plan, Rows, Columns, X, Y, E).

/* 	sees_dir/7	*/
sees_dir(Direction, Plan, Rows, Columns, X, Y, E) :-
	adjacent(Direction,Rows, Columns, X, Y, X1, Y1), 
	element1(X1,Y1,Plan, '.'),
	sees_dir(Direction, Plan, Rows, Columns, X1, Y1, E), !.

sees_dir(Direction, Plan, Rows, Columns, X, Y, E) :-
	adjacent(Direction,Rows, Columns, X, Y, X1, Y1), 
	element1(X1,Y1,Plan, E).
	%write([Direction, X1, Y1, E]), nl.

/* 	adjacent/7	*/
% left-before
adjacent(lb, _, _, X, Y, X1, Y1) :-
 	X>1,		%before
	Y>1,		%left
	X1 is X-1,
	Y1 is Y-1.

% before
adjacent(b, _, _, X, Y, X1, Y) :-
 	X>1,		%before
	X1 is X-1.

% right-before
adjacent(rb, _, Columns, X, Y, X1, Y1) :-
 	X>1,		%before
	Y<Columns,	%right
	X1 is X-1,
	Y1 is Y+1.

% left
adjacent(l, _,_, X, Y, X, Y1) :-
	Y>1,		%left
	Y1 is Y-1.

% right
adjacent(r, _, Columns, X, Y, X, Y1) :-
	Y<Columns,	%right
	Y1 is Y+1.

% left-after
adjacent(la, Rows, _, X, Y, X1, Y1) :-
	Y>1,		%left
	X<Rows,		%after
	Y1 is Y-1,
	X1 is X+1.
% after
adjacent(a, Rows, _, X, Y, X1, Y) :-
	X<Rows,		%after
	X1 is X+1.

% right-after
adjacent(ra, Rows, Columns, X, Y, X1, Y1) :-
	Y<Columns,	%right
	X<Rows,		%after
	Y1 is Y+1,
	X1 is X+1.


/* 	element1/4	*/
element1(X,Y,Plan, Element):-
	nth1(X, Plan, Row),
	nth1(Y,Row, Element).

next_indices(X,Y, Y, X1, 1) :-
	X1 is X+1, !.

next_indices(X,Y, _, X, Y1) :-
	Y1 is Y+1, !.

/* 	empty_plan/3	*/
empty_plan(0,_,[]) :-	!.
empty_plan(Rows, Columns, [Row|EmptyPlan]):-
	length(Row, Columns),
	Rows2 is Rows - 1,
	empty_plan(Rows2, Columns, EmptyPlan).
	


/* 	print_plan/1 
for debugging purposes
*/

print_plan([]).
print_plan([Row|Rows]):-
	%write(Row), nl,
	print_row(Row), nl,	
	print_plan(Rows).

/* 	print_row/1 	*/
print_row([]).
print_row([H|T]):-
	write(H),
	print_row(T).

