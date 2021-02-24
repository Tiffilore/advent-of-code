/*  Day6

	?- solution_part1(input, Solution).
	Solution = 6878.

	?- solution_part2(input, Solution).
	Solution = 3464.

*/


solution_part1(File, Solution) :- solution(count_any, File, Solution).

solution_part2(File, Solution) :- solution(count_every, File, Solution).

solution(Predicate, File, Solution) :-
	import_entries(File, ListEntries),
	maplist(maplist(list_to_set), ListEntries, SetEntries),
	maplist(Predicate, SetEntries, Counts),
	sumlist(Counts, Solution).

count_any(List, Len):-
	bigUnion(List,BigU),
	length(BigU, Len).

bigUnion([], []).
bigUnion([H|T], U) :-
	bigUnion(T, U_T),
	union(H, U_T, U).

count_every(List, Len):-
	bigIntersection(List,BigU),
	length(BigU, Len).

bigIntersection([E], E) :- !.
bigIntersection([], []).
bigIntersection([H|T], U) :-
	bigIntersection(T, U_T),
	intersection(H, U_T, U).

/* read in entries

	bug: doesn't work if last line of input is not ended by newline
*/

:- use_module(library(dcg/basics)).

import_entries(File, Ns) :-
	open(File, read, Stream),
	phrase_from_stream(input(Ns), Stream),
	close(Stream),
	!.


peek(Head, [Head|Tail], [Head|Tail]).
peek([], [], []).

input(Entries) 	--> entries(Entries), input_rest.

input_rest 	--> "\n", input_rest.
input_rest 	--> eos.

entries([E|R]) --> entry(E), entries_rest(R).

entries_rest(E) --> "\n", entries(E).
entries_rest([]) --> [].

entry([L|R]) --> line(L), entry_rest(R).
entry_rest([]) --> "\n", peek(10), !.
entry_rest(E) --> "\n", entry(E). 


line([L|R]) --> letter(L), line_rest(R), !.
line_rest([L|R]) --> letter(L), line_rest(R), !.
line_rest([]) --> [].

letter(A) --> [Code], {97 =< Code, Code =< 122, atom_chars(A, [Code])}. 


