/******************************************************** 

	--- Day 10: Adapter Array ---

********************************************************/


solution_part1(File, Solution) :-
	import_numbers(File, Numbers),
	sort(Numbers, Sorted),
	distances([0|Sorted], Distances),
	findall(1, member(1, Distances), Ones),
	findall(3, member(3, Distances), Threes),
	length(Ones, C_Ones),
	length(Threes, C_Threes),
	Solution is C_Ones * (C_Threes +1),  % '+1' for the built-in joltage adapter 
	%length(Numbers, L), Others is L-C_Ones-C_Threes, write(Others), nl, 
		% to check whether there are other distances
	!.

/* precondition: the distances between the ordered list of numbers read in from File are 
either 1 or 3. */ 
solution_part2(File, Solution) :-
	import_numbers(File, Numbers),
	sort(Numbers, Sorted),
	distances([0|Sorted], Distances), 	% the built-in joltage adapter is left out, 
						% since it adds not possibility
	contains_sequences(Distances, Sequences),
	maplist(tribonacci, Sequences, Tribs),
	list_product(Tribs, Solution),
	!.

/* 
distances(+List, -Distances)
List is a list of numbers, Distances is the sequences of distances between these numbers.  
*/

distances([_], []).
distances([E1, E2|Tail], [D|Distances]):-
	D is E2-E1,
	distances([E2|Tail], Distances), 
	!.


/*
contains_sequences(+List, -Sequences)
List is a sequence of numbers, where each number is either 1 or 3.
Sequences is the list of the lengths of 1-sequences within List.
*/
contains_sequences([], []).
contains_sequences([3|Tail], Sequences) :-
	contains_sequences(Tail, Sequences).

contains_sequences([1|Tail], [Len| Sequences]) :-
	length_prefix_Ones(Tail, 1, Rest, Len),
	contains_sequences(Rest, Sequences).
 
length_prefix_Ones([1|T], Accu, Rest, Len) :-
	Accu2 is Accu+1,
	length_prefix_Ones(T, Accu2, Rest, Len),
	!.
length_prefix_Ones(L, Accu, L, Accu). % L is either empty or starts not with a 1


/*   
tribonacci(+N,-Tribonacci)
N is a nnumber, Tribonacci the Nth tribonacci number.
For efficiency, every calculated tribonacci number is asserted in the database.
*/
:- dynamic trib/2.

tribonacci(0,1):- !.
tribonacci(1,1):- !.
tribonacci(2,2):- !.
tribonacci(N,X):- trib(N,X).
tribonacci(N,X):-
	N1 is N-1,
	N2 is N-2,
	N3 is N-3,
	tribonacci(N1,X1),
	tribonacci(N2,X2),
	tribonacci(N3,X3),
	X is X1+X2+X3, 
	assertz(trib(N,X)),!.


/* 
list_product(+List, -Product)
List is a list of numbers, Product is its "big" product.  
*/
list_product([],1).
list_product([H|T],N) :-
	list_product(T, N1),
	N is H*N1, !.









/******************************************************** 

	read code in 

********************************************************/


:- use_module(library(dcg/basics)).


import_numbers(File, Numbers) :-
	open(File, read, Stream),
	phrase_from_stream(input(Numbers), Stream),
	close(Stream),
	!.
input(Is) --> integers(Is), input_rest.
input_rest --> "\n", input_rest.
input_rest --> eos.

integers([I|Is]) -->
    	integer(I),
   	 "\n", 
    	integers(Is).
integers([]) --> [].





