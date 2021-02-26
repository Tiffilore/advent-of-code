/******************************************************** 

	--- Day 7: Handy Haversacks ---

********************************************************/


/******************************************************** 

	read rules in 

********************************************************/

:- use_module(library(dcg/basics)).

import_rules(File, Rules) :-
	open(File, read, Stream),
	phrase_from_stream(input(Rules), Stream),
	close(Stream),
	!.

/* input */
input(Rules) 	--> rules(Rules), input_rest.

input_rest 	--> "\n", input_rest. %allows for trailing empty lines
input_rest 	--> eos.

/* rules */
rules([Rule|Rules]) 
		--> rule(Rule), rules_rest(Rules).

rules_rest([Rule|Rules])	
		--> "\n", rule(Rule), rules_rest(Rules).
rules_rest([])	--> [].

/* rule */
rule(contains(Color, Content)) 	
		--> color_spec(Color), " bags contain ", content(Content).

/* content */
content([]) 		--> "no other bags.", !.
content([Bag|Bags])	--> bags(Bag), content_rest(Bags).

content_rest([])	--> ".".
content_rest(Bags)	--> ", ", content(Bags). 

/* bags */
bags(bags(N, Color))
		--> number(N), " ",  color_spec(Color), (" bag";" bags").

/* color_spec */ 
color_spec(col(Color_Qualifier, Color))
		-->     string_without(" ", Color_Qualifier_Codes), 
			" ", 
			string_without(" ", Color_Codes),
			{atom_codes(Color_Qualifier, Color_Qualifier_Codes), 
			atom_codes(Color,Color_Codes)}.

/********************************************************  

	check whether there is a vicious cycle 
	in the rules of a file:

	contains(Col0, [..., bags(_, Col0), ...]). 

	contains(Col1, [..., bags(_, Col2), ...]). 
	contains(Col2, [..., bags(_, Col1), ...]). 

********************************************************/


:- dynamic contains/2.

test(File) :- 
	import_rules(File, Rules),
	retractall(contains(_,_)),
	assert_rules(Rules),
	member(Rule, Rules),
	Rule = contains(Color, _),
	contains_itself(Color),
	write(Color), nl.

assert_rules([]).
assert_rules([H|T]) :-
	assertz(H),
	assert_rules(T).

contains_itself(X) :- contains_single_trans(X, X), !.	

contains_single_trans(X, Y) :-	
	contains(X, List),
	member(bags(_, Y), List).

contains_single_trans(X, Y) :-	
	contains(X, List),
	member(bags(_, Z), List),
	contains_single_trans(Z, Y).
			
/********************************************************  

	solve part 1

How many bag colors can eventually contain 
at least one shiny gold bag? 

********************************************************/

solution_part1(File, Solution) :- 
	import_rules(File, Rules),
	retractall(contains(_,_)),
	assert_rules(Rules),
	findall(Color, contains_single_trans(Color, col(shiny,gold)), Bag),
	list_to_set(Bag, Set),
	length(Set, Solution).

/********************************************************  

	solve part 2

How many individual bags are required inside 
your single shiny gold bag? 

********************************************************/

solution_part2(File, Solution) :- 
	import_rules(File, Rules),
	retractall(contains(_,_)),
	assert_rules(Rules),
	number_of_bags(col(shiny, gold), Total),
	Solution is Total -1, !.


number_of_bags(col(C1, C2), Sum):-
	number_of_bags([bags(1, col(C1, C2))], 0, Sum).
number_of_bags([], Sum, Sum).
number_of_bags([bags(N, Color)|Tail], Acc, Sum) :- 
	contains(Color, Content),
	number_of_bags(Content, 0, Sum_Content),
	Acc1 is Acc + (N* (1+ Sum_Content)),
	number_of_bags(Tail, Acc1, Sum).



