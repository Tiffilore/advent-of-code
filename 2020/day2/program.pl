/* Day 2 

The Input is parsed into structures that are added to the database of this form:

	entry(policy(CharCode, Min, Max), PasswdCodeList)

*/


/* Test

Since instantiation can influence whether a program works, the solutions are first calculated, then checked for accuracy.

	?- solutions(input_expl, Solution1, Solution2), Solution1=2, Solution2=1
*/



/* solutions for Part 1 and Part 2

I am still unsure whether to keep the importing of input as facts into the database and the solution separate. 

*/

solutions(File, Solution1, Solution2) :-
	importfacts(File),
	solution(valid1, Solution1),
	solution(valid2, Solution2).
	


solution_part1(File, Solution) :- solution(File, valid1, Solution).

solution_part1(Solution) :- 	solution(valid1, Solution).


solution_part2(File, Solution) :- solution(File, valid2, Solution).

solution_part2(Solution) :- 	solution(valid2, Solution).



/*  solution dependent on definition for validity */

solution(File, ValidityPredicate, Solution) :-
	importfacts(File),
	solution(ValidityPredicate, Solution). 

solution(ValidityPredicate, Solution) :-
	findall(Passwd, (Term=..[ValidityPredicate, Passwd], call(Term)), Bag),
	length(Bag, Solution).


valid1(PasswdCodeList) :-
	entry(policy(CharCode, Min, Max), PasswdCodeList),
	findall(CharCode, member(CharCode, PasswdCodeList), Bag),
	length(Bag, Occurences),
	Occurences >= Min,
	Occurences =< Max.
	

valid2(PasswdCodeList) :-
	entry(policy(CharCode, Pos1, Pos2), PasswdCodeList),
	findall(X,
		(member(Pos, [Pos1, Pos2]), 
		nth1(Pos, PasswdCodeList, X)),
		Bag), 
	findall(_, member(CharCode, Bag), [_]).


/* Parsing the Input 

	entry(policy(CharCode, Min, Max), PasswdCodeList)

*/ 


:- use_module(library(dcg/basics)).
:- dynamic entry/2.

importfacts(File) :-
	retractall(entry(_,_)),
	open(File, read, Stream),
	phrase_from_stream(input, Stream),
	!.

input -->
    	entry(Entry),
   	 "\n",
	{assertz(Entry)},
    	input.

input --> eos.

entry(entry(Policy, PasswdCodeList)) -->
	whites,
	policy(Policy),
	":",
	whites,
	nonblanks(PasswdCodeList).

policy(policy(CharCode, Min, Max)) -->
	integer(Min),
	"-",
	integer(Max),
	whites,
	nonblank(CharCode).

	
