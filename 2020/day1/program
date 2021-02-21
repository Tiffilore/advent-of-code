/* -----------------	Day 1	-----------------

For this puzzle, the input is not parsed into a data structure, but imported as facts into the knowledge base.
For each number N on a line in the input file, the following fact is asserted:

	inputnumber(N).


-----------------	Questions to obtain Solutions 	-------------------

** to get the solution for the first part:
	?- importfacts(<name of the input file>), solution_part1(Solution).

** to get the solution for the second part:
	?- importfacts(<name of the input file>), solution_part2(Solution).

** to get the solution for the both parts:
	?- importfacts(<name of the input file>), solution_part1(Solution1), solution_part2(Solution2).

*/



/* IO - read input and add suitable facts to the knowledge base */


:- use_module(library(dcg/basics)).
:- dynamic inputnumber/1.

importfacts(File) :-
	retractall(inputnumber(_)),
	open(File, read, Stream),
	phrase_from_stream(input, Stream),
	!.

input -->
    	integer(X),
   	 "\n",
	{assertz(inputnumber(X))},
    	input.

input --> eos.



/* solutions for part 1 and 2 */

solution_part1(Solution)	:-	
	inputnumber(X), 
	inputnumber(Y), 
	2020 is X + Y, 
	Solution is X * Y, 
	!.

solution_part2(Solution)	:-	
	inputnumber(X), 
	inputnumber(Y), 
	inputnumber(Z),
	2020 is X + Y + Z, 
	Solution is X * Y * Z, 
	!.




