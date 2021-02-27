/******************************************************** 

	--- Day 12: Rain Risk ---

********************************************************/


solution_part1(File, Solution):-
	import_instructions(File, Instructions),
	navigate(1, Instructions, state(0,0,e), state(NS,EW,_)),
	abs(NS, NS_abs), 
	abs(EW, EW_abs), 
	Solution is NS_abs + EW_abs,
	!.

solution_part2(File, Solution):- 
	import_instructions(File, Instructions),
	navigate(2, Instructions, 
		state(position(0,0),waypoint(1,10)), 
		state(position(NS, EW), _)),
	abs(NS, NS_abs),
	abs(EW, EW_abs),
	Solution is NS_abs + EW_abs,
	!.





/*------------------------------------------------------
	navigate(+Policy, +Instructions, +CurrentState, -Endstate)

Policy is a navigation policy corresponding to the puzzle part, either 1 or 2.

Instruction is a list of instructions, i.e. of structures 
i(Action, Value).

CurrentState is a structure representing the current state. This structure differs 
according to policy.

Endstate is also a structure representing a state. It represents the state
of the ship after the instructions are executed.
------------------------------------------------------*/

navigate(_, [],  EndState, EndState).

navigate(Policy, [Head|Tail],  State, EndState) :-
	nextstate(Policy, Head, State, State2),
	navigate(Policy, Tail, State2, EndState).


/*------------------------------------------------------
	nextstate(+Policy, +Instruction, +State, -NextState)

NextState is the state that results from following the instruction 
Instruction according to policy Policy, currently being in state State.

In policy 1, states have the form state(NS, EW, Face),
In policy 2, states have the form state(position(NS_p,EW_p), waypoint(NS_w, EW_w)).
------------------------------------------------------*/
:- discontiguous nextstate/4.

/* Policy for part 1 */

nextstate(1, i(r, Number), state(NS, EW, Face), state(NS, EW,Face2)) :-
	face(r, Face, Number, Face2).
nextstate(1, i(l, Number), state(NS, EW, Face), state(NS, EW,Face2)) :-
	face(l, Face, Number, Face2).
nextstate(1, i(f, Number), state(NS, EW, Face), State2) :-
	nextstate(1, i(Face, Number), state(NS, EW, Face), State2).
nextstate(1, i(n, Number), state(NS, EW, Face), state(NS2, EW,Face)) :-
	NS2 is NS + Number.
nextstate(1, i(s, Number), state(NS, EW, Face), state(NS2, EW,Face)) :-
	NS2 is NS - Number.
nextstate(1, i(e, Number), state(NS, EW, Face), state(NS, EW2,Face)) :-
	EW2 is EW + Number.
nextstate(1, i(w, Number), state(NS, EW, Face), state(NS, EW2,Face)) :-
	EW2 is EW - Number.

face(r, Face, N, Face2) :- map(Face, X), Y is (X+N) mod 360, map(Face2, Y), !.
face(l, Face, N, Face2) :- map(Face, X), Y is (X-N) mod 360, map(Face2, Y), !.

map(n, 0).
map(e, 90).
map(s, 180).
map(w, 270).

/* Policy for part 2 */

% f: changes position
nextstate(2, i(f, Number), 
	state(position(NS_p,EW_p), waypoint(NS_w, EW_w)), 	
	state(position(NS_p2,EW_p2), waypoint(NS_w, EW_w)) ) :-
	NS_p2 is  Number*NS_w + NS_p,
	EW_p2 is  Number*EW_w + EW_p.

% n: changes waypoint 
nextstate(2, i(n, Number), 
	state(position(NS_p,EW_p), waypoint(NS_w, EW_w)), 
	state(position(NS_p,EW_p), waypoint(NS_w2, EW_w)) ) :-
	NS_w2 is  NS_w +Number.

% s: changes waypoint 
nextstate(2, i(s, Number), 
	state(position(NS_p,EW_p), waypoint(NS_w, EW_w)), 
	state(position(NS_p,EW_p), waypoint(NS_w2, EW_w)) ) :-
	NS_w2 is  NS_w -Number.

% e: changes waypoint 
nextstate(2, i(e, Number), 
	state(position(NS_p,EW_p), waypoint(NS_w, EW_w)), 
	state(position(NS_p,EW_p), waypoint(NS_w, EW_w2)) ) :-
	EW_w2 is  EW_w +Number.

% w: changes waypoint 
nextstate(2, i(w, Number), 
	state(position(NS_p,EW_p), waypoint(NS_w, EW_w)), 
	state(position(NS_p,EW_p), waypoint(NS_w, EW_w2)) ) :-
	EW_w2 is  EW_w -Number.

% r: changes waypoint
nextstate(2, i(r, 90), 
	state(position(NS_p,EW_p), waypoint(NS_w, EW_w)), 
	state(position(NS_p,EW_p), waypoint(NS_w2, EW_w2)) ) :-
	EW_w2 is NS_w,
	NS_w2 is -EW_w.
nextstate(2, i(r, 180), 
	state(position(NS_p,EW_p), waypoint(NS_w, EW_w)), 
	state(position(NS_p,EW_p), waypoint(NS_w2, EW_w2)) ) :-
	EW_w2 is -EW_w,
	NS_w2 is -NS_w.
nextstate(2, i(r, 270), 
	state(position(NS_p,EW_p), waypoint(NS_w, EW_w)), 
	state(position(NS_p,EW_p), waypoint(NS_w2, EW_w2)) ) :-
	EW_w2 is -NS_w,
	NS_w2 is EW_w.

% l: changes waypoint 
nextstate(2, i(l, Number), State, State2) :-
	Number2 is 360-Number,
	nextstate(2, i(r, Number2), State, State2).


/*------------------------------------------------------
	manhattan_distance/2
------------------------------------------------------*/



/*------------------------------------------------------
	import_instructions(+File, -Instructions)

Each instruction is read into a structure of the form
	i(Action, Value).
------------------------------------------------------*/

:- use_module(library(dcg/basics)).


import_instructions(File, Instructions) :-
	open(File, read, Stream),
	phrase_from_stream(input(Instructions), Stream),
	close(Stream),
	!.


input(Instructions) --> instructions(Instructions), input_rest.
input_rest --> "\n", input_rest.
input_rest --> eos.

instructions([i(A,I)|Is]) --> action(A), integer(I), "\n", instructions(Is).
instructions([]) --> [].

action(n)	--> "N".
action(s)	--> "S".
action(e)	--> "E".
action(w)	--> "W".
action(f)	--> "F".
action(l)	--> "L".
action(r)	--> "R".



