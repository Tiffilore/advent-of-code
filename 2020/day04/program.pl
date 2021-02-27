/* Day 4 */


:- use_module(library(dcg/basics)).

:- dynamic policy/1.


solution_part1(File, Solution) :- solution(1, File, Solution). /* input_expl: 2, input: 200 */

solution_part2(File, Solution) :- solution(2, File, Solution). /* input_expl: 2, input: 116 */

solution(Policy, File, Solution) :-
	retractall(policy(_)),
	asserta(policy(Policy)),
	import_valid_entries(File, Entries),
	length(Entries, Solution).



import_valid_entries(File, Entries) :-
	open(File, read, Stream),
	phrase_from_stream(input(Entries), Stream),
	close(Stream),
	!.

input(Entries) --> entries(Entries), input_rest(_).

input_rest([]) --> eos.
input_rest([]) --> "\n", input_rest(_).

entries(Entries) --> entry(Entry), entries_rest(Ls), {add_valid_entry(Entry, Ls, Entries)}.
entries_rest(Ls) --> "\n\n", entries(Ls).
entries_rest([]) --> [].


/*
byr (Birth Year)
iyr (Issue Year)
eyr (Expiration Year)
hgt (Height)
hcl (Hair Color)
ecl (Eye Color)
pid (Passport ID)
*/

add_valid_entry(Entry, Ls, [Entry|Ls]):-
	Entry=[BYR, IYR, EYR, HGT, HCL, ECL, PID],
	nonvar(BYR),
	nonvar(IYR),
	nonvar(EYR),
	nonvar(HGT),
	nonvar(HCL),
	nonvar(ECL),
	nonvar(PID), !.




add_valid_entry(_, Ls, Ls).




	
entry(Components) --> components(Components).

components(C) --> {policy(P)}, component(P,C), components_rest(Cs), {C=Cs}.

components_rest([_, _, _, _, _, _ ,_]) --> [].
components_rest(Cs) --> " ",  components(Cs).
components_rest(Cs) --> "\n",  components(Cs).


/* ---------------- byr (Birth Year) ---------------- */
component(1, [BYR, _, _, _, _, _ ,_]) --> 
	"byr:",
	nonblanks(BYR).

/* byr - four digits; at least 1920 and at most 2002. */
component(2, [BYR, _, _, _, _, _ ,_]) --> 
	"byr:",
	integer(BYR),
	{1920 =< BYR, BYR =< 2002}.

/* ---------------- iyr (Issue Year) ---------------- */
component(1, [_, IYR, _, _, _, _ ,_]) --> 
	"iyr:",
	nonblanks(IYR).

/* iyr - four digits; at least 2010 and at most 2020. */
component(2, [_, IYR, _, _, _, _ ,_]) --> 
	"iyr:",
	integer(IYR),
	{2010 =< IYR, IYR =< 2020}.

/* ---------------- eyr (Expiration Year) ---------------- */
component(1, [_, _, EYR, _, _, _ ,_]) --> 
	"eyr:",
	nonblanks(EYR).

/* eyr - four digits; at least 2020 and at most 2030. */
component(2, [_, _, EYR, _, _, _ ,_]) --> 
	"eyr:",
	integer(EYR),
	{2020 =< EYR, EYR =< 2030}.

/* ---------------- hgt (Height) ---------------- */
component(1, [_, _, _, HGT, _, _ ,_]) --> 
	"hgt:",
	nonblanks(HGT).


/* hgt - a number followed by either cm or in:
	If cm, the number must be at least 150 and at most 193.
	If in, the number must be at least 59 and at most 76.
*/
component(2, [_, _, _, (I,Unit), _, _ ,_]) --> 
	"hgt:",
	integer(I),
	nonblanks(UnitCodes),
	{atom_string(Unit,UnitCodes),
	(	(Unit=cm, 150 =< I, I =<193);
		(Unit=in, 59 =< I, I =< 76)	)	
	}.


/* ---------------- hcl (Hair Color) ---------------- */
component(1, [_, _, _, _, HCL, _ ,_]) --> 
	"hcl:",
	nonblanks(HCL).

/* hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f. */
component(2, [_, _, _, _, HCL, _ ,_]) --> 
	"hcl:",
	nonblanks(HCLCodes),
	{valid_hcl(HCLCodes), atom_codes(HCL, HCLCodes)}.

/* ---------------- ecl (Eye Color) ---------------- */
component(1, [_, _, _, _, _, ECL ,_]) --> 
	"ecl:",
	nonblanks(ECL).

/* ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth. */
component(2, [_, _, _, _, _, ECL ,_]) --> 
	"ecl:",
	nonblanks(ECLCodes),
	{atom_codes(ECL, ECLCodes), member(ECL, [amb, blu, brn, gry, grn, hzl, oth])}.

/* ---------------- pid (Passport ID) ---------------- */
component(1, [_, _, _, _, _, _ ,PID]) --> 
	"pid:",
	nonblanks(PID).

/* pid (Passport ID) - a nine-digit number, including leading zeroes. */
component(2, [_, _, _, _, _, _ ,PID]) --> 
	"pid:",
	nonblanks(PIDCodes),
	{length(PIDCodes, 9), number_string(_, PIDCodes), atom_string(PID,PIDCodes)}.


/* ---------------- catch-all: for invalid fields/values ---------------- */
component(_,_) --> 
	string(_),
	":", !,
	nonblanks(_).


valid_hcl([35|Tail]) :-
	length(Tail, 6),
	valid_members_tail_hcl(Tail).

valid_members_tail_hcl([]).	
valid_members_tail_hcl([Head|Tail]):-
	(between(97, 122, Head); between(48, 57, Head)),
	valid_members_tail_hcl(Tail), 
	!.

