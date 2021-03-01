## --- Day 18: Operation Order ---

*implemented in [SWI-Prolog](https://www.swi-prolog.org/)*.

### Description of the puzzle:

[link!](https://adventofcode.com/2020/day/18
)


**This problem is fun! So I am trying to implement it in different ways.**

#### Version 0

This version makes use of Prolog's ability to define precedence and associativity of operators directly.
For both parts, the operators <code>+</code> and <code>*</code> are defined according to the respective rules.

The expression-strings of the input are taken as they are, and then converted to terms by means of the 
predicate <code>term_string/2</code>, which makes use of the parser of the Prolog system.
The terms are then simply evaluated by use of the predicate <code>is/2</code>.
