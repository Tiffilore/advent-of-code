## --- Day 8: Handheld Halting ---

*implemented in [SWI-Prolog](https://www.swi-prolog.org/)*.

### Description of the puzzle:

[link!](https://adventofcode.com/2020/day/8
)

### Remarks on importing the data 

The data is first imported into a list of structures of the form

```
instruction(<Operation>, <Number>)
```
and then successively asserted as facts of the form

```
codeline(<Linenumber>, <Operation>, <Number>)
```

### Remarks on solutions

The solutions were implemented in such a way that as much code could be reused by the implementations for both parts.
The price is code that is less readable and less efficient than if both solutions were implemented separately.


### Example Questions


```prolog
?- solution_part1(input_expl, Solution).
Solution = 5.

?- solution_part2(input_expl, Solution).
Solution = 8.

?- solution_part1(input, Solution).
Solution = 1782.

?- solution_part2(input, Solution).
Solution = 797.

```
