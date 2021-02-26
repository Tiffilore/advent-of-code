## --- Day 9: Encoding Error ---

*implemented in [SWI-Prolog](https://www.swi-prolog.org/)*.

### Description of the puzzle:

[link!](https://adventofcode.com/2020/day/9
)

### Remark on Implementation

The implementation for the second part is very short and pretty declarative.
Its search space are *all* sublists of the list of numbers.

This search space could be considerably restricted if we do not consider empty sublists
and sublists that have a prefix with a sum greater than the invalid number found in part 1.

### Example Questions

This time, the result of part2 depends on the result of part1. 
Thus, it has been implemented together:

```prolog
?- solutions(input, Solution1, Solution2).
Solution1 = 29221323,
Solution2 = 4389369.
```


