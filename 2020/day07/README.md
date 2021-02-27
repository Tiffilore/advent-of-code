## --- Day 7: Handy Haversacks ---

*implemented in [SWI-Prolog](https://www.swi-prolog.org/)*.

### Description of the puzzle:

[link!](https://adventofcode.com/2020/day/7
)

### Remarks on Parsing

The goal in the construction of the grammar is to avoid as much (longer) backtracking as possible.
That makes it longer and possibly harder to read.


The **datastructure** the input file is read in shall be illustrated by a short example input

```
light red bags contain 1 bright white bag, 2 muted yellow bags.
bright white bags contain 1 shiny gold bag.
dotted black bags contain no other bags.
```

```prolog
?- import_rules(<input file>, Rules).
Rules = [
  contains(col(light, red), [bags(1, col(bright, white)), bags(2, col(muted, yellow))]), 
  contains(col(bright, white), [bags(1, col(shiny, gold))]), 
  contains(col(dotted, black), [])
].
```

### Extra Check

In both parts of the puzzle, we are interested in transitive relations. 
If the rules contain direct or indirect cycles, there is the threat of getting into an endless loop.

#### Example1
```
	contains(Col0, [..., bags(_, Col0), ...]). 
```

#### Example2
```
	contains(Col1, [..., bags(_, Col2), ...]). 
	contains(Col2, [..., bags(_, Col1), ...]). 
```

### Example Questions


```prolog
?- solution_part1(input_expl, Solution).
Solution = 4.

?- solution_part2(input_expl, Solution).
Solution = 32.

?- solution_part1(input, Solution).
Solution = 296.

?- solution_part2(input, Solution).
Solution = 9339.
```
