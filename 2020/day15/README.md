## --- Day 15: Rambunctious Recitation ---


*implemented in [SWI-Prolog](https://www.swi-prolog.org/)*.


### Description of the puzzle:

[link!](https://adventofcode.com/2020/day/15
)

### Example Questions

```prolog


```

### Comments and Extension

I solved this puzzle without recursion to keep the stack minimal. With simple recursion, the second part exceeded the stack's capacity.
I did not memorize all turns and corresponding spoken numbers, but for each spoken number only the last turn it was spoken.
Still, the list of memorized spoken numbers is large.
To illustrate the growth in memory and thus execution time, I wrote a program to gather statistic data about 

predicate: 


```prolog


```

