## --- Day 15: Rambunctious Recitation ---


*implemented in [SWI-Prolog](https://www.swi-prolog.org/)*.


### Description of the puzzle:

[link!](https://adventofcode.com/2020/day/15
)

### Example Questions

```prolog
?- solution([0,3,1,6,7,5], 2020, Solution).
Solution = 852.

?- solution([0,3,1,6,7,5], 30000000, Solution).
Solution = 6007666.
```

### Comments and Extension

I solved this puzzle without recursion to keep the stack minimal. With simple recursion, solving the second part exceeded the stack's capacity.
I did not memorize all turns and corresponding spoken numbers, but for each spoken number only the last turn it was spoken.
Still, the list of memorized spoken numbers is large.

To look at the growth in memory and thus execution time, I wrote a predicate to gather statistic data: `export_statistics(Stops, File_out)`. 
`Stops` is a list of numbers. For each number in this list, it proves the predicate `solution/3`  with the my puzzle input 
`[0,3,1,6,7,5]` as start sequence. It then writes the number, the  solution, the number of memorized spoken words and the execution time into the file named
`File_out`.
 


```prolog


```

