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
 
 The following two graphics are the result of 
```bash
STOPS="[10,100,1000,2020,10000,50000,100000,500000,1000000 ,2000000,3000000,4000000,5000000,6000000,7000000,8000000,9000000,10000000]"
swipl -s program -g "export_statistics($STOPS, data),halt." 1>&1 | sed 's/ERROR: halt\/1.*/True/g'
gnuplot plot_data

```
<p float="left">
  <img src="./memory" width="470" />
  <img src="./exec" width="470" /> 
</p>
