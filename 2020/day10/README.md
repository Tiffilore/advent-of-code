## --- Day 10: Adapter Array ---

*implemented in [SWI-Prolog](https://www.swi-prolog.org/)*.


### Description of the puzzle:

[link!](https://adventofcode.com/2020/day/10
)

### On Solution Finding

for the second part, my first quick solution was to just define a valid subsequence of the sequence of ordered input numbers, enumerate and then count
theses valid subsequences. Worked well with smaller examples, however, my stack got exhausted facing the input.

I then realised that the problem can be divided by dividing the the full cain of adapters into sublists where the distances are all 1 but the last. 

For the first example given in the puzzle description, where the full cain of adapters is <code>[0,1,4,5,6,7,10,11,12,15,16,19,23]</code>, those sublists are
<code>[0,1,4], [4,5,6,7,10], [10,11,12,15], [15,16,19], [19,23]</code>.
For a full chain divided into such sublists <code>L1, L2, ..., Ln</code>, the number of valid subsequences of this chain is simply 
<code>p(L1) * p(L2) * ... * p(Ln)</code>, where <code>p(X)</code> denotes the number of valid subsequences of the chain X.

Since all distances in full chains are either 1 or 3, the number of valid subsequences of a full chain in which all distances are all 1 but the last solely depends 
on the length of this chain. Bot how? I calculated it for a sequence of number, but couldn't find the function. 
Putting the result sequence <code>[1,1,2,4,7,13,24,44,81,149,..]</code> into a search engine gave my that it is a sequence of Tribonacci numbers.
I don't know why it is the Tribonacci numbers, but still use this function.


