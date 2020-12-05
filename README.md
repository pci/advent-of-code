# Advent of code 2020

Giving this a go in haskell, let's see how it goes!

## Code layout

Most of the interesting code is in each day's `src/Lib.hs`. I'm using [stack](https://docs.haskellstack.org/en/stable/README/), and you can run most days by cd-ing into the day's folder and running

```bash
stack build && cat input.txt | stack exec dayX-exe
```

There might be a better way, but I'm pretty new to the haskell ecosystem and still learning.
