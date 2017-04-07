Will Fancher recently wrote a [blog post](http://elvishjerricco.github.io/2017/03/23/applicative-sorting.html)
(see also the [Reddit thread](https://www.reddit.com/r/haskell/comments/610sa1/applicative_sorting/))
about sorting arbitrary `Traversable` containers without any of the ugly incomplete pattern matches that
accompany the well-known technique of dumping all the entries into a list and then sucking them back out
in `State`. Fancher used a custom applicative based on the usual
[free applicative](https://hackage.haskell.org/package/free-4.12.4/docs/Control-Applicative-Free.html).
Unfortunately, this type is rather hard to work with, and Fancher was not immediately able to find a
way to use anything better than insertion sort. This repository demonstrates an asymptotically optimal heap
sort using a heap-merging applicative.

The three modules:

* `BasicNat`: unary natural numbers, singletons, and properties
* `IndexedPairingHeap`: size-indexed pairing heaps
* `HSTrav`: the big payoff: heap-sorting anything
