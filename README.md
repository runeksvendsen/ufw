# Haskell weighted quick union find

## [Implementation 1](https://github.com/runeksvendsen/ufw/tree/state)

Naïve translation of [WeightedQuickUnionUF.java](https://algs4.cs.princeton.edu/15uf/WeightedQuickUnionUF.java.html) into Haskell.

Uses the `State` monad, and is roughly 300 times slower<sup>1</sup> than its Java equivalent.

## [Implementation 2](https://github.com/runeksvendsen/ufw/tree/st)

Less naïve. Uses the `ST` monad (to support mutable arrays and references). About as fast<sup>1</sup> as its Java counterpart.

<sup>1</sup> for [`largeUF.txt`](https://algs4.cs.princeton.edu/15uf/largeUF.txt).
