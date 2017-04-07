It is not immediately obvious that `Sort` is a lawful `Applicative` at all.
Let's see if we can figure it out! The indices just get in the way here, so
let's clean up the `Applicative` instance a bit. It won't compile like this,
but that doesn't matter.

```haskell
pure x = Sort (\_ h -> (h, x)) empty

(<*>) :: forall a b . Sort x (a -> b) -> Sort x a -> Sort x b
Sort f xs <*> Sort g ys =
  Sort h (merge xs ys)
  where
    h :: forall o. Proxy o -> Heap ((m + n) + o) x -> (Heap o x, b)
    h p v = case f Proxy v of { (v', a) ->
              case g Proxy v' of { (v'', b) ->
                (v'', a b)}}
```

As ["paf31" noted](https://www.reddit.com/r/haskell/comments/63a4ea/fast_total_sorting_of_arbitrary_traversable/dfu4uar/), `Sort a` is (indices, proxies, and strictness annotation aside) the
[`Product`](https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Functor-Product.html)
of two applicative functors:

```haskell
Sort a ~= Product (State (Heap a)) (Const (Heap a))
```

with precisely the `Applicative` instance that suggests.
