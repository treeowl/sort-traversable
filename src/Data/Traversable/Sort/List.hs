{-# LANGUAGE GADTs #-}
module Data.Traversable.Sort.List (
    sortTraversable,
) where

data Sort a r where
    Sort :: ([a] -> ([a], r)) -> [a] -> Sort a r

instance Functor (Sort a) where
    fmap f (Sort g xs) = Sort (\ys -> case g ys of (zs, r) -> (zs, f r)) xs

instance Ord a => Applicative (Sort a) where
    pure x = Sort (\xs -> (xs, x)) []

    Sort f xs <*> Sort g ys = Sort h (merge xs ys) where
        h zs = case f zs of
            (zs', f') -> case g zs' of
                (zs'', x') -> (zs'', f' x')

liftSort :: Ord x => x -> Sort x x
liftSort x = Sort (\ys -> case ys of
                      []    -> ([], x) -- this shouldn't happen, but it's easy to fullfil anyway.
                      y:ys' -> (ys', y)
                  ) [x]

runSort :: Sort x a -> a
runSort (Sort f xs) = snd (f xs)

merge :: Ord a => [a] -> [a] -> [a]
merge []     ys     = ys
merge xs     []     = xs
merge (x:xs) (y:ys)
    | x < y     = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- | Sort an arbitrary 'Traversable' container using a heap.
sortTraversable :: (Ord a, Traversable t) => t a -> t a
sortTraversable = runSort . traverse liftSort
{-# INLINABLE sortTraversable #-}
