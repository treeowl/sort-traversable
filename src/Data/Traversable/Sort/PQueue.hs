{-# LANGUAGE GADTs #-}
module Data.Traversable.Sort.PQueue (
    sortTraversable,
) where

import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MinQueue

data Sort a r where
    Sort :: (MinQueue a -> (MinQueue a, r)) -> !(MinQueue a) -> Sort a r

instance Functor (Sort a) where
    fmap f (Sort g xs) = Sort (\ys -> case g ys of (zs, r) -> (zs, f r)) xs

instance Ord a => Applicative (Sort a) where
    pure x = Sort (\xs -> (xs, x)) MinQueue.empty

    Sort f xs <*> Sort g ys = Sort h (xs <> ys) where
        h zs = case f zs of
            (zs', f') -> case g zs' of
                (zs'', x') -> (zs'', f' x')

liftSort :: Ord x => x -> Sort x x
liftSort x = Sort (\ys -> case MinQueue.minView ys of
                      Nothing       -> (MinQueue.empty, x) -- this shouldn't happen, but it's easy to fullfil anyway.
                      Just (y, ys') -> (ys', y)
                  ) (MinQueue.singleton x)

runSort :: Sort x a -> a
runSort (Sort f xs) = snd (f xs)

-- | Sort an arbitrary 'Traversable' container using a heap.
sortTraversable :: (Ord a, Traversable t) => t a -> t a
sortTraversable = runSort . traverse liftSort
{-# INLINABLE sortTraversable #-}
