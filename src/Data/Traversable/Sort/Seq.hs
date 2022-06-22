{-# LANGUAGE GADTs #-}
module Data.Traversable.Sort.Seq (
    sortTraversable,
) where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data Sort a r where
    Sort :: (Seq a -> (Seq a, r)) -> !(Seq a) -> Sort a r

instance Functor (Sort a) where
    fmap f (Sort g xs) = Sort (\ys -> case g ys of (zs, r) -> (zs, f r)) xs

-- Note: no Ord!
instance Applicative (Sort a) where
    pure x = Sort (\xs -> (xs, x)) Seq.empty

    Sort f xs <*> Sort g ys = Sort h (xs <> ys) where
        h zs = case f zs of
            (zs', f') -> case g zs' of
                (zs'', x') -> (zs'', f' x')

liftSort :: Ord x => x -> Sort x x
liftSort x = Sort (\ys -> case ys of
                      Seq.Empty     -> (Seq.Empty, x) -- this shouldn't happen, but it's easy to fullfil anyway.
                      y Seq.:<| ys' -> (ys', y)
                  ) (Seq.singleton x)

-- Ord is here!
runSort :: Ord x => Sort x a -> a
runSort (Sort f xs) = snd (f (Seq.sort xs))

-- | Sort an arbitrary 'Traversable' container using a heap.
sortTraversable :: (Ord a, Traversable t) => t a -> t a
sortTraversable = runSort . traverse liftSort
{-# INLINABLE sortTraversable #-}
