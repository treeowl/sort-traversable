{-# LANGUAGE ScopedTypeVariables, GADTs, TypeOperators,
      RankNTypes, InstanceSigs, DataKinds #-}
module Data.Traversable.Sort.PairingHeap where
import Data.Traversable.Sort.PairingHeap.IndexedPairingHeap (
    Heap
  , Sized (..)
  , empty
  , singleton
  , merge
  , minView
  )
import Data.Proxy
import Data.Type.Equality ((:~:) (..))
import Data.Traversable.Sort.PairingHeap.BasicNat
        (type (+), Nat (..), plusAssoc, plusZero)

-- | A heap of some size whose element have type @a@ and a
-- function that, applied to any heap at least that large,
-- will produce a result and the rest of the heap.
data Sort a r where
  Sort :: (forall n. Proxy n -> Heap (m + n) a -> (Heap n a, r))
       -> !(Heap m a)
       -> Sort a r

instance Functor (Sort x) where
  fmap f (Sort g h) =
    Sort (\p h' -> case g p h' of (remn, r) -> (remn, f r)) h
  {-# INLINE fmap #-}

instance Ord x => Applicative (Sort x) where
  pure x = Sort (\_ h -> (h, x)) empty
  {-# INLINE pure #-}

  -- Combine two 'Sort's by merging their heaps and composing
  -- their functions.
  (<*>) :: forall a b . Sort x (a -> b) -> Sort x a -> Sort x b
  Sort f (xs :: Heap m x) <*> Sort g (ys :: Heap n x) =
    Sort h (merge xs ys)
    where
      h :: forall o . Proxy o -> Heap ((m + n) + o) x -> (Heap o x, b)
      h p v = case plusAssoc (size xs) (size ys) p of
        Refl -> case f (Proxy :: Proxy (n + o)) v of { (v', a) ->
                case g (Proxy :: Proxy o) v' of { (v'', b) ->
                  (v'', a b)}}
  {-# INLINABLE (<*>) #-}

-- Produce a 'Sort' with a singleton heap and a function that will
-- produce the smallest element of a heap.
liftSort :: Ord x => x -> Sort x x
liftSort a = Sort (\_ h -> case minView h of (x, h') -> (h', x)) (singleton a)
{-# INLINABLE liftSort #-}

-- Apply the function in a 'Sort' to the heap within, producing a
-- result.
runSort :: forall x a . Sort x a -> a
runSort (Sort f xs) = case plusZero (size xs) of
    Refl -> snd $ f (Proxy :: Proxy 'Z) xs

-- | Sort an arbitrary 'Traversable' container using a heap.
sortTraversable :: (Ord a, Traversable t) => t a -> t a
sortTraversable = runSort . traverse liftSort
{-# INLINABLE sortTraversable #-}

-- | Sort an arbitrary container using a 'Traversal' (in the
-- 'lens' sense).
sortTraversal :: Ord a => ((a -> Sort a a) -> t -> Sort a t) -> t -> t
sortTraversal trav = runSort . trav liftSort
{-# INLINABLE sortTraversal #-}
