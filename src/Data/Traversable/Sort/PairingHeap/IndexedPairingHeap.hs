{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeOperators, GADTs, RoleAnnotations #-}
{-# LANGUAGE BangPatterns #-}
module Data.Traversable.Sort.PairingHeap.IndexedPairingHeap (
    Heap
  , Sized (..)
  , empty
  , singleton
  , insert
  , merge
  , minView
  ) where
import Data.Traversable.Sort.PairingHeap.BasicNat
import Data.Type.Equality ((:~:)(..))
import Data.Coerce
import Data.Type.Coercion

-- | Okasaki's simple representation of a pairing heap, but with
-- a size index.
data Heap n a where
  E :: Heap 'Z a
  T :: a -> HVec n a -> Heap ('S n) a

-- Coercing a heap could destroy the heap property, so we declare both
-- type parameters nominal.
type role Heap nominal nominal

-- | A vector of heaps whose sizes sum to the index.
data HVec n a where
  HNil :: HVec 'Z a
  HCons :: Heap m a -> HVec n a -> HVec (m + n) a

class Sized h where
  -- | Calculate the size of a structure
  size :: h n a -> Natty n

instance Sized Heap where
  size E = Zy
  size (T _ xs) = Sy (size xs)

instance Sized HVec where
  size HNil = Zy
  size (HCons h hs) = size h `plus` size hs

-- Produce an empty heap
empty :: Heap 'Z a
empty = E

-- Produce a heap with one element
singleton :: a -> Heap ('S 'Z) a
singleton a = T a HNil

-- Insert an element into a heap
insert :: Ord a => a -> Heap n a -> Heap ('S n) a
insert x xs = merge (singleton x) xs
{-# INLINABLE insert #-}

-- Merge two heaps
merge :: Ord a => Heap m a -> Heap n a -> Heap (m + n) a
merge E ys = ys
merge xs E = case plusZero (size xs) of Refl -> xs
merge h1@(T x xs) h2@(T y ys)
  | x <= y = case plusCommutative (size h2) (size xs) of Refl -> T x (HCons h2 xs)
  | otherwise = case plusSucc (size xs) (size ys) of Refl -> T y (HCons h1 ys)
{-# INLINABLE merge #-}

-- Get the smallest element of a non-empty heap, and the rest of
-- the heap
minView :: Ord a => Heap ('S n) a -> (a, Heap n a)
minView (T x hs) = (x, mergePairs hs)
{-# INLINABLE minView #-}

mergePairs :: Ord a => HVec n a -> Heap n a
mergePairs HNil = E
mergePairs (HCons h HNil) = case plusZero (size h) of Refl -> h
mergePairs (HCons h1 (HCons h2 hs)) =
  case plusAssoc (size h1) (size h2) (size hs) of
    Refl -> merge (merge h1 h2) (mergePairs hs)
{-# INLINABLE mergePairs #-}
