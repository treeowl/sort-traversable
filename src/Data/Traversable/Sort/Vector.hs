{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Traversable.Sort.Vector (sortTraversableBy, sortTraversable) where
import           Control.Monad.ST.Strict
import           Control.Monad.State.Strict
import           Data.Foldable
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Mutable          as VM

{-# INLINE sortTraversableBy #-}
sortTraversableBy :: (Ord a, Traversable f)
       => (forall s. VM.STVector s a -> ST s ())
       -> f a
       -> f a
sortTraversableBy sort val = runST (do
  vec <- indexed val
  sort vec
  evalStateT (traverse
    (\_ -> StateT
      (\i -> do
          r <- VM.unsafeRead vec i
          return (r, i + 1)))
    val)
    (0 :: Int))

{-# INLINE sortTraversable #-}
-- | Sort a traversable container using introsort from vector-algorithms.
sortTraversable :: (Ord a, Traversable f) => f a -> f a
sortTraversable = sortTraversableBy Intro.sort

data P s a = P
  {-# UNPACK #-} !Int
  !(VM.STVector s a -> ST s ())

{-# INLINE indexed #-}
indexed :: forall f a s. (Ord a, Foldable f) => f a -> ST s (VM.STVector s a)
indexed x = do
  case foldl'
    (\(P i f) el -> P
       (i + 1)
       (\v -> f v >> VM.unsafeWrite v i el))
    (P 0 (\_ -> return ()) :: P s a)
    x of
    P len initFn -> do
      vec <- VM.unsafeNew len
      initFn vec
      return vec


