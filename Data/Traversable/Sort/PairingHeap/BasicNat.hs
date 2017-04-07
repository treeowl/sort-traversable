{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, GADTs,
      ScopedTypeVariables, TypeOperators #-}

-- | Type-level natural numbers and singletons, with proofs of
-- a few basic properties.

module Data.Traversable.Sort.PairingHeap.BasicNat (
    -- | Type-level natural numbers
    Nat (..)
  , type (+)

    -- | Natural number singletons
  , Natty (..)
  , plus

    -- | Basic properties
  , plusCommutative
  , plusZero
  , plusSucc
  , plusAssoc
  ) where
import Data.Type.Equality ((:~:)(..))
import Unsafe.Coerce

-- | Type-level natural numbers
data Nat = Z | S Nat

-- | Type-level natural number addition
type family (+) m n where
  'Z + n = n
  ('S m) + n = 'S (m + n)

-- | Singletons for natural numbers
data Natty n where
  Zy :: Natty 'Z
  Sy :: Natty n -> Natty ('S n)

-- | Singleton addition
plus :: Natty m -> Natty n -> Natty (m + n)
plus Zy n = n
plus (Sy m) n = Sy (plus m n)


----------------------------------------------------------
-- Proofs of basic arithmetic
--
-- The legitimate proofs are accompanied by rewrite rules that
-- effectively assert termination. These rules prevent us from
-- actually having to run the proof code, which would be slow.

plusCommutative :: Natty m -> Natty n -> (m + n) :~: (n + m)
plusCommutative Zy n = case plusZero n of Refl -> Refl
plusCommutative (Sy m) n =
  case plusCommutative m n of { Refl ->
  case plusSucc n m of Refl -> Refl }
{-# NOINLINE plusCommutative #-}

plusZero :: Natty m -> (m + 'Z) :~: m
plusZero Zy = Refl
plusZero (Sy n) = case plusZero n of Refl -> Refl
{-# NOINLINE plusZero #-}

plusSucc :: Natty m -> proxy n -> (m + 'S n) :~: ('S (m + n))
plusSucc Zy _ = Refl
plusSucc (Sy n) p = case plusSucc n p of Refl -> Refl
{-# NOINLINE plusSucc #-}

plusAssoc :: Natty m -> p1 n -> p2 o -> (m + (n + o)) :~: ((m + n) + o)
plusAssoc Zy _ _ = Refl
plusAssoc (Sy m) p1 p2 = case plusAssoc m p1 p2 of Refl -> Refl
{-# NOINLINE plusAssoc #-}

{-# RULES
"plusCommutative" forall m n. plusCommutative m n = unsafeCoerce (Refl :: 'Z :~: 'Z)
"plusZero" forall m . plusZero m = unsafeCoerce (Refl :: 'Z :~: 'Z)
"plusSucc" forall m n. plusSucc m n = unsafeCoerce (Refl :: 'Z :~: 'Z)
"plusAssoc" forall m p1 p2. plusAssoc m p1 p2 = unsafeCoerce (Refl :: 'Z :~: 'Z)
 #-}
