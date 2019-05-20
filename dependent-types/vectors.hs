{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

import Data.Maybe

{---

unsafe :: Int
unsafe = head []

this will type-check, but be a runtime-error! How do we promote such entities to type-level statements?

--}

-- dependent types are types that depend on values.
-- Haskell doesn't allow them, but we can "simulate" them -- singletons!

data Nat = Z | S Nat

infixl 6 :+

type family (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z :+ m = m
type instance (S n) :+ m = S (n :+ m)

infixl 7 :*

type family (n :: Nat) :* (m :: Nat) :: Nat
type instance Z :* m = Z
type instance (S n) :* m = (n :* m) :+ m

-- vector implementation

data Vector a n where
    Nil  :: Vector a Z
    (:-) :: a -> Vector a n -> Vector a (S n)
infixr 5 :-

deriving instance Eq a => Eq (Vector a n)
instance Show a => Show (Vector a n) where
    showsPrec d = showsPrec d . toList

toList :: Vector a n -> [a]
toList Nil = []
toList (x :- xs) = x : toList xs

{-- why isn't this working?
fromList :: Nat -> [a] -> Maybe (Vector a n)
fromList Z _ = Just Nil
fromList (S n) (x : xs) = (x :-) <$> fromList n xs
fromList _ _ = Nothing
--}

-- vectors now have safe heads and tails

headV :: Vector a (S n) -> a
headV (x :- _) = x

tailV :: Vector a (S n) -> Vector a n
tailV (_ :- xs) = xs

appendV :: Vector a n -> Vector a m -> Vector a (n :+ m)
appendV (x :- xs) ys = x :- appendV xs ys
appendV Nil ys = ys

{--

replicate :: Nat -> a -> Vector a n
replicate Z _ = Nil
replicate (S n) a = a :- replicate n a

This doesn't type-check as the type n in the vector doesn't match Nat.
But if we replace Nat with the matching n, it becomes a type with no inhabitants,
    so we can't pattern match

--}

-- singleton natural numbers
data SNat n where
    SZ :: SNat Z
    SS :: SNat n -> SNat (S n)

data SBool b where
    STrue  :: SBool True
    SFalse :: SBool False

(%:+) :: SNat n -> SNat m -> SNat (n :+ m)
SZ   %:+ m = m
SS n %:+ m = SS (n %:+ m)

(%:*) :: SNat n -> SNat m -> SNat (n :* m)
SZ   %:* m = SZ
SS n %:* m = (n %:* m) %:+ m

replicateV :: SNat n -> a -> Vector a n
replicateV SZ _     = Nil
replicateV (SS n) a = a :- replicateV n a

sLength :: Vector a n -> SNat n
sLength Nil = SZ
sLength (x :- xs) = (SS SZ) %:+ (sLength xs)