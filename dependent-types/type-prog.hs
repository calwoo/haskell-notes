{-- 
notes following "basic type level programming in haskell"
https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html
--}

{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Maybe

data Unit = MkUnit

nothingA = Nothing :: Maybe a
nothingInt = Nothing :: Maybe Int
nothingChar = Nothing :: Maybe Char

data HigherKinded f a
    = Bare a
    | Wrapped (f a)

-- type-level Peano

data Zero
data Succ a

type One = Succ Zero
type Two = Succ One
type Three = Succ Two
type Four = Succ (Succ (Succ One))

-- problem: we can write
type What = Succ Bool

-- this is strange, but this comes from the fact that our only kind is *
-- we need more kinds to restrict what types we can plug into Succ, which
-- we do by using the DataKinds extension

-- ### DataKinds

data Nat = Z | S Nat

-- this forms a type-error, as Bool is not of kind "Nat"!
-- type What2 = S Bool

-- ### GADTs

data IntBool a where
    Int :: Int -> IntBool Int
    Bool :: Bool -> IntBool Bool

-- this has extra type hints, as we know that matching on Int tells us
-- a ~ Int (constraint), so that we return an Int value

extractIntBool :: IntBool a -> a
extractIntBool (Int _)  = 0
extractIntBool (Bool b) = b

-- ### Dependent types

data Vector (n :: Nat) a where
    VNil  :: Vector Z a
    VCons :: a -> Vector n a -> Vector (S n) a

instance Show a => Show (Vector n a) where
    show VNil         = "VNil"
    show (VCons a as) = "VCons " ++ show a ++ " (" ++ show as ++ ")"

-- lets try to implement append. for vectors, we also need to explicitly define
-- the size of the sum vector, but that means we need to "add" types together.
-- to get such "type functions", we use the TypeFamilies extension

-- ### TypeFamilies

type family Add n m where
    Add Z n     = n
    Add (S n) m = S (Add n m)

append :: Vector n a -> Vector m a -> Vector (Add n m) a
append VNil ys         = ys
append (VCons x xs) ys = VCons x (append xs ys)