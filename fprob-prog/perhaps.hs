-- Notes on the series of blog posts by Eric Kidd.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad

newtype Prob = P Float
    deriving (Eq, Ord, Num)

instance Show Prob where
    show (P p) = show intPart ++ "." ++ show fracPart ++ "%"
        where digits = round (1000 * p)
              intPart = digits `div` 10
              fracPart = digits `mod` 10

-- Perhaps represents a value with an associated probability.

data Perhaps a = Perhaps a Prob
    deriving (Show)

neverCheck :: Perhaps a -> Bool
neverCheck (Perhaps _ 0) = True
neverCheck _             = False

-- One can think of perhaps as a "stochastic maybe".

instance Functor Perhaps where
    fmap f (Perhaps x p) = Perhaps (f x) p

instance Applicative Perhaps where
    pure  = return
    (<*>) = ap

instance Monad Perhaps where
    return x = Perhaps x 1
    pe@(Perhaps x p1) >>= f
        | neverCheck pe  = Perhaps undefined 0
        | otherwise      = let Perhaps y p2 = f x
                            in Perhaps y (p1 * p2)
                
-- We will turn perhaps into a monad transformer.

class Monad m => MonadPerhaps m where
    perhaps :: a -> Prob -> m a
    never   :: m a

instance MonadPerhaps Perhaps where
    perhaps = Perhaps
    never   = Perhaps undefined 0

