-- Defining probability distributions via sampling.

{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

import Control.Monad
import Control.Monad.Trans

type Weight = Float

class (Functor d, Monad d) => Dist d where
    normalize :: [(a, Weight)] -> d a

uniform :: Dist d => [a] -> d a
uniform = normalize . map (\x -> (x, 1))

-- example)

data Child = Boy | Girl
    deriving (Show, Eq, Ord)

child :: Dist d => d Child
child = uniform [Boy, Girl]

family :: Dist d => d [Child]
family = do
    child1 <- child
    child2 <- child
    return [child1, child2]

-- The above gives us an API for distributions. We will implement this via lists
-- and sampling functions.

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

-- PerhapsT
newtype PerhapsT m a = PerhapsT { runPerhapsT :: m (Perhaps a) }

instance MonadTrans PerhapsT where
    lift x = PerhapsT (liftM return x)

instance Monad m => Monad (PerhapsT m) where
    return   = lift . return
    pt >>= f = PerhapsT x
        where x = do
                ptv <- runPerhapsT pt
                case ptv of
                    Perhaps x1 p1
                        | p1 == 0   -> return never
                        | otherwise -> do
                                (Perhaps x2 p2) <- runPerhapsT (f x1)
                                return $ Perhaps x2 (p1 * p2) 
    
instance Monad m => Functor (PerhapsT m) where
    fmap = liftM

instance Monad m => Applicative (PerhapsT m) where
    pure  = pure
    (<*>) = ap

-- Now that we can endow monads with a stochastic component, lets define
-- our (finite) distributions.

type FDist = PerhapsT []

instance Dist FDist where
    normalize [] = error "empty dist"
    normalize xs = PerhapsT $ map prob xs
        where prob (x,wgt) = Perhaps x (P (wgt / total))
              total = foldr (\(_,w) acc -> w + acc) 0 xs

exact :: FDist a -> [Perhaps a]
exact = runPerhapsT

-- For continuous variables, finite distributions won't cut it. In fact, enumerating our
-- choices in a list is impossible. Instead, we change paradigms and define distributions
-- as functions we can sample from.

