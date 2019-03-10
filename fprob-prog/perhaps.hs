-- Notes on the series of blog posts by Eric Kidd.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad
import Control.Monad.Trans

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
-- our distributions.

type Dist = PerhapsT []

-- A standard way to get a probability distribution is to start with a sample space with
-- "relative weights" and normalizing

normalizeWeights :: [(a, Float)] -> Dist a
normalizeWeights [] = error "empty dist"
normalizeWeights xs = PerhapsT $ map prob xs
    where prob (x,wgt) = Perhaps x (P (wgt/total))
          total = foldr (\(_,wgt) acc -> acc + wgt) 0 xs

uniform :: [a] -> Dist a -- uniform distribution
uniform xs = normalizeWeights $ map (\x -> (x,1)) xs