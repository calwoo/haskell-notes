-- Defining probability distributions via sampling.

{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

import Data.List
import System.Random
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

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
    deriving (Eq, Ord, Num, Fractional)

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

newtype Rand a = Rand { runRand :: IO a }

randomFloat :: Rand Float
randomFloat = Rand $ getStdRandom random

instance Functor Rand where
    fmap = liftM
instance Applicative Rand where
    pure  = return
    (<*>) = ap

instance Monad Rand where
    return x = Rand $ return x
    r >>= f  = Rand $ do x <- runRand r
                         runRand (f x)

-- The upshot is that we can turn any finitely-supported distribution into a sampling function.

toSampler :: FDist a -> Rand a
toSampler fd = do
    n <- randomFloat
    sample (P n) (runPerhapsT fd)

sample :: Monad m => Prob -> [Perhaps a] -> m a
sample _ [] = error "nothing here"
sample n ((Perhaps x p):ps)
    | n <= p    = return x
    | otherwise = sample (n-p) ps

randSamples :: Rand a -> Int -> Rand [a]
randSamples r n = sequence $ replicate n r

randSamplesIO :: Rand a -> Int -> IO [a]
randSamplesIO r n = runRand $ randSamples r n

instance Dist Rand where
    normalize = toSampler . normalize

-- display histogram of sampling results
histogram :: Ord a => [a] -> [Int]
histogram =  map length . group . sort

---- EXAMPLE) bayes rule

data Test = Pos | Neg
    deriving (Show, Eq)

data Status = Flu | Healthy
    deriving (Show, Eq)

outcomes :: [(Status, Test)]
outcomes = do
    status <- [Flu, Healthy]
    test   <- [Pos, Neg]
    return (status, test)

fluTest1 :: FDist (Status, Test)
fluTest1 = do
    status <- normalize [(Flu,10), (Healthy,90)]
    test   <- if status == Flu
              then normalize [(Pos,70), (Neg,30)]
              else normalize [(Pos,10), (Neg,90)]
    return (status, test)

-- bayes rule requires us to collapse some of the results together, so we mask possibilities
-- using the maybe monad

fluTest2 :: FDist (Maybe Status)
fluTest2 = do
    (status, test) <- fluTest1
    return (if test == Pos
            then Just status
            else Nothing)

-- we want to get rid of the nothings and renormalize for the justs

-- getters and setters
value (Perhaps x _) = x
prob (Perhaps _ p)  = p

combineMaybes :: [Perhaps (Maybe a)] -> [Perhaps a]
combineMaybes [] = []
combineMaybes ((Perhaps Nothing _):ps)  = combineMaybes ps
combineMaybes ((Perhaps (Just x) p):ps) =
    (Perhaps x p) : (combineMaybes ps)

extractJusts :: FDist (Maybe a) -> FDist a
extractJusts dist
    | totalp > 0 = PerhapsT $ map norm filtered
    | otherwise  = PerhapsT []
    where
        filtered = combineMaybes $ runPerhapsT dist
        totalp   = sum (map prob filtered)
        norm (Perhaps x p) = Perhaps x (p / totalp)

fluTest3 :: FDist Status
fluTest3 = extractJusts fluTest2

-- in ghci, typing runPerhapsT fluTest3 returns
-- > [Perhaps Flu 43.8%,Perhaps Healthy 56.2%]

-- how do we automate this reasoning into the monad? Use the MaybeT transformer!

type FBDist = MaybeT FDist -- finite "bayesian dist"

instance Dist FBDist where
    normalize = lift . normalize

-- now to combine the bayesian inference into one

condition :: Bool -> FBDist ()
condition True  = MaybeT $ return (Just ())
condition False = MaybeT $ return Nothing

bayesTest :: FBDist Status -> FBDist Status
bayesTest prior = do
    status <- prior
    test   <- if status == Flu
              then normalize [(Pos,70), (Neg,30)]
              else normalize [(Pos,10), (Neg,90)]
    condition (test == Pos)
    return status

getBayes :: FBDist Status -> [Perhaps Status]
getBayes = runPerhapsT . extractJusts . runMaybeT

prior :: FBDist Status
prior = normalize [(Flu,10), (Healthy,90)]

-- in ghci, running getBayes (bayesTest prior)
-- gives the same answer!

---- EXAMPLE) robot localization using particle filters

-- A particle system can be represented as a function that maps an integer to a random list of particles.

newtype PS a = PS { runPS :: Int -> Rand [a] }

-- we can take any Rand a value and build a particle system by just repeatedly sampling

liftRand :: Rand a -> PS a
liftRand r = PS (randSamples r)

instance Functor PS where
    fmap f ps = PS $ mapped
        where mapped n = liftM (map f) (runPS ps n)

instance Applicative PS where
    pure  = return
    (<*>) = ap

instance Monad PS where
    return   = liftRand . return
    ps >>= f = joinPS (fmap f ps)

joinPS :: PS (PS a) -> PS a
joinPS pps = PS $ joinPS' pps

joinPS' :: PS (PS a) -> Int -> Rand [a]
joinPS' pps n = do
    ps <- runPS pps n
    xs <- sequence (map (\x -> runPS x 1) ps)
    return (concat xs)

instance Dist PS where
    normalize = liftRand . normalize