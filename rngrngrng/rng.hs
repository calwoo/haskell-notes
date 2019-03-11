-- A note on RNGs using the State monad

{-# LANGUAGE InstanceSigs, TypeSynonymInstances #-}

import Control.Monad
import System.Random

---- state monad
data State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where
    return :: a -> State s a
    return x = State $ \s -> (x,s)

    (>>=) :: State s a -> (a -> State s b) -> State s b
    st >>= f = State $ \s -> let (x,s') = runState st s
                      in runState (f x) $ s'

instance Functor (State s) where
    fmap = liftM
instance Applicative (State s) where
    pure  = return
    (<*>) = ap

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put state = State $ \_ -> ((), state)

type StringState a = State String a

testState :: StringState Int
testState = do
    initState <- get
    let newState = "Hello " ++ initState
    put newState
    return 1

---- random number generator using state
type RandomState a = State StdGen a

runRandom :: RandomState a -> StdGen -> a
runRandom st gen = r
    where (r,_) = runState st gen

rand :: Random a => RandomState a
rand = do
    gen <- get
    let (x, gen') = random gen
    put gen'
    return x

randR :: Random a => (a, a) -> RandomState a
randR (a,b) = do
    gen <- get
    let (r, gen') = randomR (a,b) gen
    put gen'
    return r

rollTwoDice :: RandomState Int
rollTwoDice = do
    roll1 <- randR (1,6) :: RandomState Int
    roll2 <- randR (1,6) :: RandomState Int
    return (roll1 + roll2)

randTwo :: RandomState (Int, Int)
randTwo = do
    r1 <- rand
    r2 <- rand
    return (r1, r2)

---- approximating pi
piTrial :: RandomState Bool
piTrial = do
    r1 <- randR (-1.0, 1.0) :: RandomState Double
    r2 <- randR (-1.0, 1.0) :: RandomState Double
    return (r1*r1 + r2*r2 < 1)

bernoulliTrials :: Int -> RandomState Bool -> RandomState Int
bernoulliTrials n condition = if n == 0
    then return 0
    else do
        suc <- condition
        others <- bernoulliTrials (n-1) condition
        if suc then return (1 + others)
        else return others

approxPi :: Int -> RandomState Double
approxPi n = do
    success <- bernoulliTrials n piTrial
    return $ (fromIntegral $ 4 * success) / (fromIntegral n)