import System.Random
import Control.Monad

tenInts :: IO ()
tenInts = do
    gen <- newStdGen
    let ns = randoms gen :: [Int]
    print $ take 10 ns

tenFloats :: Int -> IO ()
tenFloats seed = do
    let ns = randoms (mkStdGen seed) :: [Float]
    print $ take 10 ns

-- the dumbest way to deal with random numbers is to just explicitly
-- pass the state around for the RNG

rng :: StdGen
rng = mkStdGen 42

-- randomR takes a range and an rng and spits our a pseudo-random number + 
-- next state of the generator

getInterval :: StdGen -> (Double, StdGen)
getInterval = randomR (0 :: Double, 1)

genRands :: Int -> IO ()
genRands n = do
    xs <- randomRIO (0 :: Double, 1)
    print $ show xs

