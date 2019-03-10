-- Note on the Giry monad and probabilistic measures.

{-# LANGUAGE InstanceSigs #-}

import Control.Monad
import Numeric.Integration.TanhSinh
-- import Numeric.SpecFunctions

-- We approach it via the integration operator that takes a measure and a measurable function on X and
-- returns the integral of the function with respect to the measure.
-- The type signature of the operator fixed at a measure v is :: (X -> Reals) -> Reals
-- i.e. it takes a measurable function and returns its integral.

-- This looks like a continuation.

newtype Cont a r = Cont { runCont :: (a -> r) -> r }

-- In fact, we can call our integration operator as a continuation to the reals.

newtype Integral a = Integral { integral :: (a -> Double) -> Double }

-- But since we can only simulate measures via their associated integration operators (and not as mappings
-- from \sigma-algebras, we might as well overload our notations and *call* an integration operator
-- a measure.

newtype Measure a = Measure ((a -> Double) -> Double)

integrate :: (a -> Double) -> Measure a -> Double
integrate f (Measure nu) = nu f -- this is exactly the evaluation relation in notes

-- The above integration operator has the form \integral f d\nu.
-- A subtle point is that we don't talk about the space that we integrate over. That should be encoded in the
-- definition of a measure itself.

-- The change-of-variables formula (https://en.wikipedia.org/wiki/Pushforward_measure) for pushforward measures
-- gives the functor instance of Measure

instance Functor Measure where
    fmap f nu = Measure $ \g -> integrate (g . f) nu

-- The monad structure of the Giry monad allows us to sequence probability measures together by marginalizing
-- one into another. 

instance Monad Measure where
    -- Dirac measure
    return :: a -> Measure a
    return x = Measure $ \f -> f x

    (>>=) :: Measure a -> (a -> Measure b) -> Measure b
    rho >>= g = Measure $ \f ->
        integrate (\m -> integrate f (g m)) rho

instance Applicative Measure where
    pure  = return
    (<*>) = ap

--------------------
-- Discrete RVars --
--------------------

-- So now we just need a way to build measures. We usually get them from probability mass functions.
-- We'll take in a mass function, the support of the function (rep. by list) and return a measure.

fromPMF :: (a -> Double) -> [a] -> Measure a
fromPMF mf support = Measure $ \f ->
    foldl (\acc x -> acc + (f x) * (mf x)) 0 support

-- Binomial distribution:

binCoeff :: Int -> Int -> Int -- inefficient implementation of n choose k coeff
binCoeff n k
    | k == 0    = 1
    | n == k    = 1
    | otherwise = binCoeff (n-1) k + binCoeff (n-1) (k-1)

binomial :: Int -> Double -> Measure Int
binomial n p = fromPMF (binomPMF n p) [0..n] where
    binomPMF n p k
        | k < 0 || n < k = 0
        | otherwise = fromIntegral (binCoeff n k) * p^^k * (1-p)^^(n-k)

----------------------
-- Continuous RVars --
----------------------

-- For continuous probability distributions, we have a density function supported on the entire real line.

fromPDF :: (Double -> Double) -> Measure Double
fromPDF pdf = Measure $ \f ->
    quadrature (\x -> (f x) * (pdf x))
        where quadrature = result . last . everywhere trap

gaussian :: Double -> Double -> Measure Double
gaussian m s2 = fromPDF (gaussianPDF m s2) where
    gaussianPDF m s2 x
        | s2 <= 0   = 0
        | otherwise = let coef = 1 / (s2 * sqrt (2 * pi))
                       in coef * exp (negate ((x - m)^^2 / (2 * s2^^2)))

-- Now we can use the Giry monad to create more complicated probability distributions:
-- Consider a generative model where we use a gaussian to determine the mean of another
-- gaussian. This is for example, featured in a variational autoencoder (an infinite mixture of gaussians).

infiniteMixture :: Double -> Measure Double
infiniteMixture s2 = gaussian 0 1 >>= (\m -> gaussian m s2)

----------------------
-- Basic statistics --
----------------------

-- Now with our integration viewpoint, we can ask for some basic statistical queries.

expectation :: (Double -> Double) -> Measure Double -> Double
expectation f nu = integrate f nu

mean :: Measure Double -> Double
mean = expectation id

moment :: Int -> Measure Double -> Double
moment n nu
    | n < 0     = 0
    | otherwise = expectation (^^n) nu

variance :: Measure Double -> Double
variance nu = moment 2 nu - (mean nu)^^2

-- cumulative distribution function (cdf)

cdf :: Measure Double -> Double -> Double
cdf nu x = integrate (negativeInfinity `to` x) nu

negativeInfinity :: Double
negativeInfinity = negate (1 / 0)

to :: (Num a, Ord a) => a -> a -> a -> a
to a b x
    | x >= a && x <= b = 1
    | otherwise        = 0