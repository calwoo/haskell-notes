-- A comonad is the dual of a monad.

{-# LANGUAGE DeriveFunctor, InstanceSigs #-}

class Functor w => Comonad w where
    -- Here we dualize the kleisli composition
    (=>=) :: (w a -> b) -> (w b -> c) -> (w a -> c)
    extract :: w a -> a
    duplicate :: w a -> w (w a)

-- What good are they?

-- Intuitively, monads allow you to place values into computational contexts, but there
-- is no given way to get that value out. Comonads give you a way to extract a value from
-- a computational context, but not a way to placing it into one.

-- example) A standard monad is the reader monad, given by function types (e ->). It provides
-- a context for computations that require some external read-only data. By adjunction, arrows
-- a -> (e -> b) are equivalent to (a, e) -> b

data Product e a = Prod e a
    deriving Functor

-- This is a comonad, by adjunction!

instance Comonad (Product e) where
    (=>=) :: (Product e a -> b) -> (Product e b -> c) -> (Product e a -> c)
    f =>= g = \(Prod e a) -> let b = f (Prod e a)
                              in g (Prod e b)

    duplicate :: Product e a -> Product e (Product e a)
    duplicate (Prod e a) = Prod e (Prod e a)

    extract :: Product e a -> a
    extract (Prod e a) = a

-- We can also dualize the standard monadic functions, bind and join.

-- extend :: (w a -> b) -> w a -> w b
-- duplicate :: w a -> w (w a)

-- Comonads formalize the process of shifting focus between elements in a computational
-- container/context.

-- example) Streams, which are infinite lists

data Stream a = Cons a (Stream a)

instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Comonad Stream where
    -- The focus of a stream is the first element
    extract :: Stream a -> a
    extract (Cons a _) = a

    -- Duplicate creates a stream of streams, each focused on a different element.
    duplicate :: Stream a -> Stream (Stream a)
    duplicate (Cons x xs) = Cons (Cons x xs) (duplicate xs)

    f =>= g = g . fmap f . duplicate

tail :: Stream a -> Stream a
tail (Cons x xs) = xs

-- Comonads allow us to propagate a computation while focusing on different elements of
-- a data structure.

sumS :: Num a => Int -> Stream a -> a
sumS n (Cons x xs) = if n <= 0 then 0 else x + sumS (n-1) xs

average :: Fractional a => Int -> Stream a -> a
average n st = (sumS n st) / (fromIntegral n)

-- extends a comonadic computation to all the "neighboring values" of the values in
-- the container.
extend :: Comonad w => (w a -> b) -> w a -> w b
extend f = fmap f . duplicate

movingAvg :: Fractional a => Int -> Stream a -> Stream a
movingAvg n = extend (average n) -- computes moving averages
