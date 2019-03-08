-------------------
-- Hylomorphisms --
-------------------

{-# LANGUAGE DeriveFunctor #-}

import Data.Char

-- A hylomorphism is an anamorphism followed by a catamorphism. The idea is that data is generated
-- by a seed value via an anamorphism, and then collapsed and folded into a value via a catamorphism.

data Term f = In { out :: f (Term f) }

-- arrows
(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
(f >>> g) x = g (f x)

(<<<) :: (b -> c) -> (a -> b) -> (a -> c)
(<<<) = (.)

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Term f -> a
cata fn = out >>> fmap (cata fn) >>> fn

type Coalgebra f a = a -> f a

ana :: Functor f => Coalgebra f a -> a -> Term f
ana fn = In <<< fmap (ana fn) <<< fn

-- Then a hylomorphism is

hylo :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo alg coalg = ana coalg >>> cata alg

-- Hylo comes from the greek hyle, which means matter.

-- example) reverse polish notation calculator

-- To do this as a hylomorphism, we need a coalgebra and an algebra. Our coalgebra will unfold a
-- list of numbers and symbols from a seed (string) and the algebra will consume the list, producing
-- a result value.

data Token
    = Lit Int
    | Op (Int -> Int -> Int)

parse :: String -> Token
parse "+" = Op (+)
parse "-" = Op (-)
parse "*" = Op (*)
parse "/" = Op div
parse num = Lit $ read num 

-- We represent our stack by a list. Since we need to write our lists as a fixed point of some functor,
-- we use the functor given by f b = Either () (a,b)

data List a b = Nil | Cons a b
    deriving (Show, Eq, Functor)

parseRPN :: Coalgebra (List Token) String
parseRPN "" = Nil
parseRPN str = Cons token next
    where
        (x, rest) = span (not. isSpace) str
        token = parse x
        next = dropWhile isSpace rest

-- Now we need our catamorphism. We will return a "stack of ints"

type Stack = [Int]

-- Note: we return Stack -> Stack as catamorphisms are "right folds" but for a reverse polish notation
-- calculator, we must evaluate from left-to-right. Hence we reverse the flow of computation using
-- the trick of the difference list: turn the stack operation into function composition! Now that is
-- left associative.

evalRPN :: Algebra (List Token) (Stack -> Stack)
evalRPN Nil stack                      = stack
evalRPN (Cons (Lit i) cont) stack      = cont (i : stack)
evalRPN (Cons (Op fn) cont) (a:b:rest) = cont (fn b a : rest)
evalRPN _ stack                        = error ("too few arguments on stack: " ++ show stack)

-- Now we can pass these two into our hylomorphism to get our calculator!

rpn :: String -> Stack
rpn s = hylo evalRPN parseRPN s []