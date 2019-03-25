-- really basic probabilistic programming interpreter

import Data.List hiding (empty, insert, map)
import Data.HashMap.Strict
import Control.Monad
import System.Random.MWC as MWC
import System.Random.MWC.Distributions as MD

type Name = String
type Env = HashMap String Val

-- values
data Val = D Double        -- doubles
         | B Bool          -- bools
         | F (Val -> Val)  -- functions
         | P Val Val       -- pairs

instance Eq Val where
    D x == D y         = x == y
    B x == B y         = x == y
    P x1 x2 == P y1 y2 = x1 == y1 && x2 == y2
    _ == _             = False

instance Ord Val where
    D x <= D y         = x <= y
    B x <= B y         = x <= y
    P x1 x2 <= P y1 y2 = x1 <= y1 && x2 <= y2
    _ <= _             = error "doesn't make sense"

-- expressions
data Expr = Lit Double
          | Var Name
          | Pair Expr Expr
          | Fst Expr
          | Snd Expr
          | If Expr Expr Expr
          -- comparison ops
          | Eql Expr Expr
          | Les Expr Expr
          | Gre Expr Expr
          | And Expr Expr
          -- function lambdas
          | Lam Name Expr
          | App Expr Expr
          -- arithmetic ops
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Eq, Show)

evalT :: Expr -> Env -> Val
evalT (Lit a) _ = D a
evalT (Var x) env = env ! x -- lookup
evalT (Pair e1 e2) env = P (evalT e1 env) (evalT e2 env)
evalT (Fst e) env = getFst $ evalT e env
    where getFst (P a _) = a
evalT (Snd e) env = getSnd $ evalT e env
    where getSnd (P _ b) = b
evalT (If cn th el) env = if (getBool $ evalT cn env)
                          then evalT th env
                          else evalT el env
                          where getBool (B b) = b
evalT (Eql x y) env = B $ (evalT x env) == (evalT y env)
evalT (Les x y) env = B $ (evalT x env) <= (evalT y env)
evalT (Gre x y) env = B $ (evalT x env) >= (evalT y env)
evalT (And x y) env = liftB (&&) (evalT x env) (evalT y env)
evalT (Add x y) env = liftOp (+) (evalT x env) (evalT y env)
evalT (Sub x y) env = liftOp (-) (evalT x env) (evalT y env)
evalT (Mul x y) env = liftOp (*) (evalT x env) (evalT y env)
evalT (Div x y) env = liftOp (/) (evalT x env) (evalT y env)

-- helpers
liftB :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftB op (B e1) (B e2) = B (op e1 e2)

liftOp :: (Double -> Double -> Double) -> Val -> Val -> Val
liftOp op (D e1) (D e2) = D (op e1 e2)

-- to make this a probabilistic programming language, we add measures
data Meas = Uniform Expr Expr
          | Weight Expr Expr
          | Bind Name Meas Meas
          deriving (Eq, Show)

dirac :: Expr -> Meas
dirac x = Weight (Lit 1.0) x

-- bind takes a measure as input and a function that draws in that measure into
-- another measure

-- example)
prog1 = Bind "x" (Uniform (Lit 1) (Lit 5))  -- x ~ Uniform(1,5)
        (dirac (Add (Var "x") (Var "x")))   -- return (x + x)

-- measures are evaluated by producing a weighted sample from the measure space they represent

evalM :: Meas -> Env -> MWC.GenIO -> IO (Val, Double)
evalM (Uniform lo hi) env gen =
    do
        let D lo' = evalT lo env
        let D hi' = evalT hi env
        x <- MWC.uniformR (lo', hi') gen
        return (D x, 1.0)
evalM (Weight i x) env gen =
    do
        let D i' = evalT i env
        return (evalT x env, i')
evalM (Bind x m f) env gen =
    do
        (x', w) <- evalM m env gen
        let env' = insert x x' env
        (f', w1) <- evalM f env' gen
        return (f', w * w1)

test1 :: IO ()
test1 = do
    gen <- MWC.create
    draw <- evalM prog1 empty gen
    printVal draw

printVal :: (Val, Double) -> IO ()
printVal (D d, d') = putStrLn $ show d ++ ", " ++ show d'

-- how do we represent conditionals?

data Cond = UCond Meas
          | UniformC Expr Expr Expr
          | WeightC Expr Expr Expr
          | BindC Name Cond Cond

-- to draw from a conditioned measure, we convert it to a unconditional measure

evalC :: Cond -> Meas
evalC (UCond m) = m
evalC (UniformC lo hi x) = Weight (If
                            (And (Gre x lo) (Les x hi))
                            (Div x (Sub hi lo))
                            (Lit 0)) x
evalC (WeightC i x y) = Weight (If (Eql x y) i (Lit 0)) y
evalC (BindC x m f) = Bind x (evalC m) (evalC f)

prog2 = BindC "x" (UCond (Uniform (Lit 1) (Lit 5)))      -- x <~ uniform(1, 5)
         (BindC "_" (UniformC (Var "x") (Lit 7) (Lit 3)) -- y <~ uniform(x, 7)
                                                         -- observe y 3
          (UCond (dirac (Var "x"))))                     -- return x

