-----------------------
-- Recursion schemes --
-----------------------

{-# LANGUAGE DeriveFunctor, TypeApplications #-}

-- Recursion schemes are design idioms for traversing through recursive data types.
-- They were brought into light by the paper of Meijer et. al "Functional programming
-- with bananas, lenses, envelopes and barbed wire." They connect recursion schemes to
-- constructs in category theory, bringing a theoretical framework for recursive data.

-- Lets start by looking at simple syntax trees

data Lit
    = SLit String
    | ILit Int
    | Name String
    deriving (Show, Eq)

data Expr
    = Index Expr Expr
    | Call Expr [Expr]
    | Unary String Expr
    | Binary Expr String Expr
    | Paren Expr
    | Literal Lit
    deriving (Show, Eq)

data Stmt
    = Break
    | Continue
    | Empty
    | IfElse Expr [Stmt] [Stmt]
    | Return (Maybe Expr)
    | While Expr [Stmt]
    | Expression Expr
    deriving (Show, Eq)

-- Although this is simple, functions that operate on expressions are annoying.

flatten :: Expr -> Expr -- recursively removes parentheses
flatten (Literal i) = Literal i -- base case
flatten (Paren e) = flatten e -- recursion step
flatten (Index e i) = Index (flatten e) (flatten i)
flatten (Call e args) = Call (flatten e) (map flatten args)
flatten (Unary op arg) = Unary op (flatten arg)
flatten (Binary l op r) = Binary (flatten l) op (flatten r)

-- This works, but it's ugly. Most of the function code is dedicated to ensuring flatten
-- is recursed into subexpressions. This explicit recursion makes it hard to refactor code
-- when we need to.

-- One way to alleviate this is to shift all the recursive function application to a generic.

applyExpr :: (Expr -> Expr) -> Expr -> Expr
applyExpr f (Literal i) = Literal i -- base case
applyExpr f (Paren p) = Paren (f p) -- all others are recursion step
applyExpr f (Index e i) = Index (f e) (f i)
applyExpr f (Call e args) = Call (f e) (map f args)
applyExpr f (Unary op arg) = Unary op (f arg)
applyExpr f (Binary l op r) = Binary (f l) op (f r)

flatten' :: Expr -> Expr
flatten' (Paren e) = flatten' e -- special base case
flatten' e = applyExpr flatten' e

-- Better, but now our boilerplate just changed. We haven't gotten rid of it!

-- To see more general patterns, lets generalize our type by parameterization.
data ExprF a
    = IndexF a a
    | CallF a [a]
    | UnaryF String a
    | BinaryF a String a
    | ParenF a
    | LiteralF Lit
    deriving (Show, Eq, Functor)

-- But now we have a restriction-- the typevar a prevents us from having infinite-depth
-- recursive trees, and that to establish the depth of a tree we terminate with a Lit.
-- i.e
--   ExprF Lit ~ expressions with no subexpressions
--   ExprF (ExprF Lit) ~ expressions of max depth 1
--   ExprF (ExprF (ExprF Lit)) ~ expressions of max depth 2 ...

-- To get back our Expr type, we need something like
-- type ArbExpr = ExprF (ExprF (ExprF (ExprF ...)))

-- This sorta looks like the Y-combinator! y f = f (f (f (f ...))).. but we need it at the
-- type-level. The above can be written y f = f (y f), i.e y f is a fixed-point of f.

-- We are looking at a Y ExprF = ExprF (Y ExprF), ie a fixed-point of the ExprF functor.
-- In usual Haskell, we just call Y = Fix, but now we'll call it Term.

data Term f = In { out :: f (Term f) } -- codata

type Expr' = Term ExprF -- so now we know what the F stands for... finite!

-- Now onto some traversals. How do we traverse the codata represented by fixed-points of functors?

(>>>) :: (a -> b) -> (b -> c) -> (a -> c) -- arrow notation, also in Control.Arrow
(f >>> g) x = g (f x)

-- traverses through structure and applies function from the inside-out
bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp fn =
    out                     -- unpack
    >>> fmap (bottomUp fn)  -- recurse
    >>> In                  -- repack
    >> fn                   -- apply

-- This is our first recursion scheme! This is a type-generic combinator for recursively
-- traversing ANY fixed-point of a functor.

flattenTerm :: Expr' -> Expr' -- remember that Expr' = Term ExprT
flattenTerm (In (ParenF e)) = e -- base case: remove parens
flattenTerm other = other -- do nothing otherwise

flatten'' :: Expr' -> Expr'
flatten'' = bottomUp flattenTerm

-- Can we traverse the other direction? In the top-down traversal, we apply fn first,
-- and then we move into the subexpressions.

(<<<) :: (b -> c) -> (a -> b) -> (a -> c)
(<<<) = (.)

topDown :: Functor f => (Term f -> Term f) -> Term f -> Term f
topDown f = In <<< fmap (topDown f) <<< out <<< f
-- vs bottomUp f = out >>> (bottomUp f) >>> In >>> f

-- These are the simplest recursion schemes, coming from representing an infinitely-
-- recursive codata structure as the fixed-point of a functor.