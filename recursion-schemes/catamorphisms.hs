-- Lets define a simple syntax tree.

{-# LANGUAGE DeriveFunctor #-}

data Expr a
    = Literal { intVal :: Int }
    | Name    { name :: String }
    | Index   { target :: a, idx :: a }
    | Unary   { op :: String, target :: a }
    | Binary  { lhs :: a, op :: String, rhs :: a }
    | Call    { func :: a, args :: [a] }
    | Paren   { target :: a }
    deriving (Show, Eq, Functor)

-- To define infinitely-nested codata structures we use fixed points of functors

newtype Term f = In { out :: f (Term f) }

-- arrows
(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
(f >>> g) x = g (f x)

(<<<) :: (b -> c) -> (a -> b) -> (a -> c)
(<<<) = (.)

-- We defined a recursion scheme before that takes a Term and returns a Term. However,
-- it is limited by that return type.

bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp fn =
    out                     -- unpack
    >>> fmap (bottomUp fn)  -- recurse
    >>> In                  -- repack
    >>> fn                   -- apply

-- What if we just wanted to return a raw `a` value for further processing? Lets remove the
-- repacking step in the above traversal scheme.

newScheme fn =
    out                     -- unpack
    >>> fmap (newScheme fn)  -- recurse
    >>> fn                   -- apply

-- this typechecks as Functor f => (f b -> b) -> Term f -> b
-- That f b -> b looks familiar categorically. It's an algebra for the functor f.

type Algebra f a = f a -> a

-- example) node counting. An algebra can represent an computaiton we are trying to do from
-- the "inside-out".

countNodes :: Algebra Expr Int
countNodes (Literal _) = 1
countNodes (Name _) = 1
countNodes (Unary _ arg) = arg + 1
countNodes (Binary left _ right) = left + right + 1
countNodes (Call fn args) = fn + sum args + 1
countNodes (Index it idx) = it + idx + 1
countNodes (Paren arg) = arg + 1

-------------------
-- Catamorphisms --
-------------------

newScheme :: (Functor f) => Algebra f a -> Term f -> a

-- Our newScheme function is a catamorphism, where the greek cata- means "collapse".

cata :: Functor f => Algebra f a -> Term f -> a
cata fn = out >>> fmap (cata fn) >>> fn

-- A catamorphism uses an f-algebra to collapse an f-container of values into one value.

-- The stereotypical catamorphism is a right fold. To see this, note that the list type [a]
-- is the fixed point of the functor f b = Either () (a, b). An f-algebra then is a function
-- Either () (a, b) -> b, which by universal property of coproduct is a pair of maps () -> b
-- and (a, b) -> b. But this is exactly the data needed in a right fold!

-- Thus, catamorphisms can be thought of as "generalized folds".

bottomUp' fn = cata (In >>> fn)



