-- Algebraic data types

-- 0
data Void

-- 1
-- data () = ()

-- 2
-- data Bool = True | False

-- isomorphisms
data Spin = Up | Down

boolToSpin1 :: Bool -> Spin
boolToSpin1 False = Up
boolToSpin1 True  = Down

spinToBool1 :: Spin -> Bool
spinToBool1 Up   = False
spinToBool1 Down = True

-- ... but there's another iso
boolToSpin2 :: Bool -> Spin
boolToSpin2 False = Down
boolToSpin2 True  = Up

spinToBool2 :: Spin -> Bool
spinToBool2 Up   = True
spinToBool2 Down = False

-- does which iso we use matter? nah. get over it.

-- sum type: Either a b ~> a + b
-- here's a + b + 2
data Deal a b
    = This a
    | That b
    | TheOther Bool

-- Maybe a ~> 1 + a

-- product type: (a, b) ~> a * b

-- function type: a -> b ~> b^a

-- The algebra of types is pretty cool. Let's try it out with a tic-tac-toe game.
-- naive implementation:

data TicTacToe a = TicTacToe
    { topLeft   :: a
    , topCenter :: a
    , topRight  :: a
    , midLeft   :: a
    , midCenter :: a
    , midRight  :: a
    , botLeft   :: a
    , botCenter :: a
    , botRight  :: a }

emptyBoard :: TicTacToe (Maybe Bool)
emptyBoard =
    TicTacToe
        Nothing Nothing Nothing
        Nothing Nothing Nothing
        Nothing Nothing Nothing

-- horrible.
-- better implementation: note that TicTacToe a ~> a^(3*3), so is isomorphic to a function
-- (Three, Three) -> a

data Three = One | Two | Three
    deriving (Eq, Ord, Enum, Bounded)

data TicTacToe2 a = TicTacToe2 { board :: Three -> Three -> a }

emptyBoard2 :: TicTacToe2 (Maybe Bool)
emptyBoard2 = TicTacToe2 $ const $ const Nothing

-- the above algebraic relations are simple cases of the far-reaching curry-howard isomorphism
-- between types, logic, mathematics, etc.

