-- Zippers

import Control.Monad

-- A zipper for lists is a "cursor" and 2 lists: left and right
data ListZipper a = LZip [a] a [a]

-- Lets assume our lists are infinite, so we can move our cursor left or right
-- without fail.

listLeft :: ListZipper a -> ListZipper a
listLeft (LZip (l:ls) curs rs) = LZip ls l (curs:rs)
listLeft _ = error "can't go left"

listRight :: ListZipper a -> ListZipper a
listRight (LZip ls curs (r:rs)) = LZip (curs:ls) r rs
listRight _ = error "can't go right"

listRead :: ListZipper a -> a
listRead (LZip _ curs _) = curs

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZip ls _ rs) = LZip ls x rs

instance Functor ListZipper where
    fmap f (LZip ls curs rs) = LZip (map f ls) (f curs) (map f rs)

-- The list zipper is a comonad!

class Functor w => Comonad w where
    extract :: w a -> a
    extend :: (w a -> b) -> w a -> w b
    duplicate :: w a -> w (w a)

instance Comonad ListZipper where
    -- extract picks out the element at the cursor
    extract = listRead
    -- duplicate returns a list zipper of list zippers, where each position contains the list
    -- zipper of the original, shifted by the appropriate amount
    duplicate lzip = LZip (iterate' listLeft lzip) lzip (iterate' listRight lzip)
    -- extend can be written in terms of duplicate
    extend f = fmap f . duplicate

iterate' :: (a -> a) -> a -> [a]
iterate' f = tail . iterate f

-- Let's generalize for a zipper on planes of cells.

data PlaneZipper a = PZip (ListZipper (ListZipper a))

-- moves

up :: PlaneZipper a -> PlaneZipper a
up (PZip z) = PZip (listLeft z)

down :: PlaneZipper a -> PlaneZipper a
down (PZip z) = PZip (listRight z)

left :: PlaneZipper a -> PlaneZipper a
left (PZip z) = PZip (fmap listLeft z)

right :: PlaneZipper a -> PlaneZipper a
right (PZip z) = PZip (fmap listRight z)

planeRead :: PlaneZipper a -> a
planeRead (PZip z) = listRead (listRead z)

planeWrite :: a -> PlaneZipper a -> PlaneZipper a
planeWrite x (PZip z) = PZip (listWrite newLine z)
    where newLine = listWrite x (listRead z)

-- As before, plane zippers are comonads.

instance Functor PlaneZipper where
    fmap f (PZip z) = PZip (fmap (fmap f) z)

-- The comonad instance is similar to that of lists: for each possible motion, we
-- express the plane zipper that gives that motion

horizontal :: PlaneZipper a -> ListZipper (PlaneZipper a)
horizontal z = LZip (iterate' left z) z (iterate' right z)

vertical :: PlaneZipper a -> ListZipper (PlaneZipper a)
vertical z = LZip (iterate' up z) z (iterate' down z)

instance Comonad PlaneZipper where
    extract = planeRead
    duplicate z = PZip $ fmap horizontal $ vertical z
    extend f = fmap f . duplicate

----------------------------
-- Comonadic game of life --
----------------------------

neighbors :: [PlaneZipper a -> PlaneZipper a]
neighbors =
    horiz ++ vert ++ liftM2 (.) horiz vert
    where horiz = [left, right]
          vert  = [up, down]
        
count :: [Bool] -> Int
count = length . filter (==True)

aliveNeighbors :: PlaneZipper Bool -> Int
aliveNeighbors z = count $ map (\f -> extract (f z)) neighbors

-- Conway's Game of Life rules:
-- 1) if 2 neighbors are alive, return previous state
-- 2) if 3 neighbors are alive, a new cell is born
-- 3) any other count causes cell to die
rule :: PlaneZipper Bool -> Bool
rule z =
    case aliveNeighbors z of
        2 -> extract z
        3 -> True    -- born!
        _ -> False   -- dies.

-- COMONADICITY

evolve :: PlaneZipper Bool -> PlaneZipper Bool
evolve = extend rule -- local to global!

