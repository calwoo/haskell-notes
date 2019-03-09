-- Zippers

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

