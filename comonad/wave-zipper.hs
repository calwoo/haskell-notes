{-# LANGUAGE DeriveFunctor #-}

import Control.Concurrent

data ListZipper a = LZip [a] a [a]
    deriving (Show, Functor)

shiftL :: ListZipper a -> ListZipper a
shiftL (LZip (l:ls) x rs) = LZip ls l (x:rs)
shiftL _ = error "invalid"

shiftR :: ListZipper a -> ListZipper a
shiftR (LZip ls x (r:rs)) = LZip (x:ls) r rs
shiftR _ = error "invalid"

readZip :: ListZipper a -> a
readZip (LZip _ x _) = x

-- convert to list zipper with padding (first argument)
toLZip :: a -> [a] -> ListZipper a
toLZip pad xs =
    case xs of
        []      -> LZip (repeat pad) pad (repeat pad)
        (x:xs') -> LZip (repeat pad) x (xs' ++ repeat pad)

-- from list zipper get back a range, where head is at index 0
(|>) :: a -> (a -> b) -> b
x |> f = f x -- I don't know why people don't use this more often in Haskell...

getInterval :: (Int, Int) -> ListZipper a -> [a]
getInterval (a,b) lz =
    lz |> centerZipper a
       |> iterate shiftR
       |> take (b-a+1)
       |> map readZip
    where centerZipper a lz
            | a < 0     = centerZipper (a+1) (shiftL lz)
            | a > 0     = centerZipper (a-1) (shiftR lz)
            | otherwise = lz

rule :: ListZipper Char -> Char
rule (LZip (l:_) _ (r:_))
    | fromL && fromR = 'X'
    | fromL = '>'
    | fromR = '<'
    | otherwise = ' '
    where
        fromL = l `elem` ">*X"
        fromR = r `elem` "<*X"
    
propagate :: ListZipper Char -> ListZipper Char
propagate lz = LZip (pgate shiftL lz) (rule lz) (pgate shiftR lz)
    where
        pgate f lz =
            lz |> iterate f
               |> tail
               |> map rule

main :: IO ()
main = mapM_ putStrLn $
            map (getInterval (-20,40)) $
            take 20 (iterate propagate $ toLZip ' ' initialWorld)
            where initialWorld = "*  >  *   *  <  **<"

loop :: ListZipper Char -> IO ()
loop world = do
    putStr "\ESC[2J"
    putStrLn $ nextWorld
    threadDelay 200000
    loop (propagate world)
    where
        nextWorld =
            world |> propagate
                  |> getInterval (-20,40)