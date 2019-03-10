-- Wave propagation

import Data.List (tails)
import Control.Concurrent

type World a = [a] -- 1D world

-- propagation rule
rule :: Char -> Char -> Char
rule l r
    | fromL && fromR = 'X'
    | fromL = '>'
    | fromR = '<'
    | otherwise = ' '
    where
        fromL = l `elem` ">*X"
        fromR = r `elem` "<*X"

propagate :: World Char -> World Char
propagate world = 
    take (length world)
    . map (\(l:x:r:_) -> rule l r) $ tails world'
    where world' = " " ++ world ++ " "

main :: IO ()
main = loop propagate start
    where start = "*  >  *   *  <  **<"

loop :: (World Char -> World Char) -> World Char -> IO ()
loop step world = do
    putStr "\ESC[2J" -- clears terminal
    putStrLn (propagate world)
    threadDelay 200000
    loop step (step world)