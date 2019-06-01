-- Converts a list of minimum sudokus to a PROLOG-friendly format.
-- With the minimum puzzles in the current directory, use prolog x to
--  convert x of those puzzles to the desired format.
-- If x == 0, all puzzles will be converted.

import Data.List
import Data.List.Split

toprolog x = do
    content <- readFile "minimum.txt"
    let ps = lines content
    let nps = length ps
    let os = if x == 0 then nps else x
    writeFile "out.txt" . unlines $ take os (map convert (zip [1..nps] ps))

convert :: (Int,String) -> String
convert (i,s) = prefix i . brackets . concat . commas $ map (brackets . format) chunks
    where   chunks = chunksOf 9 s
            commas = intersperse ","
            brackets s = "[" ++ s ++ "]"
            prefix i s = "minimum_puzzles(P, " ++ show i ++ ") :- P = " ++ s ++ "."

format :: String -> String
format s = intersperse ',' $ [if c == '0' then '_' else c | c <- s]