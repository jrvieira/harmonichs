module Color
( Color (..)
, putClr
, putClrLn
) where

-- stdout colors

data Color = Reset | R | G | Y | B | M | C | W

code :: Color -> Int
code Reset = 0
code R = 31
code G = 32
code Y = 33
code B = 34
code M = 35
code C = 36
code W = 37

instance Show Color where
    show c = "\x1b[" ++ (show . code) c ++ "m"

type ColorOut = Color -> String -> IO () 

putClr :: ColorOut
putClr c s = putStr $ show c ++ s ++ show Reset ++ " "

putClrLn :: ColorOut
putClrLn c s = putStrLn $ show c ++ s ++ show Reset
