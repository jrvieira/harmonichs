import System.IO
import System.Environment
import Data.Numbers.Primes
import Fibonacci
import System.Directory
import Codec.Picture
import Color

import Debug.Trace

{- gradient:

0 255 255 --> 0 0 255 --> 0 0 0 --> 255 0 0 --> 255 255 0
-510        -255          0               255           510
0           255           510             765           1020

/\/\/\/\/
a perfect saw period max resolution at 2040 pixels

rgb(0,255,255)
rgb(0,0,255)
rgb(0,0,0)
rgb(255,0,0)
rgb(255,255,0)

-}

-- dimensions

type Dimensions = (Int,Int)

-- named drawing functions

type F = Int -> Int

data Function = Function { 
    name :: String,
    apply :: F
}

-- functions

f_fibonacci :: Function
f_fibonacci = Function "fibonacci" fibonacci

f_primes :: Function
f_primes = Function "primes" f where
    f n = primes !! n

f_chromaticornth :: Function
f_chromaticornth = Function "chromaticornth" f where
    f n = let sub = 9 in n * 2 ^ (1 / sub)

f_octave :: Function
f_octave = Function "octave" f where
    f n = n * 2

f_graph :: Function
f_graph = Function "graph" f where
    f n = (n * (n-1)) / 2

f_wholefractions :: Function
f_wholefractions = Function "wholefractions" f where
    f n = 1 / n

f_wholefractionsreverse :: Function
f_wholefractionsreverse = Function "wholefractionsreverse" f where
    f n = 1 - 1 / n

f_even :: Function
f_even = Function "even" f where
    f n = 2 * n

f_odd :: Function
f_odd = Function "odd" f where
    f n = 2 * n-1

f_whole :: Function
f_whole = Function "whole" f where
    f n = n / 2


functions :: [Function]
functions = [
    f_fibonacci,
    f_primes,
    f_chromaticornth,
    f_octave,
    f_graph,
    f_wholefractions,
    f_wholefractionsreverse,
    f_even,
    f_odd,
    f_whole
  ]

-- convert to pixel

rgb :: Int -> PixelRGB8
-- i is always 0 ??
rgb i = if i == 0 then let ni = -i in
      PixelRGB8 0 0 255--0 (max 0 ni-255) (min 255 ni)
    else
      PixelRGB8 255 0 0--(min 255 i) (max 0 i-255) 0

-- calculate sines

sine :: Dimensions -> Function -> Int -> Int -> PixelRGB8
sine d f x y' = trace ("rgb $ " ++ "2 * " ++ show (y / w) ++ " * " ++ show (apply f y) ++ " = " ++ show r) $ rgb i
    where
      y = y'+1
      (w,h) = d
      r = 2 * (y / w) * (apply f y)
      i = round $ 510 * (sin (pi * fromIntegral r)) -- there are 1020 possible RGB colors in our spectrum: [-510..510]

-- draw

draw :: Dimensions -> Function -> IO()
draw d f = do
    createDirectoryIfMissing True "io"
    done <- doesFileExist file
    if done then
        putClrLn W (file ++ " skipped") -- print existing file to console
    else do
        savePngImage file (ImageRGB8 (generateImage (sine d f) w h))
        putClrLn G file -- print drawn file to console
    where
        (w,h) = d
        file = "io/" ++ show w ++ "x" ++ show h ++ "pixel_" ++ name f ++ ".png"

draw_list :: Dimensions -> [Function] -> IO ()
draw_list d list = do
   mapM_ (draw d) functions

main :: IO ()
main = do
   args <- getArgs
   if length args == 2 then let
      (w:h:_) = map read args
      in
      draw_list (w,h) functions
   else do
      putClr R "args:"
      putClrLn W "width height"
      pure ()
    
