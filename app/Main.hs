import System.IO
import System.Environment
import Data.Numbers.Primes
import Fibonacci
import System.Directory
import Codec.Picture
import Color

--import Debug.Trace

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

-- named functions

data Sequence = Sequence { 
    name :: String,
    nth :: Int -> Double
}

-- test

_test :: Sequence
_test = Sequence "test" f where
  f n = 1

-- sequences

_harmonic :: Sequence
_harmonic = Sequence "harmonic" f where
  f n = fromIntegral n

_fibonacci :: Sequence
_fibonacci = Sequence "fibonacci" f where
  f n = fromIntegral $ fibonacci n

_primes :: Sequence
_primes = Sequence "primes" f where
  f n = fromIntegral $ primes !! n

_chromatic_or_nth :: Sequence
_chromatic_or_nth = Sequence "chromatic_or_nth" f where
  f n = fromIntegral n * 2 ** (1 / sub)
  sub = 9

_octave :: Sequence
_octave = Sequence "octave" f where
  f n = fromIntegral n * 2

_graph :: Sequence
_graph = Sequence "graph" f where
  f n = (fromIntegral n * (fromIntegral n - 1)) / 2

_wholefractions :: Sequence
_wholefractions = Sequence "wholefractions" f where
  f n = 1 / (fromIntegral n)

_wholefractionsreverse :: Sequence
_wholefractionsreverse = Sequence "wholefractionsreverse" f where
  f n = 1 - 1 / (fromIntegral n)

_even :: Sequence
_even = Sequence "even" f where
  f n = fromIntegral $ 2 * n

_odd :: Sequence
_odd = Sequence "odd" f where
  f n = fromIntegral $ 2 * n-1

_whole :: Sequence
_whole = Sequence "whole" f where
  f n = fromIntegral n / 2


sequences :: [Sequence]
sequences = [ _test
  , _harmonic
  , _fibonacci
  , _primes
  , _chromatic_or_nth
  , _octave
  , _graph
  , _wholefractions
  , _wholefractionsreverse
  , _even
  , _odd
  , _whole

  ]

-- turn signal into pixel

pixelize :: Double -> PixelRGB8
pixelize signal = PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)
  where

    -- COMPLEX:
    -- light:
    --r = 255 - (max 0 (min 255 ni))
    --g = 255 - (max 0 ((max i ni) - 255))
    --b = 255 - (max 0 (min 255 i))
    -- dark:
    --r = max 0 (min 255 i)
    --g = max 0 ((max i ni) - 255)
    --b = max 0 (min 255 ni)

    --i = round $ signal * 510

    -- SIMPLE M/C:
    r = 255 - (max 0 i)
    g = 255 - (max 0 ni)
    b = 255

    i = round $ signal * 255

    ni = -i

-- calculate sines

sine :: Dimensions -> Sequence -> Int -> Int -> PixelRGB8
sine d s x y = pixelize signal
    where
      w = fromIntegral $ fst d
      p = fromIntegral x + 1
      n = nth s y
      signal = sin $ (p / w) * 2 * pi * n -- from [-1..1]

-- draw

draw :: Dimensions -> Sequence -> IO()
draw d s = do
  createDirectoryIfMissing True "io"
  done <- doesFileExist file
  if done then
    putClrLn W (file ++ " skipped") -- print existing file to console
  else do
    savePngImage file (ImageRGB8 (generateImage (sine d s) w h))
    putClrLn G file -- print drawn file to console
  where
    (w,h) = d
    file = "io/" ++ show w ++ "x" ++ show h ++ "pixel_" ++ name s ++ ".png"

draw_list :: Dimensions -> [Sequence] -> IO ()
draw_list d list = do
  mapM_ (draw d) sequences

main :: IO ()
main = do
  args <- getArgs
  if length args == 2 then let
    (w:h:_) = map read args
    in
    draw_list (w,h) sequences
  else do
    putClr R "args:"
    putClrLn W "width height"
    pure ()

