import Color

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

draw :: Dimensions -> IO()
draw d = do
    createDirectoryIfMissing True "io"
    done <- doesFileExist file
    if done then
        putClrLn W (file ++ " skipped") -- print existing file to console
    else do
        savePngImage file (ImageRGB8 (generateImage harmonic w h))
        putClrLn G file -- print drawn file to console
    where
        (w,h) = d
        file = "io/" ++ show w ++ "x" ++ show h ++ "pixel_harmonic.png"


main :: IO ()
main = do 
    
