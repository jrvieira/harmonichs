module Fibonacci (fibonacci) where

fibonacci :: Int -> Int
fibonacci n | n >= 0 = fst (fib n)

fib :: Int -> (Int, Int)
fib 0 = (0, 1)
fib n =
   let (a, b) = fib (div n 2)
       c = a * (b * 2 - a)
       d = a * a + b * b
   in if mod n 2 == 0
      then (c, d)
      else (d, c + d)
