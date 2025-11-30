module Main where

import Lib (someFunc)

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    someFunc
