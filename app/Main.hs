module Main where

import Functions
import System.Environment
import Expr

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] ->  putStrLn "No agrs given"
    [s]-> printResult (read (arithmetic (stripChars " \t" (args!!0))) :: Double)  
