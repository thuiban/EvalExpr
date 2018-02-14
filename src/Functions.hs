module Functions where

import Text.Printf

printResult :: Double -> IO()
printResult s = 
        do  putStrLn $ printf "%.2f" s     
stripChars :: String -> String -> String
stripChars = filter . flip notElem
