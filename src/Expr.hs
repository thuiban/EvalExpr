module Expr where

import          Data.Char (isDigit, ord)
import          Data.String

type SyntaxError = String

data Expr = Val Double
          | Mul Expr Expr
          | Div Expr Expr
          | Plus Expr Expr
          | Sub Expr Expr
          | Pow Expr Expr
           deriving (Eq, Show)

arithmetic :: String -> String
arithmetic s =
  case exprParser s of
    Left er -> er
    Right ex -> show $ evalExpr ex

exprParser :: String -> Either SyntaxError Expr
exprParser input =
  case arithParser input of
    Left err -> Left err
    Right v  -> Right $ fst v


arithParser :: String -> Either SyntaxError (Expr, String)
arithParser cs =
  do (v1, cs') <- intParser cs
     case cs' of
       [] -> return (v1, [])
       _  -> do
         (plus, cs'') <- signParser cs'
         (v2, cs''')  <- arithParser cs''
         return (plus v1 v2, cs''')

intParser :: String -> Either SyntaxError (Expr, String)
intParser (c:rest)
  | isDigit c = intParser' (fromIntegral(ord c - ord '0')) rest
  | otherwise = Left ("syntax error in '" ++ (c:rest) ++ "', expected Expr digit")
  where
    intParser' :: Double -> String -> Either SyntaxError (Expr, String)
    intParser' n (c:cs)
      | isDigit c = intParser' (n * 10 + (fromIntegral(ord c - ord '0'))) cs
      | otherwise = Right (Val n, c:cs)
    intParser' n [] = Right (Val n, [])
intParser [] = Left ("syntax error in'" ++ [] ++ "', expected a digit")

signParser :: String -> Either SyntaxError (Expr -> Expr -> Expr, String)
signParser ('+':rest) = Right (Plus, rest)
signParser ('-':rest) = Right (Sub, rest)
signParser ('*':rest) = Right (Mul, rest)
signParser ('/':rest) = Right (Div, rest)
signParser ('^':rest) = Right (Pow, rest)
signParser s        = Left ("syntax error in '" ++ s ++  "', expected [+|-|*|/]")

evalExpr :: Expr -> Double
evalExpr (Plus v1 v2) = evalExpr v1 + evalExpr v2
evalExpr (Sub v1 v2)  = evalExpr v1 - evalExpr v2
evalExpr (Mul v1 v2)  = evalExpr v1 * evalExpr v2
evalExpr (Div v1 v2)  = evalExpr v1 / evalExpr v2
evalExpr (Pow v1 v2)  = evalExpr v1 `pow` evalExpr v2 
evalExpr (Val v1)     = v1


pow :: Double -> Double -> Double
pow base 0 = base
pow base pui  = pow2  base (pui - 1) base
        where
          pow2 :: Double -> Double -> Double -> Double
          pow2 base pui res =
            if pui > 0
               then pow2 base (pui - 1) (res*base)
               else res
