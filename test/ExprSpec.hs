module ExprSpec where

import  Test.Hspec
import  Test.QuickCheck
import  Data.Either (isLeft)
import  Expr

spec :: Spec
spec = describe "Analyseur syntaxique d'additions à 1 chiffre" $ do

  describe "analyseur de plus" $ do
    it "parse '1+2'" $ do
      exprParser "1+2" `shouldBe` Right (Val 1 `Plus` Val 2)

    it "parse '2+1'" $ do
      exprParser "2+1" `shouldBe` Right (Val 2 `Plus` Val 1)

    it "parse 'a+1' est une erreur" $ do
      exprParser "a+1" `shouldSatisfy` isLeft

    it "parse '2+a' est une erreur" $ do
      exprParser "2+a" `shouldSatisfy` isLeft

    it "parse '2+1+3'" $ do
      exprParser "2+1+3" `shouldBe` Right (Val 2 `Plus` (Val 1 `Plus` Val 3))

    it "parse '2+1+' est une erreur" $ do
      exprParser "2+1+" `shouldSatisfy` isLeft

    it "parse '+' est une erreur" $ do
      exprParser "+" `shouldSatisfy` isLeft

  describe "analyseur de moins" $ do
   it "parse '1-2'" $ do
     exprParser "1-2" `shouldBe` Right (Val 1 `Sub` Val 2)

   it "parse '2-1'" $ do
     exprParser "2-1" `shouldBe` Right (Val 2 `Sub` Val 1)

   it "parse 'a-1' est une erreur" $ do
     exprParser "a-1" `shouldSatisfy` isLeft

   it "parse '2-a' est une erreur" $ do
     exprParser "2-a" `shouldSatisfy` isLeft

   it "parse '2-1+3'" $ do
     exprParser "2-1+3" `shouldBe` Right (Val 2 `Sub` (Val 1 `Plus` Val 3))

   it "parse '2+1-' est une erreur" $ do
     exprParser "2+1-" `shouldSatisfy` isLeft

   it "parse '-' est une erreur" $ do
     exprParser "-" `shouldSatisfy` isLeft
 
  describe "Parser de Int" $ do

    it "analyse un chiffre comme un entier" $
      property $ analyseSingleDigit

    it "analyse plusieurs chiffres comme un entier" $
      property $ analyseDigitString

    it "analyse un non-digit comme une syntaxerror" $
      intParser "a" `shouldSatisfy` isLeft

  describe "Parser de signe" $ do
    it "analyse le caractère '+' comme un 'plus'" $
      let Right (f,_) = signParser "+"
      in f (Val 1) (Val 2) `shouldBe` Plus (Val 1) (Val 2)

    it "analyse le caractère '-' comme moins" $
      let Right (e,_) = signParser "-"
      in e (Val 1) (Val 2) `shouldBe` Sub (Val 1) (Val 2)

    it "analyse le caractère '*' comme un 'multiplier'" $
      let Right (f,_) = signParser "*"
      in f (Val 1) (Val 2) `shouldBe` Mul (Val 1) (Val 2)

    it "analyse le caractère '/' comme un 'diviser'" $
      let Right (f,_) = signParser "/"
      in f (Val 1) (Val 2) `shouldBe` Div (Val 1) (Val 2)

    it "analyse le caractère 'a' comme une erreur" $
      let Left e = signParser "a"
      in True `shouldBe` True

  describe "Evaluateur arithmétique" $ do
    it "calcule '1+2' retourne 3" $
      evalExpr (Plus (Val 1) (Val 2)) `shouldBe` 3

    it "calcule '1+3' retourne 4" $
      evalExpr (Plus (Val 1) (Val 3)) `shouldBe` 4

    it "calcule '1+3+5' retourne 9" $
      evalExpr (Plus (Val 1) (Plus (Val 3) (Val 5))) `shouldBe` 9

    it "calcule '(1+3)+5' retourne 9" $
      evalExpr (Plus (Plus (Val 1) (Val 3)) (Val 5)) `shouldBe` 9

newtype Digit = Digit Char
  deriving (Eq, Show)

instance Arbitrary Digit where
  arbitrary = Digit <$> elements ['0'.. '9']

analyseSingleDigit :: Digit -> Bool
analyseSingleDigit (Digit c) =
  intParser [c] == Right (Val $ read [c], [])

newtype Digits = Digits String
  deriving (Eq, Show)

instance Arbitrary Digits where
  arbitrary = Digits <$> listOf1 (elements ['0'.. '9'])

analyseDigitString :: Digits -> Bool
analyseDigitString (Digits s) =
  intParser s == Right (Val $ read s, [])