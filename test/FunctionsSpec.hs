module FunctionsSpec where

import Test.Hspec
import Test.QuickCheck
import Functions


spec :: Spec
spec = describe "Analyse de la chaine de caractere donne en entree" $ do

  describe "test facile" $ do
    it "test 01" $ do
      stripChars " \t" "toto" `shouldBe` "toto"

    it "test 02" $ do
      stripChars " \t" "  toto" `shouldBe` "toto"
    
    it "test 03" $ do
      stripChars " \t" "\t toto  \to" `shouldBe` "totoo"

    it "test 04" $ do
      stripChars " \t" "\t\ttoto  " `shouldBe` "toto"

    it "test 05" $ do
      stripChars " \t" "  toto\t\t" `shouldBe` "toto"

  describe "test difficile" $ do
    it "test 06" $ do
      stripChars " \t" "1 + 2" `shouldBe` "1+2"

    it "test 07" $ do
      stripChars " \t" "1 + 2 - 3" `shouldBe` "1+2-3"

    it "test 08" $ do
      stripChars " \t" " 1 + 2 " `shouldBe` "1+2"

    it "test 09" $ do
      stripChars " \t" "\t1\t+ 2 + 3 + 4" `shouldBe` "1+2+3+4"

    it "test 10" $ do
      stripChars " \t" "1 -2 -4 +5" `shouldBe` "1-2-4+5"
      
