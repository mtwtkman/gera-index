module Main where

import Gera
import System.IO.Unsafe
import Test.Hspec
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.Hspec as HS
import Test.Tasty.SmallCheck as SC

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, specs]

properties :: TestTree
properties =
  testGroup
    "Properties"
    [ SC.testProperty "write test" $ \x -> getPositive (x :: Positive Integer) > 0
    ]

specs :: TestTree
specs =
  testGroup "Specs" $
    map
      unsafePerformIO
      [ HS.testSpec "write test" $ do
          describe "write test" $ do
            it "write test now!" $ do
              "hoge" `shouldBe` "hoge"
      ]
