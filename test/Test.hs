{-# LANGUAGE OverloadedStrings #-}
module Main where

import Gera
import Network.HTTP.Req
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
      [ HS.testSpec "constants" $ do
          describe "twitter search url" $ do
            it "is correct" $ do
              renderUrl twitterSearchUrl == "https://twitter.com/search"
      ]
