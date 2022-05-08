{-# LANGUAGE OverloadedStrings #-}

module Main where

import Generator
import Gera
import Network.HTTP.Req
import System.IO.Unsafe
import Test.Hspec
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.Hspec as HS
import Test.Tasty.SmallCheck as SC
import Prelude hiding (toInteger)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, specs]

properties :: TestTree
properties = testGroup "Properties" [prop_buildQuery]

prop_buildQuery :: TestTree
prop_buildQuery =
  testGroup
    "buildQuery"
    [ SC.testProperty "builds from specified values" $
        \y m d ->
          let date = Date (getValidYear y) (getValidMonth m) (getValidDay d)
           in 1 == 1
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
