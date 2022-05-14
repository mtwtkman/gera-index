{-# LANGUAGE OverloadedStrings #-}

module GeraTest (tests) where

import Data.ByteString.Builder
import Generator
import Gera
import Network.HTTP.Req
import System.IO.Unsafe
import Test.Hspec
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.Hspec as HS
import Test.Tasty.SmallCheck as SC
import Text.Printf
import Prelude hiding (toInteger)

tests = [properties, specs]

properties :: TestTree
properties = testGroup "Properties" [prop_dateToFormattedString]

prop_dateToFormattedString :: TestTree
prop_dateToFormattedString =
  testGroup
    "dateToFormattedString"
    [ SC.testProperty "concates members with a hyphen and pads by zero" $
        \y m d ->
          let yv = getValidYear (y :: ValidYear)
              mv = getValidMonth (m :: ValidMonth)
              dv = getValidDay (d :: ValidDay)
              date = Date yv mv dv
           in dateToFormattedString date == printf "%04d-%02d-%02d" yv mv dv
    ]

specs :: TestTree
specs =
  testGroup "Specs" $
    map
      unsafePerformIO
      [ HS.testSpec "constants" $ do
          describe "twitter search url" $ do
            it "is correct" $ do
              renderUrl twitterSearchUrl `shouldBe` "https://twitter.com/search",
      ]
