{-# LANGUAGE OverloadedStrings #-}

module TwitterTest (tests) where

import qualified Data.ByteString.Lazy as L
import Data.Aeson
import Generator
import System.IO.Unsafe
import Test.Hspec
import Test.SmallCheck
import Test.Tasty
import Test.Tasty.Hspec as HS
import Test.Tasty.SmallCheck as SC
import Text.Printf
import Twitter

tests = [specs, properties]

properties :: TestTree
properties = testGroup "Properties" [prop_datetimeToFormattedString]

prop_datetimeToFormattedString :: TestTree
prop_datetimeToFormattedString =
  testGroup
    "dateToFormattedString"
    [ SC.testProperty "transform itself by specified format" $
        \y m d h mi s ->
          let yv = yearToInteger (y :: Year)
              mv = monthToInteger (m :: Month)
              dv = dayToInteger (d :: Day)
              hv = hourToInteger (h :: Hour)
              miv = minuteToInteger (mi :: Minute)
              sv = secondToInteger (s :: Second)
              date = Datetime yv mv dv hv miv sv
           in datetimeToFormattedString date == printf "%04d-%02d-%02dT%02d:%02d:%02dZ" yv mv dv hv miv sv
    ]

specs :: TestTree
specs = testGroup "Specs" $ map unsafePerformIO (spec_sep ++ spec_splitLine)

spec_sep :: [IO TestTree]
spec_sep =
  [ HS.testSpec "sep" $ do
      it "separates specified character" $ do
        let s1 = "xxx"
            c = '='
            s2 = "yyy"
         in sep c (s1 ++ [c] ++ s2) `shouldBe` (s1, s2)
  ]

spec_splitLine :: [IO TestTree]
spec_splitLine =
  [ HS.testSpec "splitLine" $ do
      it "split by newline character" $ do
        let row1 = "hogehoge"
            row2 = "nekoneko"
         in splitLine (row1 ++ "\n" ++ row2) [] [] `shouldBe` [row1, row2]
  ]

spec_TweetsJsonParser :: [IO TestTree]
spec_TweetsJsonParser =
  [ HS.testSpec "Tweets json parser" $ do
      it "can decode tweets api response correctly" $ do
        j <- L.readFile "tweets.json"
        case decode j :: Maybe Tweets of
          Just tweets -> 1 `shouldBe` 1
          Nothing -> fail "error"
  ]
