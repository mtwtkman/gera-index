{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module TwitterTest (tests) where

import Data.Aeson
import qualified Data.ByteString.Lazy as L
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
specs = testGroup "Specs" $ map unsafePerformIO (spec_sep ++ spec_splitLine ++ spec_TweetJsonParser ++ spec_TweetsJsonParser)

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

spec_TweetJsonParser :: [IO TestTree]
spec_TweetJsonParser =
  [ HS.testSpec "Tweet json parser" $ do
      describe "parses" $ do
        it "correctly" $ do
          j <- L.readFile "test/twitter-api-data/tweet/with_gera_link.json"
          (decode j :: Maybe Tweet) `shouldBe` Just (Tweet "1503325072974663680" ["LLR", "ダブバイ"] "https://radio.gera.fan/jWy1")
        it "reject by no gera link" $ do
          j <- L.readFile "test/twitter-api-data/tweet/no_gera_link.json"
          (decode j :: Maybe Tweet) `shouldBe` Nothing
  ]

spec_TweetsJsonParser :: [IO TestTree]
spec_TweetsJsonParser =
  [ HS.testSpec "Tweets json parser" $ do
      it "can decode tweets api response correctly" $ do
        j <- L.readFile "test/twitter-api-data/tweets/correct.json"
        (decode j :: Maybe Tweets)
          `shouldBe` Just
            ( Tweets
                [ Tweet "1503325072974663680" ["LLR", "ダブバイ"] "https://radio.gera.fan/jWy1",
                  Tweet "1503325066389438465" ["錦鯉", "人生五十年"] "https://radio.gera.fan/A3do",
                  Tweet "1503325066376978433" ["モダンタイムス", "ブサイクラジオ"] "https://radio.gera.fan/TRXK",
                  Tweet "1503325063751348228" ["ゆったり感", "ヘル中"] "https://radio.gera.fan/iYmE"
                ]
            )
  ]
