module TwitterTest (tests) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
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
        \y m d ->
          let yv = yearToInteger (y :: Year)
              mv = monthToInteger (m :: Month)
              dv = dayToInteger (d :: Day)
              date = Datetime yv mv dv
           in datetimeToFormattedString date == printf "%04d-%02d-%02dT00:00:00Z" yv mv dv
    ]

specs :: TestTree
specs =
  testGroup "Specs" $
    map
      unsafePerformIO
      [ spec_sep,
        spec_splitLine,
        spec_TweetJsonParser,
        spec_TweetsJsonParser,
        spec_stringToDatetime
      ]

spec_sep :: IO TestTree
spec_sep =
  HS.testSpec "sep" $ do
    it "separates specified character" $ do
      let s1 = "xxx"
          c = '='
          s2 = "yyy"
       in sep c (s1 ++ [c] ++ s2) `shouldBe` (s1, s2)

spec_splitLine :: IO TestTree
spec_splitLine =
  HS.testSpec "splitLine" $ do
    it "split by newline character" $ do
      let row1 = "hogehoge"
          row2 = "nekoneko"
       in splitLine (row1 ++ "\n" ++ row2) [] [] `shouldBe` [row1, row2]

spec_TweetJsonParser :: IO TestTree
spec_TweetJsonParser =
  HS.testSpec "Tweet json parser" $ do
    describe "parses" $ do
      it "correctly" $ do
        j <- C.readFile "test/twitter-api-data/tweet/with_gera_link.json"
        (decode j :: Maybe Tweet) `shouldBe` Just (Tweet "1503325072974663680" ["LLR", "ダブバイ"] (Just "https://radio.gera.fan/jWy1"))
      it "reject by no gera link" $ do
        j <- C.readFile "test/twitter-api-data/tweet/no_gera_link.json"
        (decode j :: Maybe Tweet) `shouldBe` Just (Tweet "1503339295327031296" ["声溜めラジオ", "ラランド18禁ライブ"] Nothing)

spec_TweetsJsonParser :: IO TestTree
spec_TweetsJsonParser =
  HS.testSpec "Tweets json parser can decode tweets api response" $ do
    it "has gera links absolutely" $ do
      doTest "test/twitter-api-data/tweets/correct.json"
    it "has no gera link" $ do
      doTest "test/twitter-api-data/tweets/all.json"
  where
    doTest :: String -> IO ()
    doTest testDataPath = do
      j <- C.readFile testDataPath
      (decode j :: Maybe Tweets)
        `shouldBe` Just
          ( Tweets
              [ Tweet "1503325072974663680" ["LLR", "ダブバイ"] (Just "https://radio.gera.fan/jWy1"),
                Tweet "1503325066389438465" ["錦鯉", "人生五十年"] (Just "https://radio.gera.fan/A3do"),
                Tweet "1503325066376978433" ["モダンタイムス", "ブサイクラジオ"] (Just "https://radio.gera.fan/TRXK"),
                Tweet "1503325063751348228" ["ゆったり感", "ヘル中"] (Just "https://radio.gera.fan/iYmE")
              ]
          )

spec_stringToDatetime :: IO TestTree
spec_stringToDatetime =
  HS.testSpec "stringToDatetime" $ do
    it "can parse from valid string" $ do
      let y = 2022
          m = 6
          d = 1
          s = C.concat $ map (C.pack . printf "%02d")  [y, m, d]
       in stringToDatetime s `shouldBe` Right (Datetime y m d)
    it "cannot parse from invalid string" $ do
      let s = "xxx"
       in stringToDatetime s `shouldBe` Left (MalformedDatetimeString InvalidDatetimeFormat)

