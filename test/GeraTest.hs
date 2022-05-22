{-# LANGUAGE OverloadedStrings #-}

module GeraTest (tests) where

import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy as L
import Gera
import System.IO.Unsafe
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec as HS

tests = [specs]

specs :: TestTree
specs =
  testGroup "Specs" $
    map
      unsafePerformIO
      [ spec_parsePage
      , spec_findAudioUrl
      ]

spec_findAudioUrl :: IO TestTree
spec_findAudioUrl =
  HS.testSpec "findAudioUrl" $ do
    it "can find url from an audio tag" $ do
      tag <- L.readFile "test/gera-data/audiotag.html"
      findAudioUrl (parseTags tag) `shouldBe` Right "https://firebasestorage.googleapis.com/v0/b/gera-prd.appspot.com/o/episode-audios%2Ff5Zq7fsnkbFSIbfvEAmw.mp3?alt=media"

spec_parsePage :: IO TestTree
spec_parsePage =
  HS.testSpec "parsePage" $ do
    it "can find audio source url" $ do
      content <- L.readFile "test/gera-data/page.html"
      parsePage content
        `shouldBe` Right
          ( Gera
              "ギュネイ"
              35
              "https://firebasestorage.googleapis.com/v0/b/gera-prd.appspot.com/o/episode-audios%2Ff5Zq7fsnkbFSIbfvEAmw.mp3?alt=media"
              (Just (Datetime 2022 9 14 12 0))
          )
