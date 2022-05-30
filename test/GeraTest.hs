module GeraTest (tests) where

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Gera
import System.IO.Unsafe
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec as HS
import Text.HTML.TagSoup

tests = [specs]

specs :: TestTree
specs =
  testGroup "Specs" $
    map
      unsafePerformIO
      [ spec_findAudioUrl,
        spec_findEpisode,
        spec_findBroadcastDeadLine,
        spec_parsePage
      ]

readTestDataPage :: FilePath -> IO T.Text
readTestDataPage = T.readFile . (++) "test/gera-data/"

tagsFromTestData :: FilePath -> IO [Tag T.Text]
tagsFromTestData fname = do
  content <- readTestDataPage fname
  return $ parseTags content

spec_findAudioUrl :: IO TestTree
spec_findAudioUrl =
  HS.testSpec "findAudioUrl" $ do
    it "can find url from the audio tag" $ do
      tag <- tagsFromTestData "audio.html"
      findAudioUrl tag `shouldBe` Right "https://firebasestorage.googleapis.com/v0/b/gera-prd.appspot.com/o/episode-audios%2Ff5Zq7fsnkbFSIbfvEAmw.mp3?alt=media"

spec_findEpisode :: IO TestTree
spec_findEpisode =
  HS.testSpec "findEpisode" $ do
    it "finds episode number and title from the page" $ do
      tags <- tagsFromTestData "episode.html"
      findEpisode tags `shouldBe` Right (Episode "ギュネイ" 35)

spec_findBroadcastDeadLine :: IO TestTree
spec_findBroadcastDeadLine =
  HS.testSpec "findBroadcastDeadLine" $ do
    it "can find dead line as datetime" $ do
      tags <- tagsFromTestData "deadline.html"
      findBroadcastDeadLine tags `shouldBe` Just (Datetime 2022 9 14 12 0)
    describe "can find blank deadline as nothing" $ do
      it "because no deadline" $ do
        tags <- tagsFromTestData "nodeadline.html"
        findBroadcastDeadLine tags `shouldBe` Nothing
      it "because unexpected format" $ do
        tags <- tagsFromTestData "invalid_formatted_deadline.html"
        findBroadcastDeadLine tags `shouldBe` Nothing

spec_parsePage :: IO TestTree
spec_parsePage =
  HS.testSpec "parsePage" $ do
    it "can build a structure from a page" $ do
      content <- readTestDataPage "page.html"
      let parsed = parsePage content
          episode = Episode "ギュネイ" 35
          datetime = Datetime 2022 9 14 12 0
      parsed `shouldBe` Right (Gera episode "https://firebasestorage.googleapis.com/v0/b/gera-prd.appspot.com/o/episode-audios%2Ff5Zq7fsnkbFSIbfvEAmw.mp3?alt=media" (Just datetime))
    it "can build a struct from a page though no deadline" $ do
      content <- readTestDataPage "page_no_deadline.html"
      let parsed = parsePage content
          episode = Episode "ギュネイ" 35
      parsed `shouldBe` Right (Gera episode "https://firebasestorage.googleapis.com/v0/b/gera-prd.appspot.com/o/episode-audios%2Ff5Zq7fsnkbFSIbfvEAmw.mp3?alt=media" Nothing)
    it "fails building by unexpected contents" $ do
      content <- readTestDataPage "invalid_page.html"
      parsePage content `shouldBe` Left FailedParse
