module GeraTest (tests) where

import qualified Data.ByteString.Lazy.Char8 as C
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
      [ spec_parsePage,
        spec_findAudioUrl,
        spec_findEpisode
      ]

readTestDataPage :: FilePath -> IO C.ByteString
readTestDataPage = C.readFile . (++) "test/gera-data/"

tagsFromTestData :: FilePath -> IO [Tag C.ByteString]
tagsFromTestData fname = do
  content <- readTestDataPage fname
  return $ parseTags content

spec_findAudioUrl :: IO TestTree
spec_findAudioUrl =
  HS.testSpec "findAudioUrl" $ do
    it "can find url from the audio tag" $ do
      tag <- tagsFromTestData "audiotag.html"
      findAudioUrl tag `shouldBe` Right "https://firebasestorage.googleapis.com/v0/b/gera-prd.appspot.com/o/episode-audios%2Ff5Zq7fsnkbFSIbfvEAmw.mp3?alt=media"

spec_findEpisode :: IO TestTree
spec_findEpisode =
  HS.testSpec "findEpisode" $ do
    it "finds episode number and title from the page" $ do
      tags <- tagsFromTestData "episode.html"
      findEpisode tags `shouldBe` Right (Episode "ギュネイ" 35)

spec_findBroadCastDeadLine :: IO TestTree
spec_findBroadCastDeadLine =
  HS.testSpec "findBroadCastDeadLine" $ do
    it "can find dead line as datetime" $ do
      tags <- tagsFromTestData "page.html"
      findBroadCastDeadLine tags `shouldBe` Just (Datetime 2022 9 14 12 0)
    describe "can finds blank deadline as nothing" $ do
      it "because no deadline" $ do
        tags <- tagsFromTestData "nodeadline.html"
        findBroadCastDeadLine tags `shouldBe` Nothing
      it "because unexpected format" $ do
        tags <- tagsFromTestData "unepxecteddeadline.html"
        findBroadCastDeadLine tags `shouldBe` Nothing

spec_parsePage :: IO TestTree
spec_parsePage =
  HS.testSpec "parsePage" $ do
    xit "can find audio source url" $ do
      tags <- readTestDataPage "page.html"
      parsePage tags
        `shouldBe` Right
          ( Gera
              (Episode "ギュネイ" 35)
              "https://firebasestorage.googleapis.com/v0/b/gera-prd.appspot.com/o/episode-audios%2Ff5Zq7fsnkbFSIbfvEAmw.mp3?alt=media"
              (Just (Datetime 2022 9 14 12 0))
          )
