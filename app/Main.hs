module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import qualified Gera as G
import qualified Twitter as Tw

data Item = Item
  { getId :: Tw.TweetId,
    getHashTags :: Tw.HashTags,
    getGeraItems :: [G.Gera]
  }
  deriving (Show, Eq)

aggregateTweets :: Tw.Datetime -> IO Tw.Tweets
aggregateTweets dt = do
  client <- Tw.fromDotEnv
  let sc = Tw.SearchCriteria 5 (Just dt) Nothing
  Tw.aggregate client sc

saveTweets :: FilePath -> Tw.Tweets -> IO ()
saveTweets path tweets =
  L.writeFile path (encode tweets)

tweetsResultFilePath :: FilePath
tweetsResultFilePath = "result/tweets.json"

buildItem :: Tw.Tweet -> [G.Gera] -> Item
buildItem tweet = Item (Tw.getId tweet) (Tw.getHashTags tweet)

main :: IO ()
main = do
  tweets <- aggregateTweets (Tw.Datetime 2022 4 1 0 0 0)
  saveTweets tweetsResultFilePath tweets
