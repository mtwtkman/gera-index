module Main where

import qualified Twitter as Tw
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Aeson

aggregateTweets :: IO Tw.Tweets
aggregateTweets = do
  client <- Tw.fromDotEnv
  let sc = Tw.SearchCriteria 5 (Just (Tw.Datetime 2022 4 1 0 0 0)) Nothing
  Tw.aggregate client sc

saveTweets :: FilePath -> Tw.Tweets -> IO ()
saveTweets path tweets =
  L.writeFile path (encode tweets)

tweetsResultFilePath :: FilePath
tweetsResultFilePath = "result/tweets.json"

main :: IO ()
main = do
  tweets <- aggregateTweets
  saveTweets tweetsResultFilePath tweets
