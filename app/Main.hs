module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Gera as G
import Options.Applicative
import qualified Twitter as Tw
import Data.Maybe

data Item = Item
  { getId :: Tw.TweetId,
    getHashTags :: Tw.HashTags,
    getGeraItems :: [G.Gera]
  }
  deriving (Show, Eq)

aggregateTweets :: Tw.SearchCriteria -> IO Tw.Tweets
aggregateTweets sc = do
  client <- Tw.fromDotEnv
  Tw.aggregate client sc

saveTweets :: FilePath -> Tw.Tweets -> IO ()
saveTweets path tweets =
  L.writeFile path (encode tweets)

tweetsResultFilePath :: FilePath
tweetsResultFilePath = "result/tweets.json"

buildItem :: Tw.Tweet -> [G.Gera] -> Item
buildItem tweet = Item (Tw.getId tweet) (Tw.getHashTags tweet)

data Option = Option { start :: String
                     , end :: Maybe String
                     , maxResult :: Maybe Integer
                     }
optionParser :: Parser Option
optionParser =
  Option
    <$> strOption
      ( long "start"
          <> short 's'
          <> metavar "YYYYMMDD"
          <> help "startpoint datetime"
      )
    <*> option auto
      ( long "end"
          <> short 'e'
          <> metavar "YYYYMMDD"
          <> help "endpoint datetime"
          <> showDefault
          <> value Nothing
      )
    <*> option auto
      ( long "max-result"
          <> short 'r'
          <> help "max number of tweets"
          <> metavar "INT"
          <> showDefault
          <> value (Just 100)
      )

main :: IO ()
main = do
  o <- execParser opts
  print $ "s=" ++ start o ++ " e=" ++ fromMaybe "" (end o) ++ " r=" ++  show (fromMaybe 0 (maxResult o))
    where
      opts = info (optionParser <**> helper) fullDesc
