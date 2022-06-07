module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
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
  C.writeFile path (encode tweets)

tweetsResultFilePath :: FilePath
tweetsResultFilePath = "result/tweets.json"

buildItem :: Tw.Tweet -> [G.Gera] -> Item
buildItem tweet = Item (Tw.getId tweet) (Tw.getHashTags tweet)

data Option = Option { start :: C.ByteString
                     , end :: Maybe C.ByteString
                     , maxResult :: Integer
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
          <> value 100
      )

main :: IO ()
main = do
  opt <-  execParser (info optionParser fullDesc)
  let sc = buildTwitterSearchCriteria opt
  print sc

data AppError = TwitterError Tw.TwitterError
              | GeraError G.GeraError
              deriving(Show)

type ThrowsError = Either AppError

buildTwitterSearchCriteria :: Option -> ThrowsError Tw.SearchCriteria
buildTwitterSearchCriteria o = do
  startDatetime <- case Tw.stringToDatetime $ start o of
            Right v -> Right v
            Left e -> Left (TwitterError e)
  endDatetime <- case end o of
                   Nothing -> Right Nothing
                   Just v -> case Tw.stringToDatetime v of
                               Right v' -> Right (Just v')
                               Left e -> Left (TwitterError e)
  Right (Tw.SearchCriteria (maxResult o) (Just startDatetime) endDatetime)
