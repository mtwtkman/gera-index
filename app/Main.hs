{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Gera as G
import Options.Applicative
import System.Directory as D
import qualified Twitter as Tw

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

data Option = Option
  { start :: C.ByteString,
    end :: C.ByteString,
    maxResult :: Integer,
    resultDir :: FilePath
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
    <*> strOption
      ( long "end"
          <> short 'e'
          <> metavar "YYYYMMDD"
          <> help "endpoint datetime"
          <> value C.empty
      )
    <*> option
      auto
      ( long "max-result"
          <> short 'm'
          <> help "max number of tweets"
          <> metavar "INT"
          <> showDefault
          <> value 100
      )
    <*> strOption
      ( long "result-dir"
          <> short 'd'
          <> metavar "DIRNAME"
          <> showDefault
          <> help "the file of fetched tweets"
          <> value "result"
      )

main :: IO ()
main = do
  opt <- execParser (info optionParser fullDesc)
  let sc = buildTwitterSearchCriteria opt
  D.createDirectoryIfMissing True $ resultDir opt
  print sc

data AppError
  = TwitterError Tw.TwitterError
  | GeraError G.GeraError
  | InvalidOption
  deriving (Show)

type ThrowsError = Either AppError

buildTwitterSearchCriteria :: Option -> ThrowsError Tw.SearchCriteria
buildTwitterSearchCriteria o = do
  validOption <- validateOption o
  startDatetime <- case Tw.stringToDatetime $ start validOption of
    Right v -> Right v
    Left e -> Left (TwitterError e)
  endDatetime <-
    let endV = end validOption
     in if C.null endV
          then Right Nothing
          else case Tw.stringToDatetime endV of
            Right v -> Right (Just v)
            Left e -> Left (TwitterError e)
  Right (Tw.SearchCriteria (maxResult validOption) (Just startDatetime) endDatetime)

validateOption :: Option -> ThrowsError Option
validateOption o@(Option s e _ _)
  | C.null e = Right o
  | s > e = Left InvalidOption
  | otherwise = Right o
