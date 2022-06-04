module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Gera as G
import Options.Applicative
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
  L.writeFile path (encode tweets)

tweetsResultFilePath :: FilePath
tweetsResultFilePath = "result/tweets.json"

buildItem :: Tw.Tweet -> [G.Gera] -> Item
buildItem tweet = Item (Tw.getId tweet) (Tw.getHashTags tweet)

data Sample = Sample
  { hello :: String,
    quite :: Bool,
    enthusiasm :: Int
  }

sample :: Parser Sample
sample =
  Sample
    <$> strOption
      ( long "hello"
          <> metavar "TARGET"
          <> help "Target for the greeting"
      )
    <*> switch
      ( long "quiet"
          <> short 'q'
          <> help "Whether to be quiet"
      )
    <*> option
      auto
      ( long "enthusiasm"
          <> help "Howenthusiastically to greet"
          <> showDefault
          <> value 1
          <> metavar "INT"
      )

main :: IO ()
main = greet =<< execParser opts
  where
    opts =
      info
        (sample <**> helper)
        ( fullDesc
            <> progDesc "Print a greeting for TARGET"
            <> header "hello - a test for optparse-applicative"
        )

greet :: Sample -> IO ()
greet (Sample h False n) = putStrLn $ "Helo, " ++ h ++ replicate n '!'
greet _ = return ()
