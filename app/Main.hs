{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as Tx
import qualified Data.Text.IO as Tx
import Network.HTTP.Req
import Twitter

main :: IO ()
main = do
  client <- fromDotEnv
  let sc = SearchCriteria 10 (Just (Datetime 2022 4 1 0 0 0)) Nothing
  resp <- fetchTwitter client sc
  let tweets = responseBody resp :: Tweets
  Tx.putStrLn $ Tx.intercalate "\n" [getText t | t <- getTweets tweets]
