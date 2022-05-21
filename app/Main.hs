{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as Tx
import qualified Data.Text.IO as Tx
import Network.HTTP.Req
import Twitter
import Data.Aeson
import qualified Data.ByteString.Lazy as L


main :: IO ()
main = do
  client <- fromDotEnv
  let sc = SearchCriteria 5 (Just (Datetime 2022 4 1 0 0 0)) Nothing
  resp <- fetchTwitter client sc
  let tweets = responseBody resp :: Tweets
  L.writeFile "gera-links.json" (encode tweets)
