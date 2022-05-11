module Main where

import Data.Text
import Gera
import qualified Data.ByteString.Lazy.UTF8 as BS

main :: IO ()
main = do
  let sc = SearchCriteria (pack "ラジ母") (Date 2022 4 1) (Date 2022 4 24)
  content <- aggregateTweets sc
  let links = findGeraLink content
  print $ BS.toString content
  print links
