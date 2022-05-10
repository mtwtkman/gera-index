module Main where

import Data.Text
import Gera

main :: IO ()
main = do
  let sc = SearchCriteria (pack "ラジ母") (Date 2022 1 1) (Date 2022 1 8)
  content <- aggregateTweets sc
  return ()
