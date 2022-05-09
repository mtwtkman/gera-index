module Main where

import Gera
import Data.Text

main :: IO ()
main = do
  let sc = SearchCriteria (pack "ラジ母") (Date 2022 1 1) (Date 2022 1 8)
  aggregateTweets sc
