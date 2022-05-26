module Main where

import Twitter

main :: IO ()
main = do
  client <- fromDotEnv
  let sc = SearchCriteria 5 (Just (Datetime 2022 4 1 0 0 0)) Nothing
  aggregate client sc "result/tweets.json"
