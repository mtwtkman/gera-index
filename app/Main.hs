module Main where

import Twitter

main :: IO ()
main = do
  client <- fromDotEnv
  let sc = SearchCriteria 10 (Just (Datetime 2022 4 1 0 0 0)) Nothing
  resp <- fetchTwitter client sc
  print resp
