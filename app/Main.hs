module Main where

import Twitter

main :: IO ()
main = do
  client <- fromDotEnv
  print client
