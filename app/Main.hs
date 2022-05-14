module Main where

import Gera
import Twitter

main :: IO ()
main = do
  client <- fromDotEnv
  print client

