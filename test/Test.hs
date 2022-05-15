module Main where

import Test.Tasty
import qualified TwitterTest as TT

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" $ TT.tests
