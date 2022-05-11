module Main where

import qualified GeraTest as GT
import qualified TwitterTest as TT
import Test.Tasty

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" $ GT.tests ++ TT.tests
