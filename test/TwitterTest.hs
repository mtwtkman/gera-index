module TwitterTest (tests) where

import Twitter
import Test.Tasty
import Test.Tasty.Hspec as HS
import Test.Hspec
import System.IO.Unsafe

tests = [specs]

specs :: TestTree
specs = testGroup "Specs" $ map unsafePerformIO (spec_sep ++ spec_splitLine)

spec_sep :: [IO TestTree]
spec_sep =
    [ HS.testSpec "sep" $ do
        it "separates specified character" $ do
          let s1 = "xxx"
              c = '='
              s2 = "yyy"
           in sep c (s1 ++ [c] ++ s2) `shouldBe` (s1,s2)
    ]

spec_splitLine :: [IO TestTree]
spec_splitLine =
    [ HS.testSpec "splitLine" $ do
        it "split by newline character" $ do
          let row1 = "hogehoge"
              row2 = "nekoneko"
           in splitLine (row1 ++ "\n" ++ row2) [] [] `shouldBe` [row1,row2]
    ]
