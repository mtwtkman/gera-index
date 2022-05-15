{-# LANGUAGE OverloadedStrings #-}

module TwitterTest (tests) where

import Generator
import System.IO.Unsafe
import Test.Hspec
import Test.SmallCheck
import Test.Tasty
import Test.Tasty.Hspec as HS
import Test.Tasty.SmallCheck as SC
import Text.Printf
import Twitter

tests = [specs, properties]

properties :: TestTree
properties = testGroup "Properties" [prop_dateToFormattedString]

prop_dateToFormattedString :: TestTree
prop_dateToFormattedString =
  testGroup
    "dateToFormattedString"
    [ SC.testProperty "concates members with a hyphen and pads by zero" $
        \y m d ->
          let yv = getValidYear (y :: ValidYear)
              mv = getValidMonth (m :: ValidMonth)
              dv = getValidDay (d :: ValidDay)
              date = Date yv mv dv
           in dateToFormattedString date == printf "%04d-%02d-%02d" yv mv dv
    ]

specs :: TestTree
specs = testGroup "Specs" $ map unsafePerformIO (spec_sep ++ spec_splitLine)

spec_sep :: [IO TestTree]
spec_sep =
  [ HS.testSpec "sep" $ do
      it "separates specified character" $ do
        let s1 = "xxx"
            c = '='
            s2 = "yyy"
         in sep c (s1 ++ [c] ++ s2) `shouldBe` (s1, s2)
  ]

spec_splitLine :: [IO TestTree]
spec_splitLine =
  [ HS.testSpec "splitLine" $ do
      it "split by newline character" $ do
        let row1 = "hogehoge"
            row2 = "nekoneko"
         in splitLine (row1 ++ "\n" ++ row2) [] [] `shouldBe` [row1, row2]
  ]
