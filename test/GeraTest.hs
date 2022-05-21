{-# LANGUAGE OverloadedStrings #-}

module GeraTest (tests) where

import Data.ByteString.Builder
import Generator
import Gera
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Text.Printf
import Prelude hiding (toInteger)

tests = [properties]
