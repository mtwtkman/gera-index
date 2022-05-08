{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Generator where

import GHC.Generics
import Test.SmallCheck.Series

-- Helper

normalize :: (Bounded a, Ord a, Eq a) => a -> a
normalize x
  | x < minBound = minBound
  | x > maxBound = maxBound
  | otherwise = x

-- ValidInteger

newtype ValidYear = ValidYear {getValidYear :: Integer} deriving (Eq, Ord, Generic, Show)

instance Bounded ValidYear where
  minBound = ValidYear 1900
  maxBound = ValidYear 9999

instance Monad m => Serial m ValidYear where
  series = cons1 normalize

-- ValidMonth

newtype ValidMonth = ValidMonth {getValidMonth :: Integer} deriving (Eq, Ord, Generic, Show)

instance Bounded ValidMonth where
  minBound = ValidMonth 1
  maxBound = ValidMonth 12

instance Monad m => Serial m ValidMonth where
  series = cons1 normalize

-- ValidDay

newtype ValidDay = ValidDay {getValidDay :: Integer} deriving (Eq, Ord, Generic, Show)

instance Bounded ValidDay where
  minBound = ValidDay 1
  maxBound = ValidDay 31

instance Monad m => Serial m ValidDay where
  series = cons1 normalize

type ValidDateParams = (ValidYear, ValidMonth, ValidDay)
