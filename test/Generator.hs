{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Generator where

import GHC.Generics
import Test.SmallCheck.Series
import Prelude hiding (toInteger)

class ToInteger a where
  toInteger :: a -> Integer

-- ValidInteger

newtype ValidYear = ValidYear {getValidYear :: Integer} deriving (Eq, Ord, Generic, Show)

instance Bounded ValidYear where
  minBound = ValidYear 1900
  maxBound = ValidYear 9999

instance Monad m => Serial m ValidYear where
  series = cons1 $ \y ->
          if y < minBound
            then minBound
            else
              if y > maxBound
                then maxBound
                else y

-- ValidMonth

newtype ValidMonth = ValidMonth {getMonth :: Positive Integer} deriving (Eq, Ord, Generic)

instance ToInteger ValidMonth where
  toInteger = getPositive . getMonth

instance Show ValidMonth where show = show . toInteger

instance Bounded ValidMonth where
  minBound = ValidMonth (Positive 1)
  maxBound = ValidMonth (Positive 12)

instance Monad m => Serial m ValidMonth

-- ValidDay

newtype ValidDay = ValidDay {getDay :: Positive Integer} deriving (Eq, Ord, Generic)

instance ToInteger ValidDay where
  toInteger = getPositive . getDay

instance Show ValidDay where show = show . getPositive . getDay

instance Bounded ValidDay where
  minBound = ValidDay (Positive 1)
  maxBound = ValidDay (Positive 31)

instance Monad m => Serial m ValidDay
