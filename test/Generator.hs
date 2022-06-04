{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Generator where

import GHC.Generics
import Test.SmallCheck.Series

data Zero = Zero deriving (Generic, Show)

data OneDigit = OneDigit deriving (Generic, Show)

data TwoDigits = TwoDigits deriving (Generic, Show)

data FourDigits = FourDigits deriving (Generic, Show)

instance Monad m => Serial m Zero

instance Monad m => Serial m OneDigit

instance Monad m => Serial m TwoDigits

instance Monad m => Serial m FourDigits

zeroToInteger :: Zero -> Integer
zeroToInteger _ = 0

oneDigitToInteger :: OneDigit -> Integer
oneDigitToInteger _ = 1

twoDigitsToInteger :: TwoDigits -> Integer
twoDigitsToInteger _ = 10

fourDigitsToInteger :: FourDigits -> Integer
fourDigitsToInteger _ = 1000

newtype Year = Year FourDigits deriving (Generic, Show)

data Month = Month1 OneDigit | Month2 TwoDigits deriving (Generic, Show)

data Day = Day1 OneDigit | Day2 TwoDigits deriving (Generic, Show)

instance Monad m => Serial m Year

instance Monad m => Serial m Month

instance Monad m => Serial m Day

yearToInteger :: Year -> Integer
yearToInteger (Year d) = fourDigitsToInteger d

monthToInteger :: Month -> Integer
monthToInteger (Month1 d) = oneDigitToInteger d
monthToInteger (Month2 d) = twoDigitsToInteger d

dayToInteger :: Day -> Integer
dayToInteger (Day1 d) = oneDigitToInteger d
dayToInteger (Day2 d) = twoDigitsToInteger d
