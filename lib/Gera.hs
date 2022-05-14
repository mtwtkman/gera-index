module Gera where

import Text.RE.TDFA.ByteString.Lazy ((*=~),matches, re)
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Data.List
import Data.Monoid ((<>))
import qualified Data.Text as Tx
import Network.HTTP.Req
import Text.Printf

data Index = Index
  { getArtist :: Artist,
    getEpisodes :: [Episode]
  }

newtype Artist = Artist {getName :: Tx.Text}

data Episode = Episode
  { getTitle :: Tx.Text,
    getNumber :: Integer,
    getDate :: String,
    getLink :: String
  }

data Date = Date
  { getYear :: Integer,
    getMonth :: Integer,
    getDay :: Integer
  }

dateToFormattedString :: Date -> String
dateToFormattedString d =
  intercalate "-" (map (printf "%02d") [getYear d, getMonth d, getDay d])

data SearchCriteria = SearchCriteria
  { getTagName :: Tx.Text,
    getFrom :: Date,
    getTo :: Date
  }

twitterSearchUrl = https "twitter.com" /: "search"

geraLinkBase :: String
geraLinkBase = "radio.gera.fan"
