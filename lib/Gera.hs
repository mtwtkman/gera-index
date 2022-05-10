module Gera where

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

buildQuery :: (QueryParam param, Monoid param) => SearchCriteria -> param
buildQuery s =
  "src" =: ("typed_query" :: String)
    <> "f" =: ("live" :: String)
    <> "q"
      =: Tx.unwords
        [ getTagName s,
          "(from:radio_gera)",
          Tx.pack ("since=" ++ dateToFormattedString (getFrom s)),
          Tx.pack ("until=" ++ dateToFormattedString (getTo s))
        ]

findGeraLink :: L.ByteString -> [String]
findGeraLink bs = undefined

aggregateTweets :: SearchCriteria -> IO L.ByteString
aggregateTweets s = runReq defaultHttpConfig $ do
  let qp = buildQuery s
  resp <- req GET twitterSearchUrl NoReqBody lbsResponse qp
  return $ responseBody resp
