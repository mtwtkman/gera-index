module Gera where

import qualified Data.ByteString.Lazy as L
import qualified Data.Text as Tx
import Network.HTTP.Req
import Text.HTML.TagSoup

data GeraError
  = FailedParse
  | NotFoundAudioFile
  | NotFoundEpisode
  deriving (Show, Eq)

type ThrowsError a = Either GeraError a

data Datetime = Datetime
  { getYear :: Int,
    getMonth :: Int,
    getDay :: Int,
    getHour :: Int,
    getMinute :: Int
  }
  deriving (Show, Eq)

fetchPage :: Url 'Https -> IO LbsResponse
fetchPage url = runReq defaultHttpConfig $ do
  req GET url NoReqBody lbsResponse mempty

data Episode = Episode
  { getTitle :: Tx.Text,
    getNumber :: Int
  }
  deriving (Show, Eq)

data Gera = Gera
  { getEpisode :: Episode,
    getAudioUrl :: L.ByteString,
    getBroadcastDeadLine :: Maybe Datetime
  }
  deriving (Show, Eq)

findAudioUrl :: [Tag L.ByteString] -> ThrowsError L.ByteString
findAudioUrl tags = case dropWhile (~/= ("<audio>" :: String)) tags of
  [] -> Left NotFoundAudioFile
  x : _ -> Right $ fromAttrib "src" x

findEpisode :: [Tag L.ByteString] -> ThrowsError Episode
findEpisode tags = undefined

parsePage :: L.ByteString -> ThrowsError Gera
parsePage content =
  undefined
