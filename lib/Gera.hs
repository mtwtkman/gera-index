{-# LANGUAGE QuasiQuotes #-}
module Gera where

import Data.String
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Text.Lazy.Encoding as T
import Network.HTTP.Req
import Text.HTML.TagSoup
import Text.RE.TDFA.Text.Lazy

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

datetimeFromString :: (IsString s) => s -> Datetime
datetimeFromString s = undefined

fetchPage :: Url 'Https -> IO LbsResponse
fetchPage url = runReq defaultHttpConfig $ do
  req GET url NoReqBody lbsResponse mempty

data Episode = Episode { getTitle :: T.Text
                       , getNumber :: Int
                       } deriving (Show, Eq)

data Gera = Gera
  { getEpisode :: Episode,
    getAudioUrl :: C.ByteString,
    getBroadcastDeadLine :: Maybe Datetime
  }
  deriving (Show, Eq)

findAudioUrl :: [Tag T.Text] -> ThrowsError C.ByteString
findAudioUrl tags = case dropWhile (~/= ("<audio>" :: String)) tags of
  [] -> Left NotFoundAudioFile
  x : _ -> Right $ T.encodeUtf8 (fromAttrib "src" x)

extractEpisodeSection :: [Tag T.Text] -> [Tag T.Text]
extractEpisodeSection = dropWhile (~/= ("<div class=episode-title>" :: String))

findEpisode :: [Tag T.Text] -> ThrowsError Episode
findEpisode tags = case extractEpisodeSection tags of
                    [] -> Left NotFoundEpisode
                    xs ->
                      let s  = innerText xs :: T.Text
                          [[_, lbsNumber, title]] = s =~ [re|#([0-9]+) (.+)|] :: [[T.Text]]
                     in case T.decimal lbsNumber of
                      Right (number, _) -> Right (Episode title number)
                      _ -> Left NotFoundEpisode

findBroadCastDeadLine :: [Tag T.Text] -> Maybe Datetime
findBroadCastDeadLine tags = case dropWhile (~/= ("<div class=episode-details>" :: String)) tags of
                               [] -> Nothing
                               x : _ ->
                                 let s = fromTagText x
                                 in case s =~ [re|配信期限：([0-9]{4}/[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2})|] :: [[T.Text]] of
                                      [] -> Nothing
                                      [[datetime]] -> Just (datetimeFromString datetime)

parsePage :: T.Text -> ThrowsError Gera
parsePage content =
  undefined
