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
  | NonDigit
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

parseInt :: T.Text -> ThrowsError Int
parseInt t = case T.decimal t of
               Left _ -> Left NonDigit
               Right (i, _) -> Right i


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
                    (TagOpen "div" _) : rem ->
                      let s  = fromTagText (head rem)
                          [[_, lbsNumber, title]] = s =~ [re|#([0-9]+) (.+)|] :: [[T.Text]]
                     in case T.decimal lbsNumber of
                      Right (number, _) -> Right (Episode title number)
                      _ -> Left NotFoundEpisode

extractBroadcastDeadLine :: [Tag T.Text] -> [Tag T.Text]
extractBroadcastDeadLine = dropWhile (~/= ("<div class=episode-details>" :: String))

findBroadcastDeadLine :: [Tag T.Text] -> Maybe Datetime
findBroadcastDeadLine tags = case extractBroadcastDeadLine tags of
                               [] -> Nothing
                               (TagOpen "div" _) : rem ->
                                 let s = fromTagText(head rem)
                                 in case s =~ [re|配信期限：([0-9]{4})/([0-9]{2})/([0-9]{2}) ([0-9]{2}):([0-9]{2})|] :: [[T.Text]] of
                                      [] -> Nothing
                                      [_ : ss@[yearS,monthS,dayS,hourS,minuteS]] ->
                                        case mapM parseInt ss of
                                          Left _ -> Nothing
                                          Right [year,month,day,hour,minute] -> Just (Datetime year month day hour minute)

parsePage :: T.Text -> ThrowsError Gera
parsePage content =
  undefined

trim :: T.Text -> T.Text
trim = T.unwords . T.words
