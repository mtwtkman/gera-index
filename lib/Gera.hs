module Gera where

import Data.List
import Data.Text (Text, unpack)
import Network.HTTP.Req

newtype Artist = Artist {getValue :: Text}

instance Show Artist where show = unpack . getValue

data Date = Date
  { getYear :: Integer,
    getMonth :: Integer,
    getDay :: Integer
  }

data SearchQuery = SearchQuery
  { getArtist :: Artist,
    getFrom :: Date,
    getTo :: Date
  }

twitterSearchUrl = https "twitter.com" /: "search"

-- query parameter: ?q=<word>%20(from:radio_gera)%20until%3AYYYY-MM-DD%20since%3A2022-MM-DD&src=typed_query&f=live
--buildQuery :: SearchQuery -> QueryParam

aggregateTweets :: SearchQuery -> IO ()
aggregateTweets aq = print "todo"
