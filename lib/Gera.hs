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

instance Show Date where
  show a = intercalate "-" $ map (show . ($ a)) [getYear, getMonth, getDay]

data SearchQuery = SearchQuery
  { getArtist :: Artist,
    getFrom :: Date,
    getTo :: Date
  }

twitterSearchUrl = https "twitter.com" /: "search"

--buildQuery :: SearchQuery -> QueryParam

aggregateTweets :: SearchQuery -> IO ()
aggregateTweets aq = print "todo"
