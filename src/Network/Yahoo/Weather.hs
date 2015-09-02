{-# LANGUAGE OverloadedStrings #-}
module Horus
  (
  -- functions here
  ) where

import           Control.Applicative        ((<$>), (<*>))
import           Control.Lens
import           Control.Monad              (mzero)
import           Data.Aeson
import           Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map                   as M
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           Network.Wreq

-- model domain
-- create http request
-- parse with aeson
-- yql string select item.condition.text from weather.forecast where woeid in (select woeid from geo.places(1) where text="dallas, tx")

-- astronomy, atmosphere, item.condition`
data Weather = Weather {
    sunrise :: !T.Text
  , sunset  :: !T.Text
  , date    :: !T.Text
  , temp    :: !T.Text
  , fcast   :: !T.Text
} deriving (Show)


instance FromJSON Weather where
    parseJSON (Object o) =
        Weather <$> (astronomy >>= (.:"sunrise"))
                <*> (astronomy >>= (.: "sunset"))
                <*> (item >>= (.: "date"))
                <*> (item >>= (.: "temp"))
                <*> (item >>= (.: "text"))
        where e = (o .: "query") >>= (.:"results") >>= (.: "channel")
              astronomy = e >>= (.:"astronomy")
              item = e >>= (.:"item") >>= (.:"condition")


    parseJSON _ = mzero

        -- >>= (.: "results") >>= (.: "channel") >>= (.: "astronomy") >>= (.: "sunrise"))

endPoint = "https://query.yahooapis.com/v1/public/yql?q=select%20astronomy%2C%20%20item.condition%20from%20weather.forecast%20where%20woeid%20in%20(select%20woeid%20from%20geo.places(1)%20where%20text%3D%22dallas%2C%20tx%22)&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys"

constructQuery :: String -> String -> String
constructQuery city state = "select astronomy,  item.condition from weather.forecast" <>
                            " where woeid in (select woeid from geo.places(1)" <>
                            " where text=\"" <> city <> "," <> state <> "\")"

runRequest :: String -> IO ByteString
runRequest yql = do
    r <- get yql
    return $ r ^. responseBody


-- run :: String -> IO (Maybe Weather)
run yql = do
    w <- runRequest yql
    return $ decode w :: IO (Maybe Weather)
