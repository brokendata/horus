{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

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
import GHC.Generics
import Control.Arrow ((&&&))
import Control.Monad


data Weather = Weather {
    sunrise :: !T.Text
  , sunset  :: !T.Text
  , date    :: !T.Text
  , temp    :: !T.Text
  , fcast   :: !T.Text
} deriving (Show)

data IpResponse = IpResponse {
    ip       :: !T.Text
  , hostname :: !T.Text
  , city     :: !T.Text
  , region   :: !T.Text
  , country  :: !T.Text
  , loc      :: !T.Text
  , org      :: !T.Text
  , postal   :: !T.Text
  , phone    :: Int

} deriving (Show, Generic)

instance FromJSON IpResponse

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

-- need to convert Get exception to Nothing
getLocation:: IO (Maybe IpResponse)
getLocation = do
    r <- get "http://ipinfo.io/json"
    let body = r ^. responseBody
    return (decode body :: Maybe IpResponse)

constructQuery :: T.Text -> T.Text -> T.Text
constructQuery city state = "select astronomy,  item.condition from weather.forecast" <>
                            " where woeid in (select woeid from geo.places(1)" <>
                            " where text=\"" <> city <> "," <> state <> "\")"

buildRequest :: T.Text -> IO ByteString
buildRequest yql = do
    let root = "https://query.yahooapis.com/v1/public/yql"
        datatable = "store://datatables.org/alltableswithkeys"
        opts = defaults & param "q" .~ [yql]
                          & param "env" .~ [datatable]
                          & param "format" .~ ["json"]
    r <- getWith opts root
    return $ r ^. responseBody

run :: T.Text -> IO (Maybe Weather)
run yql = buildRequest yql >>= (\r -> return $ decode r :: IO (Maybe Weather))

dallas :: T.Text
dallas = constructQuery "dallas" "tx"

denton :: T.Text
denton = constructQuery "Denton" "Texas"

-- Quick Examples
runDenton = run denton
runDallas = run dallas

runMyLocation:: IO (Maybe Weather)
runMyLocation = do
    r <- getLocation
    case r of
        Just ip -> build ip
        _ ->  return Nothing
    where build = (run . (uncurry constructQuery) . (city &&& region))
