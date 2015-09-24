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
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans

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
-- Reader Config
data Config = Config {
    ipAPI:: String
  , root :: String
  , datatable :: T.Text} deriving (Show)

config = Config "http://ipinfo.io/json" "https://query.yahooapis.com/v1/public/yql" "https://query.yahooapis.com/v1/public/yql"

-- Monad Stack
type MaybeIO = MaybeT IO
type StackRM = ReaderT Config MaybeIO

runRM r m = runMaybeT $ runReaderT r m
-- lift a (Maybe a)  int the monad stack
liftMaybe :: Maybe a -> ReaderT r MaybeIO a
liftMaybe = liftR . liftM
    where liftR = ReaderT . return
          liftM = MaybeT . return

-- need to convert Get exception to Nothing
-- getLocation :: MaybeIO IpResponse
getLocation = do
    r <- get "http://ipinfo.io/json"
    let body = r ^. responseBody
    return (decode body :: Maybe IpResponse)

getLocation2 :: StackRM IpResponse
getLocation2 = do
    ep <- asks ipAPI
    r <- liftIO $ get ep
    let body = r ^. responseBody
    let decoded = decode body :: (Maybe IpResponse)
    liftMaybe decoded

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

getWeather :: T.Text -> StackRM Weather
getWeather yql = do
    root' <- asks root
    datatable' <- asks datatable
    let opts = defaults & param "q" .~ [yql]
                      & param "env" .~ [datatable']
                      & param "format" .~ ["json"]
    r <- liftIO $ getWith opts root'
    let body = r ^. responseBody
    let decoded = decode body :: (Maybe Weather)
    liftMaybe decoded


-- run :: T.Text -> IO (Maybe Weather)
-- run yql = buildRequest yql >>= (\r -> return $ decode r :: IO (Maybe Weather))

dallas :: T.Text
dallas = constructQuery "dallas" "tx"

denton :: T.Text
denton = constructQuery "Denton" "Texas"

-- Quick Examples

run' c = flip runRM c
run= run' config

runCity = run . getWeather
runDenton = runCity denton
runDallas = runCity dallas 


--runMyLocation :: IO (Maybe Weather)
-- runMyLocation = do
--    loc <- getLocation
--     maybe (return Nothing) getWeather loc
--    where getWeather = (run . (uncurry constructQuery) . (city &&& region))
