{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Data.ByteString.Lazy as LBS


-- model domain
-- create http request
-- parse with aeson
-- yql string select item.condition.text from weather.forecast where woeid in (select woeid from geo.places(1) where text="dallas, tx")

-- astronomy, atmosphere, item.condition`
data Weather = Weather {
    sunrise :: String
  , sunset  :: String
}


-- fromJSON instance here

data WeatherResponse = WeatherResponse {
    response :: Weather
} deriving (Show)


runRequest :: T.Text -> IO ByteString
