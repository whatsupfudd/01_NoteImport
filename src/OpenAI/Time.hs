{-# LANGUAGE DerivingStrategies #-}
module OpenAI.Time
  ( TimeOai
  , timeOai
  , doubleTO
  , utcTO
  , textTO
  , nowTO
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Numeric (showFFloat)

newtype TimeOai = TimeOai { doubleTO :: Double }
  deriving stock (Eq, Ord, Show)

timeOai :: Double -> Either Text TimeOai
timeOai timeD
  | isNaN timeD = Left "invalid OpenAI time: NaN"
  | isInfinite timeD = Left "invalid OpenAI time: infinite"
  | otherwise = Right (TimeOai timeD)

utcTO :: TimeOai -> UTCTime
utcTO time = posixSecondsToUTCTime (realToFrac time.doubleTO)

textTO :: TimeOai -> Text
textTO time = T.pack (showFFloat Nothing time.doubleTO "")

nowTO :: IO TimeOai
nowTO = TimeOai . realToFrac <$> getPOSIXTime