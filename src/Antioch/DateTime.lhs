> module Antioch.DateTime where

> import Data.Fixed (div')
> import Data.Function (on)
> import Data.Maybe (fromJust)
> import Data.Time.Calendar hiding (fromGregorian, toGregorian)
> import Data.Time.Clock hiding (getCurrentTime)
> import Data.Time.Format
> import Data.Time.LocalTime
> import Database.HDBC
> import Numeric (fromRat)
> import System.Locale
> import System.Time hiding (toClockTime)
> import Test.QuickCheck

> import qualified Data.Time.Calendar as Calendar
> import qualified Data.Time.Clock as Clock

> type DateTime = Int

> instance Arbitrary UTCTime where
>     arbitrary       = do
>         offset <- choose (0, 20000) :: Gen Float
>         return . fromMJD' $ offset + fromRational startOfTimeMJD
>     coarbitrary _ b = b

So that we can use our UTCTime class with HDBC.

> instance SqlType UTCTime where
>     toSql   = toSql . toClockTime
>     fromSql = fromClockTime . fromSql

Defined here so that users don't need to know about Data.Time.Clock.

> getCurrentTime :: IO DateTime
> getCurrentTime = fmap toSeconds Clock.getCurrentTime

Conversion back and forth between UTCTime and MJD.

> toMJD :: UTCTime -> Rational
> toMJD = getModJulianDate . toUniversalTime

> toMJD' :: RealFloat a => UTCTime -> a
> toMJD' = fromRat . toMJD

> fromMJD :: Rational -> UTCTime
> fromMJD = fromUniversalTime . ModJulianDate

> fromMJD' :: RealFloat a => a -> UTCTime
> fromMJD' = fromMJD . realToFrac

> invariant f x = f x == x
  
> prop_MJD  = invariant $ fromMJD  . toMJD
> prop_MJD' = invariant $ fromMJD' . toMJD'

Because UTCTime is opaque, we need to convert to UniversalTime in
order to do anything with it, but these functions are mainly of
interest internally.

> toUniversalTime :: UTCTime -> UniversalTime
> toUniversalTime = localTimeToUT1 0 . utcToLocalTime utc

> fromUniversalTime :: UniversalTime -> UTCTime
> fromUniversalTime = localTimeToUTC utc . ut1ToLocalTime 0

> prop_Universal = invariant $ fromUniversalTime . toUniversalTime

Take apart a UTCTime into pieces and parts.
  
> toGregorian'    :: DateTime -> (Int, Int, Int)
> toGregorian' dt = (y, m, d)
>   where
>     (y, m, d, _, _, _) = toGregorian dt

> toGregorian    :: DateTime -> (Int, Int, Int, Int, Int, Int)
> toGregorian dt = (fromIntegral year, month, day', hours, minutes, seconds `div'` 1)
>   where
>     LocalTime day tod   = utcToLocalTime utc . fromSeconds $ dt
>     (year, month, day') = Calendar.toGregorian day
>     TimeOfDay hours minutes seconds = tod

Combine pieces and parts to produce a UTCTime.
      
> fromGregorian'       :: Int -> Int -> Int -> DateTime
> fromGregorian' y m d = fromGregorian y m d 0 0 0

> fromGregorian :: Int -> Int -> Int -> Int -> Int -> Int -> DateTime
> fromGregorian year month day hours minutes seconds = toSeconds $
>     UTCTime day' (secondsToDiffTime . fromIntegral $ seconds')
>   where
>     day'     = Calendar.fromGregorian (fromIntegral year) month day
>     seconds' = 3600 * hours + 60 * minutes + seconds

Getting closer to the machine: Not all the functionality of
System.Time is available in Data.Time, and the only way we can convert
back and forth is to go through seconds.

> toSeconds    :: UTCTime -> Int
> toSeconds dt = floor $
>     86400.0 * fromRational (toMJD dt - startOfTimeMJD)

> fromSeconds   :: Int -> UTCTime
> fromSeconds s = fromMJD $
>     fromIntegral s / 86400 + startOfTimeMJD

> toClockTime    :: UTCTime -> ClockTime
> toClockTime dt = TOD (fromIntegral . toSeconds $ dt) 0

> fromClockTime           :: ClockTime -> UTCTime
> fromClockTime (TOD s _) = fromSeconds . fromIntegral $ s

> startOfTime :: DateTime
> startOfTime = 0

> startOfTimeMJD :: Rational
> startOfTimeMJD = toMJD $ UTCTime (Calendar.fromGregorian 1970 1 1) 0

Formatting and parsing, with special attention to the format used by
ODBC and MySQL.

> toSqlString :: DateTime -> String
> toSqlString = formatUTCTime sqlFormat . fromSeconds

> fromSqlString :: String -> Maybe DateTime
> fromSqlString = fmap toSeconds . parseUTCTime sqlFormat

> prop_SqlString dt = (fromJust . fromSqlString . toSqlString $ dt') == dt'
>   where
>     Just dt' = fromSqlString . toSqlString $ dt

> prop_SqlStartOfTime _ = toSqlString startOfTime == "1970-01-01 00:00:00"

> formatUTCTime :: String -> UTCTime -> String
> formatUTCTime = formatTime defaultTimeLocale

> parseUTCTime :: String -> String -> Maybe UTCTime
> parseUTCTime = parseTime defaultTimeLocale

> sqlFormat = iso8601DateFormat (Just "%T")

Simple arithmetic.

> addMinutes' :: Int -> DateTime -> DateTime
> addMinutes' = addMinutes
  
> addMinutes :: Int -> DateTime -> DateTime
> addMinutes = addSeconds . (60 *)

> diffMinutes'   :: DateTime -> DateTime -> Int
> diffMinutes' x = diffMinutes x

> diffMinutes   :: DateTime -> DateTime -> Int
> diffMinutes x = (`div` 60) . diffSeconds x
  
> addSeconds :: Int -> DateTime -> DateTime
> addSeconds = (+)

> diffSeconds :: DateTime -> DateTime -> Int
> diffSeconds = (-)

> getRise    :: DateTime -> DateTime
> getRise dt = fromGregorian y m d 12 30 0
>     where
>        (y, m, d) = toGregorian' dt

> getSet    :: DateTime -> DateTime
> getSet dt = fromGregorian y m d 22 0 0
>     where
>        (y, m, d) = toGregorian' dt

> isDayTime    :: DateTime -> Bool
> isDayTime dt = getRise dt <= dt && dt <= getSet dt
