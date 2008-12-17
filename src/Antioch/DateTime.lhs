> module Antioch.DateTime where

> import Data.Time.Calendar hiding (fromGregorian)
> import Data.Time.Clock
> import Data.Time.Format
> import Data.Time.LocalTime
> import Database.HDBC
> import System.Locale
> import System.Time hiding (toClockTime)

> import qualified Data.Time.Calendar as Calendar

> type DateTime = UTCTime

> instance SqlType UTCTime where
>     toSql   = toSql . toClockTime
>     fromSql = fromClockTime . fromSql

> toMJD :: DateTime -> Rational
> toMJD = getModJulianDate . toUniversalTime

> fromMJD :: Rational -> DateTime
> fromMJD = fromUniversalTime . ModJulianDate

> toUniversalTime :: DateTime -> UniversalTime
> toUniversalTime = localTimeToUT1 0 . utcToLocalTime utc

> fromUniversalTime :: UniversalTime -> DateTime
> fromUniversalTime = localTimeToUTC utc . ut1ToLocalTime 0

> fromGregorian'       :: Integer -> Int -> Int -> DateTime
> fromGregorian' y m d = fromGregorian y m d 0 0 0

> fromGregorian :: Integer -> Int -> Int -> Int -> Int -> Int -> DateTime
> fromGregorian year month day hours minutes seconds =
>     UTCTime day' (secondsToDiffTime . fromIntegral $ seconds')
>   where
>     day'     = Calendar.fromGregorian year month day
>     seconds' = 3600 * hours + 60 * minutes + seconds

> toSeconds    :: DateTime -> Integer
> toSeconds dt = floor $
>     86400.0 * fromRational (toMJD dt - startOfTimeMJD)

> fromSeconds   :: Integer -> DateTime
> fromSeconds s = fromMJD $
>     fromIntegral s / 86400 + startOfTimeMJD

> toClockTime    :: DateTime -> ClockTime
> toClockTime dt = TOD (toSeconds dt) 0

> fromClockTime           :: ClockTime -> DateTime
> fromClockTime (TOD s _) = fromSeconds s

> startOfTime :: DateTime
> startOfTime = fromGregorian' 1970 1 1

> startOfTimeMJD :: Rational
> startOfTimeMJD = toMJD startOfTime

> toSqlString :: DateTime -> String
> toSqlString = formatDateTime sqlFormat

> fromSqlString :: String -> Maybe DateTime
> fromSqlString = parseDateTime sqlFormat

> formatDateTime     :: String -> DateTime -> String
> formatDateTime fmt = formatTime defaultTimeLocale fmt

> parseDateTime     :: String -> String -> Maybe DateTime
> parseDateTime fmt = parseTime defaultTimeLocale fmt

> sqlFormat = iso8601DateFormat (Just "%T")
