> module Antioch.DateTime where

> import Data.Fixed (div')
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

Define a local synonym for UTCTime just to get some insulation from
the craziness of the Haskell standard library date and time functions.

> type DateTime = UTCTime

> instance Arbitrary UTCTime where
>     arbitrary       = do
>         offset <- choose (0, 20000) :: Gen Float
>         return . fromMJD' $ offset + fromRational startOfTimeMJD
>     coarbitrary _ b = b

So that we can use our DateTime class with HDBC.

> instance SqlType UTCTime where
>     toSql   = toSql . toClockTime
>     fromSql = fromClockTime . fromSql

Defined here so that users don't need to know about Data.Time.Clock.

> getCurrentTime :: IO DateTime
> getCurrentTime = Clock.getCurrentTime

Conversion back and forth between DateTime and MJD.

> toMJD :: DateTime -> Rational
> toMJD = getModJulianDate . toUniversalTime

> toMJD' :: RealFloat a => DateTime -> a
> toMJD' = fromRat . toMJD

> fromMJD :: Rational -> DateTime
> fromMJD = fromUniversalTime . ModJulianDate

> fromMJD' :: RealFloat a => a -> DateTime
> fromMJD' = fromMJD . realToFrac

> invariant f x = f x == x
  
> prop_MJD  = invariant $ fromMJD  . toMJD
> prop_MJD' = invariant $ fromMJD' . toMJD'

Because UTCTime is opaque, we need to convert to UniversalTime in
order to do anything with it, but these functions are mainly of
interest internally.

> toUniversalTime :: DateTime -> UniversalTime
> toUniversalTime = localTimeToUT1 0 . utcToLocalTime utc

> fromUniversalTime :: UniversalTime -> DateTime
> fromUniversalTime = localTimeToUTC utc . ut1ToLocalTime 0

> prop_Universal = invariant $ fromUniversalTime . toUniversalTime

Take apart a DateTime into pieces and parts.
  
> toGregorian'    :: DateTime -> (Integer, Int, Int)
> toGregorian' dt = (y, m, d)
>   where
>     (y, m, d, _, _, _) = toGregorian dt

> toGregorian    :: DateTime -> (Integer, Int, Int, Int, Int, Int)
> toGregorian dt = (year, month, day', hours, minutes, seconds `div'` 1)
>   where
>     LocalTime day tod   = utcToLocalTime utc dt
>     (year, month, day') = Calendar.toGregorian day
>     TimeOfDay hours minutes seconds = tod

Combine pieces and parts to produce a DateTime.
      
> fromGregorian'       :: Integer -> Int -> Int -> DateTime
> fromGregorian' y m d = fromGregorian y m d 0 0 0

> fromGregorian :: Integer -> Int -> Int -> Int -> Int -> Int -> DateTime
> fromGregorian year month day hours minutes seconds =
>     UTCTime day' (secondsToDiffTime . fromIntegral $ seconds')
>   where
>     day'     = Calendar.fromGregorian year month day
>     seconds' = 3600 * hours + 60 * minutes + seconds

Getting closer to the machine: Not all the functionality of
System.Time is available in Data.Time, and the only way we can convert
back and forth is to go through seconds.

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

> prop_StartOfTime _ = toSeconds startOfTime == 0

> startOfTimeMJD :: Rational
> startOfTimeMJD = toMJD startOfTime

Formatting and parsing, with special attention to the format used by
ODBC and MySQL.

> toSqlString :: DateTime -> String
> toSqlString = formatDateTime sqlFormat

> fromSqlString :: String -> Maybe DateTime
> fromSqlString = parseDateTime sqlFormat

> prop_SqlString dt = (fromJust . fromSqlString . toSqlString $ dt') == dt'
>   where
>     Just dt' = fromSqlString . toSqlString $ dt

> prop_SqlStartOfTime _ = toSqlString startOfTime == "1970-01-01 00:00:00"

> formatDateTime     :: String -> DateTime -> String
> formatDateTime fmt = formatTime defaultTimeLocale fmt

> parseDateTime     :: String -> String -> Maybe DateTime
> parseDateTime fmt = parseTime defaultTimeLocale fmt

> sqlFormat = iso8601DateFormat (Just "%T")
