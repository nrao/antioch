> module Antioch.AllTests where

> import qualified Antioch.DateTimeTests as DateTimeT
> import qualified Antioch.DebugTests as DebugT
> import qualified Antioch.ScoreTests as ScoreT
> import qualified Antioch.SLAlibTests as SLAlibT
> import qualified Antioch.StatisticsTests as StatsT
> import qualified Antioch.SimulationTests as SimsT
> import qualified Antioch.Schedule.PackTests as PackT
> import qualified Antioch.Schedule.ScheduleTests as ScheduleT
> import qualified Antioch.UtilitiesTests as UtilitiesT
> import qualified Antioch.WeatherTests as WeatherT
> import Test.HUnit

> tests = TestList [
>     DateTimeT.tests
>   , DebugT.tests
>   , ScoreT.tests
>   , SLAlibT.tests
>   , StatsT.tests
>   , PackT.tests
>   , ScheduleT.tests
>   , SimsT.tests
>   , UtilitiesT.tests
>   , WeatherT.tests
>   ]

> main = do
>     runTestTT tests
