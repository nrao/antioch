> module Antioch.AllTests where

> import qualified Antioch.DateTimeTests as DateTimeT
> import qualified Antioch.DebugTests as DebugT
> import qualified Antioch.DSSDataTests as DSSDataT
> import qualified Antioch.HardwareScheduleTests as HardwareScheduleT
> import qualified Antioch.ReportsTests as ReportT
> import qualified Antioch.ScoreTests as ScoreT
> import qualified Antioch.SLAlibTests as SLAlibT
> import qualified Antioch.StatisticsTests as StatsT
> import qualified Antioch.SimulationTests as SimsT
> import qualified Antioch.Schedule.PackTests as PackT
> import qualified Antioch.Schedule.ScheduleTests as ScheduleT
> import qualified Antioch.TimeAccountingTests as TimeAccountingT
> import qualified Antioch.UtilitiesTests as UtilitiesT
> import qualified Antioch.WeatherTests as WeatherT
> import Test.HUnit

> tests = TestList [
>     DateTimeT.tests
>   , DebugT.tests
>   , DSSDataT.tests
>   , HardwareScheduleT.tests
>   , ScoreT.tests
>   , SLAlibT.tests
>   , StatsT.tests
>   , PackT.tests
>   , ReportT.tests
>   , ScheduleT.tests
>   , SimsT.tests
>   , TimeAccountingT.tests
>   , UtilitiesT.tests
>   , WeatherT.tests
>   ]

> main = do
>     runTestTT tests
