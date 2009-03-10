> module Antioch.AllTests where

> import qualified Antioch.DebugTests as DebugT
> import qualified Antioch.ScoreTests as ScoreT
> import qualified Antioch.StatisticsTests as StatsT
> import qualified Antioch.SimulationTests as SimsT
> import qualified Antioch.Schedule.PackTests as PackT
> import qualified Antioch.Schedule.ScheduleTests as ScheduleT
> import qualified Antioch.WeatherTests as WeatherT
> import Test.HUnit

> tests = TestList [
>     DebugT.tests
>   , ScoreT.tests
>   , StatsT.tests
>   , PackT.tests
>   , ScheduleT.tests
>   , SimsT.tests
>   , WeatherT.tests
>   ]

> main = do
>     runTestTT tests
