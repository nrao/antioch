> module Antioch.AllTests where

> import qualified Antioch.ScoreTests as ScoreT
> import qualified Antioch.StatisticsTests as StatsT
> import qualified Antioch.Schedule.PackTests as PackT
> -- import qualified Antioch.Schedule.ScheduleTests as ScheduleT
> import Test.HUnit

> tests = TestList [
>     ScoreT.tests
>   , StatsT.tests
>   , PackT.tests
>   ]

> main = do
>     runTestTT tests
