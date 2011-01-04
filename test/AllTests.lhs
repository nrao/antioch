> module Antioch.AllTests where

> import qualified Antioch.DailyScheduleTests as DailyScheduleT
> import qualified Antioch.DateTimeTests as DateTimeT
> import qualified Antioch.DebugTests as DebugT
> import qualified Antioch.DSSDataTests as DSSDataT
> import qualified Antioch.FilterTests as FilterTestT
> import qualified Antioch.GenerateScheduleTests as GeneratorT
> import qualified Antioch.HardwareScheduleTests as HardwareScheduleT
> import qualified Antioch.ReceiverTests as ReceiverT
> import qualified Antioch.ReceiverTemperaturesTests as ReceiverTempT
> import qualified Antioch.ReportsTests as ReportT
> import qualified Antioch.ReservationsTests as ReservationsT
> import qualified Antioch.RunHistWeatherOptTests as RunHistWeatherOptT
> import qualified Antioch.RunScoresTests as RunScoresT
> import qualified Antioch.ScoreTests as ScoreT
> import qualified Antioch.SLAlibTests as SLAlibT
> import qualified Antioch.StatisticsTests as StatsT
> import qualified Antioch.SimulationTests as SimsT
> import qualified Antioch.Schedule.PackTests as PackT
> import qualified Antioch.TimeAccountingTests as TimeAccountingT
> import qualified Antioch.UtilitiesTests as UtilitiesT
> import qualified Antioch.WeatherTests as WeatherT
> import Test.HUnit

> tests = TestList [
>     DailyScheduleT.tests
>   , DateTimeT.tests
>   , DebugT.tests
>   , DSSDataT.tests
>   , FilterTestT.tests
>   , HardwareScheduleT.tests
>   , ScoreT.tests
>   , RunScoresT.tests
>   , SLAlibT.tests
>   , StatsT.tests
>   , PackT.tests
>   , ReceiverT.tests
>   , ReceiverTempT.tests
>   , ReservationsT.tests
>   , RunHistWeatherOptT.tests
>   , TimeAccountingT.tests
>   , UtilitiesT.tests
>   , WeatherT.tests
>   , GeneratorT.tests
>   , ReportT.tests
>   , SimsT.tests  -- place longer tests at the end
>   ]

> main = do
>     runTestTT tests
