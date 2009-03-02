> module Main where

> import Antioch.Schedule
> import Antioch.Score
> import Antioch.Simulate
> import Antioch.Reports

> main = --simulate06 (scheduleFixedDuration $ 4*60)
>   --generatePlots (scheduleFixedDuration 240) (statsPlotsToFile "~/figuresFD") 334
>   --generatePlots (scheduleFixedDuration' 240) (statsPlotsToFile "~/figuresFD2") 334
>   --generatePlots pack (statsPlotsToFile "~/figuresPack") 334
>   generatePlots scheduleMinDuration (statsPlotsToFile "figuresMD") 334
>   --generatePlots scheduleMinDuration (statsPlots ++ (statsPlotsToFile "./figures")) 334
