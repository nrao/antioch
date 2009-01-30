> module Main where

> import Antioch.Reports
> import Antioch.Schedule

> main = do
>   --generatePlots (scheduleFixedDuration 240) (statsPlotsToFile "~/figuresFD") 334  -- 76 Minutes
>   --generatePlots (scheduleFixedDuration' 240) (statsPlotsToFile "~/figuresFD2") 334  -- 76 Minutes
>   --generatePlots pack (statsPlotsToFile "~/figuresPack") 334  -- 96 Minutes
>   generatePlots scheduleMinDuration (statsPlotsToFile "~/figuresMD") 334  -- 76 Minutes
>   --generatePlots scheduleMinDuration (statsPlots ++ (statsPlotsToFile "~/figuresMD")) 4  -- 76 Minutes
