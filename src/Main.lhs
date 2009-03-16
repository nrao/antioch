> module Main where

> import Antioch.Schedule
> import Antioch.Score
> import Antioch.Simulate
> import Antioch.Reports

> main = do
>     --(_, trace) <- simulate06 (scheduleFixedDuration $ 4*60)
>     --return ()
>     -- print trace
>   --generatePlots (scheduleFixedDuration 240) (statsPlotsToFile "~/figuresFD") 334  -- 76 Minutes
>   --generatePlots (scheduleFixedDuration' 240) (statsPlotsToFile "~/figuresFD2") 334  -- 76 Minutes
>   generatePlots pack (statsPlotsToFile "../myplots_pack") (tracePlotsToFile "../myplots_pack") 330
>   --generatePlots scheduleMinDuration (statsPlotsToFile "../myplots") (tracePlotsToFile "../myplots") 330
>   --generatePlots scheduleMinDuration (statsPlots ++ (statsPlotsToFile "./figures")) 334  -- 76 Minutes

