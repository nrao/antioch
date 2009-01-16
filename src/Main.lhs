> module Main where

> import Antioch.Schedule
> import Antioch.Schedule.Pack
> import Antioch.Simulate

> main = simulate06 (scheduleFixedDuration $ 4*60)
