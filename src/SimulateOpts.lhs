> module SimulateOpts where

> import Antioch.Schedule
> import Antioch.Score
> import Antioch.Simulate
> import Antioch.Reports

> import System.Console.GetOpt
> import Data.Maybe ( fromMaybe, mapMaybe )
> import System.Environment
> import Text.Printf

> data Flag = StrategyFlag String | Output String | Days String | Name String
>       deriving Show
    
> options :: [OptDescr Flag]
> options =
>   [ Option ['s'] ["strategy"] (OptArg strategy "STRATEGY")  "Scheduling Strategy"
>   , Option ['o'] ["output"]   (OptArg outp "DIR") "Output Directory"
>   , Option ['d'] ["days"]     (OptArg days "DAYS") "Number of Days"
>   , Option ['n'] ["name"]     (OptArg name "NAME") "Simulation Name"
>   ]

TBF: why must I specify a default value, when they don't show up in the return
vlaue of getOpt?

> defaultStrategy = "Pack"
> defaultDir = "figures"
> defaultDays = "334"
> defaultName = "default"

> strategy, outp, days :: Maybe String -> Flag
> strategy = StrategyFlag . fromMaybe defaultStrategy
> outp     = Output  . fromMaybe defaultDir
> days     = Days  . fromMaybe defaultDays
> name     = Name . fromMaybe defaultName
    
> simulateOpts' :: [String] -> IO ([Flag], [String])
> simulateOpts' argv = 
>       case getOpt Permute options argv of
>          (o,n,[]  ) -> return (o,n)
>          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
>      where header = "Usage: simulate [OPTION...] "

> getFlagValue :: [Flag] -> (Flag -> Maybe String) -> String -> String
> getFlagValue flags getFlag defaultValue = case mapMaybe getFlag flags of
>   [] -> defaultValue
>   [value] -> value
>   otherwise -> defaultValue

> getOutput (Output dir) = Just (tail dir) 
> getOutput _ = Nothing

> getStrategyFlag (StrategyFlag s) = Just (tail s)
> getStrategyFlag _ = Nothing

> getDays (Days days) = Just (tail days)
> getDays _ = Nothing

> getName (Name name) = Just (tail name)
> getName _ = Nothing

> simulateOpts :: [String] -> IO (String, String, String, String)
> simulateOpts args = do
>   (flags, msgs) <- simulateOpts' args
>   let dir        = (getFlagValue flags getOutput defaultDir)
>   let stgStr     = (getFlagValue flags getStrategyFlag defaultStrategy)
>   let numDaysStr = (getFlagValue flags getDays defaultDays)
>   let name       = (getFlagValue flags getName defaultName)
>   return (stgStr, dir, numDaysStr, name)
