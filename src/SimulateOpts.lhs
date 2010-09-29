> module SimulateOpts where

> import Antioch.Schedule
> import Antioch.Score
> import Antioch.Simulate
> import Antioch.Reports

> import System.Console.GetOpt
> import Data.Maybe ( fromMaybe, mapMaybe )
> import System.Environment
> import Text.Printf

> data Flag = StrategyFlag String | Output String | Begin String | Days String | Name String | Maint String | Types String | Backlog String
>       deriving Show
    
> options :: [OptDescr Flag]
> options =
>   [ Option ['s'] ["strategy"] (OptArg strategy "STRATEGY")  "Scheduling Strategy"
>   , Option ['o'] ["output"]   (OptArg outp "DIR") "Output Directory"
>   , Option ['b'] ["begin"]    (OptArg begin "BEGIN")  "Begin Date"
>   , Option ['d'] ["days"]     (OptArg days "DAYS") "Number of Days"
>   , Option ['n'] ["name"]     (OptArg name "NAME") "Simulation Name"
>   , Option ['m'] ["maint"]    (OptArg maint "MAINT") "Use Maintenance"
>   , Option ['t'] ["types"]    (OptArg types "TYPES") "Percent Open/Fixed/Windowed"
>   , Option ['l'] ["backlog"]  (OptArg backlog "BACKLOG") "Hours Backlog"
>   ]

> defaultStrategy = "Pack"
> defaultDir = "figures"
> defaultBegin = "2008-02-01"
> defaultDays = "334"
> defaultName = ""
> defaultMaint = "False"
> defaultTypes = "100/0/0"
> defaultBacklog = "2000"

> strategy, outp, begin, days :: Maybe String -> Flag
> strategy = StrategyFlag . fromMaybe defaultStrategy
> outp     = Output  . fromMaybe defaultDir
> begin    = Begin . fromMaybe defaultBegin
> days     = Days  . fromMaybe defaultDays
> name     = Name . fromMaybe defaultName
> maint    = Maint . fromMaybe defaultMaint
> types    = Types . fromMaybe defaultTypes
> backlog  = Backlog . fromMaybe defaultBacklog
    
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

> getBegin (Begin begin) = Just (tail begin)
> getBegin _ = Nothing

> getDays (Days days) = Just (tail days)
> getDays _ = Nothing

> getName (Name name) = Just (tail name)
> getName _ = Nothing

> getMaint (Maint maint) = Just (tail maint)
> getMaint _ = Nothing

> getTypes (Types types) = Just (tail types)
> getTypes _ = Nothing

> getBacklog (Backlog backlog) = Just (tail backlog)
> getBacklog _ = Nothing

> simulateOpts :: [String] -> IO (String, String, String, String, String, String, String, String)
> simulateOpts args = do
>   (flags, msgs) <- simulateOpts' args
>   let dir        = (getFlagValue flags getOutput defaultDir)
>   let stgStr     = (getFlagValue flags getStrategyFlag defaultStrategy)
>   let beginStr   = (getFlagValue flags getBegin defaultBegin)
>   let numDaysStr = (getFlagValue flags getDays defaultDays)
>   let name       = (getFlagValue flags getName defaultName)
>   let maint      = (getFlagValue flags getMaint defaultMaint)
>   let types      = (getFlagValue flags getTypes defaultTypes)
>   let backlog    = (getFlagValue flags getBacklog defaultBacklog)
>   return (stgStr, dir, beginStr, numDaysStr, name, maint, types, backlog)
