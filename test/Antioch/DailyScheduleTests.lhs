> module Antioch.DailyScheduleTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Weather               (getWeatherTest)
> import Antioch.Utilities
> import Antioch.PProjects
> import Antioch.Score
> import Antioch.DailySchedule
> import Antioch.Schedule
> import Antioch.Filters
> import Antioch.ReceiverTemperatures
> import Data.List                     (zipWith6, sort, find)
> import Data.Maybe
> import Test.HUnit
> import System.Random
> import Control.Monad.Trans  (lift, liftIO) -- debug

> tests = TestList [
>       test_getEndTime
>     , test_removeBuffer
>     , test_removeBuffer_2
>     , test_runDailySchedule_1
>     , test_runDailySchedule_2
>     , test_runDailySchedule_3
>     , test_scheduleWindows
>      ]

> test_getEndTime = TestCase $ do
>   -- only the day matters in the dt argument
>   let expected = fromGregorian 2011 2 6 13 0 0
>   let dt = fromGregorian 2011 2 4 0 0 0
>   endTime <- liftIO $ getEndTime dt days workStartMinutes
>   assertEqual "test_getEndTime_1" expected endTime
>   let dt = fromGregorian 2011 2 4 5 0 0
>   endTime <- liftIO $ getEndTime dt days workStartMinutes
>   assertEqual "test_getEndTime_2" expected endTime
>   -- DST boundaries
>   -- Spring forward
>   let dt = fromGregorian 2011 3 10 0 0 0
>   endTime <- liftIO $ getEndTime dt days workStartMinutes
>   assertEqual "test_getEndTime_3" (fromGregorian 2011 3 12 13 0 0) endTime
>   let dt = fromGregorian 2011 3 11 0 0 0
>   endTime <- liftIO $ getEndTime dt days workStartMinutes
>   assertEqual "test_getEndTime_4" (fromGregorian 2011 3 13 12 0 0) endTime
>   -- Fall back
>   let dt = fromGregorian 2011 11 3 0 0 0
>   endTime <- liftIO $ getEndTime dt days workStartMinutes
>   assertEqual "test_getEndTime_5" (fromGregorian 2011 11 5 12 0 0) endTime
>   let dt = fromGregorian 2011 11 4 0 0 0
>   endTime <- liftIO $ getEndTime dt days workStartMinutes
>   assertEqual "test_getEndTime_6" (fromGregorian 2011 11 6 13 0 0) endTime
>     where
>       days = 2
>       workStartMinutes = 8*60

> test_removeBuffer = TestCase $ do
>   -- simplest case
>   let result = removeBuffer start dur ps history
>   assertEqual "test_removeBuffer_1" exp result
>   -- now make sure history isn't removed
>   let result = removeBuffer start dur ps history2
>   assertEqual "test_removeBuffer_2" exp2 result
>   -- shift the boundaries a little bit
>   let result = removeBuffer start dur2 ps history2
>   assertEqual "test_removeBuffer_3" exp3 result
>     where
>   start = fromGregorian 2006 6 1 12 0 0
>   dur = 24*60
>   history = []
>   -- 4 hour periods, from start to 24 + 12 hours later
>   times = [(60*4*m) `addMinutes` start | m <- [0 .. 9]]
>   ps = map (\dt -> defaultPeriod {startTime = dt, duration = (4*60)}) times
>   -- we should drop the last four periods
>   rest = (length ps) - 4
>   exp = take rest ps
>   history2 = [last ps]
>   exp2 = exp ++ history2
>   dur2 = 25*60
>   exp3 = (take ((length ps) - 3) ps) ++ history2

> test_removeBuffer_2 = TestCase $ do
>   -- simplest case
>   let result = removeBuffer start dur ps history
>   assertEqual "test_removeBuffer_2" ps result
>     where
>   start = fromGregorian 2006 2 2 0 0 0
>   dur = 24*60
>   ps = [defaultPeriod {startTime = fromGregorian 2006 2 2 12 0 0, duration = 2160}]

>   history = []

Here do the simplest schedule you can handle: one giant period
just make sure it gets cut off properly.

> test_runDailySchedule_1 = TestCase $ do
>   w <- getWeatherTest Nothing
>   rt <- getRT
>   -- get one big period
>   --results <- runScoring w [] $ runDailySchedule Pack dt minutes history [s]  
>   results <- runScoring w [] rt $ do
>       sf <- genScore dt . scoringSessions dt undefined $ [s]
>       dailySchedule' sf Pack dt minutes history [s]
>   assertEqual "test_runDailySchedule_1_1" exp results
>   -- nothing should get filtered out
>   let filtered = removeBuffer start minutes results history
>   assertEqual "test_runDailySchedule_1_2" exp filtered
>   -- make sure you work around pre-scheduled ones
>   --results <- runScoring w [] $ runDailySchedule Pack dt minutes history2 [s]  
>   results <- runScoring w [] rt $ do 
>       sf <- genScore dt . scoringSessions dt undefined $ [s]
>       dailySchedule' sf Pack dt minutes history2 [s]  
>   assertEqual "test_runDailySchedule_1_3" exp2 results
>   -- last one should get filtered out
>   let filtered = removeBuffer start minutes results history
>   assertEqual "test_runDailySchedule_1_4" exp3 filtered
>     where
>   dt = fromGregorian 2006 2 2 12 0 0
>   start = fromGregorian 2006 2 2 12 0 0
>   days = 1
>   minutes = (24*60*days)::Minutes
>   history = []
>   s = getSchedulableSession 
>   times = [(start, 2160)]
>   exp = map (mkPeriod s) times
>   -- for the history test, place pre-scheduled periods at the end & start 
>   times2 = [(start, 60), (fromGregorian 2006 2 3 23 0 0, 60)]
>   history2 = map (mkPeriod s) times2 
>   times3 = [(head times2)
>           , (fromGregorian 2006 2 2 13 0 0, (2160 - 120))
>           , (last times2)]
>   exp2 = map (mkPeriod s) times3
>   exp3 = take 2 exp2

Now, make a number of shorter spaced out periods by 
adjusting max duration and time between.

> test_runDailySchedule_2 = TestCase $ do
>   w <- getWeatherTest Nothing
>   rt <- getRT
>   --sf <- genScore dt . scoringSessions dt undefined $ [s]
>   -- get a number of smaller periods
>   results <- runScoring w [] rt $ do 
>       sf <- genScore dt . scoringSessions dt undefined $ [s]
>       dailySchedule' sf Pack dt minutes history [s]  
>   assertEqual "test_runDailySchedule_2_1" exp results
>   -- some of these should now get removed
>   let filtered = removeBuffer start minutes results history
>   assertEqual "test_runDailySchedule_2_2" exp2 filtered
>     where
>   dt = fromGregorian 2006 2 2 12 0 0
>   start = fromGregorian 2006 2 2 12 0 0
>   days = 1
>   minutes = (24*60*days)::Minutes
>   history = []
>   s' = getSchedulableSession --head ss
>   s = s' {maxDuration = (5*60), timeBetween = (5*60)}
>   times = [(fromGregorian 2006 2 2 13 45 0, 300)
>          , (fromGregorian 2006 2 3  1 30 0, 300)
>          , (fromGregorian 2006 2 3 13 15 0, 300)
>           ]
>   exp = map (mkPeriod s) times
>   exp2 = take 2 exp

> test_runDailySchedule_3 = TestCase $ do
>   w <- getWeatherTest . Just $ (-60) `addMinutes` dt
>   rt <- getRT
>   let ss = concatMap sessions pTestProjects
>   let ps = concatMap periods ss
>   results <- runScoring w [] rt $ do
>       sf <- genScore dt . scoringSessions dt undefined $ ss 
>       dailySchedule' sf Pack dt minutes ps ss
>   assertEqual "test_runDailySchedule_3" p (head $ results)
>     where
>       dt = fromGregorian 2006 10 20 4 0 0
>       days = 1
>       minutes = (24*60*days)::Minutes
>       p = (periods . findPSessionByName $ "TestWindowed2") !! 1

Here we build up a test of the window scheduling:
we start with a simple open session that can be scheduled throughout
the Scheduling Range (SR), then make it windowed, then start adding
more constraints.

> test_scheduleWindows = TestCase $ do
>   w <- getWeatherTest Nothing
>   rt <- getRT
>   -- if this was an open session, it would get scheduled w/ in the
>   -- entire scheduling range (SR).  
>   results <- runScoring w [] rt $ dailySchd dt getSchedulableSession [] 
>   assertEqual "test_scheduleWindows_0" dt (startTime . head $ results)
>   assertEqual "test_scheduleWindows_0" srDur (duration . head $ results)
>   -- As a windowed session, with the SR
>   -- falling completely w/ in the window, it acts the same.
>   results <- runScoring w [] rt $ dailySchd dt s history 
>   assertEqual "test_scheduleWindows_1" wtime (duration . head $ results)
>   assertEqual "test_scheduleWindows_2" (fromGregorian 2006 10 14 23 30 0) (startTime . head $ results)
>   -- now test the boundaries of the window - the SR only overlaps
>   -- w/ the begining of the window
>   results <- runScoring w [] rt $ dailySchd srStart2 s history 
>   -- offset because a windowed session can start earlier than midnight
>   -- because zero scores in the "overhead" quarter(s) are ignored
>   assertEqual "test_scheduleWindows_3" (addMinutes (-15) . wStart . head . windows $ s) (startTime . head $ results)
>   assertEqual "test_scheduleWindows_4" (48*60) (duration . head $ results)
>   -- now put the SR completely out of the window, and watch nothing
>   -- get scheduled:
>   results <- runScoring w [] rt $ dailySchd srStart3 s history 
>   assertEqual "test_scheduleWindows_5" [] results 
>   -- now, change the min/max duration & window total time
>   -- to match that of the default period (standard setup)
>   -- and watch the chosen period scheduled match that min/max value:
>   -- USE the availabel time in the sesssion!!!!!!!!!!!!!
>   results <- runScoring w [] rt $ dailySchd dt minMaxS history 
>   assertEqual "test_scheduleWindows_6" True (all (\p -> ((duration p)== 4*60)) results) 
>   assertEqual "test_scheduleWindows_7" 1 (length results) 
>   -- Now place the SR in range of the default period, and that's all
>   -- we should get scheduled
>   results <- runScoring w [] rt $ dailySchd srStart4 s history 
>   assertEqual "test_scheduleWindows_8" [dp] results 
>   -- Now, fuck with the window: give it a chosen period, and reduce
>   -- the default period's duration by half to compensate.  We also 
>   -- change wTotalTime, since this is really the time remaining for
>   -- the window.  Then make sure that
>   -- the chosen period scheduled is only equal to this new duration.
>   results <- runScoring w [] rt $ dailySchd dt s2 hist2 
>   assertEqual "test_scheduleWindows_9" True (all (\p -> ((duration p)== remaining)) results) 
>   assertEqual "test_scheduleWindows_10" 1 (length results) 
>     where
>   dt = fromGregorian 2006 10 14 12 0 0
>   days = 3
>   srDur = (days*24*60) + (12*60)
>   minutes = (24*60*days)::Minutes
>   -- a windowed session that can easily be scheduled
>   s' = getSchedulableSession { sType = Windowed }
>   -- a week long window
>   -- give it a lot of time at first so that we can see the window range
>   -- TBF: anything more then 2 days causes an exception!!!! WTF!!!!
>   wtime = (2*24*60)
>   winStart1 = fromGregorian' 2006 10 12
>   wr = [(winStart1, addMinutes' (7*24*60) winStart1)]
>   w =       defaultWindow {
>                 wPeriodId = Just 100
>               , wTotalTime =  wtime
>               , wRanges =  wr
>                }
>   -- w/ a default period near the end of it
>   dpDur = 4 * 60
>   dp = defaultPeriod { startTime = fromGregorian 2006 10 18 12 0 0
>                      , duration = dpDur
>                      , peId = 100
>                      , session = s' } 
>   history = [dp]
>   s  = makeSession s' [w] [dp]
>   dailySchd dt s history = do
>     sf <- genScore dt . scoringSessions dt undefined $ [s]
>     dailySchedule' sf Pack dt minutes history [s]
>   srStart2 = fromGregorian 2006 10 10 12 0 0
>   srStart3 = fromGregorian 2006 10 1 12 0 0
>   -- reduce the time for the window & min/max to match the default period
>   -- this should be the standard setup
>   w2 = w { wTotalTime = dpDur }
>   minMaxS = makeSession s { minDuration = dpDur, maxDuration = dpDur } [w2] [dp] 
>   srStart4 = fromGregorian 2006 10 17 12 0 0
>   -- schedule 1/2 the window: wTotalTime is really remaining time 
>   remaining = dpDur `div` 2
>   dp' = dp { duration = remaining }
>   w3 = w2 { wTotalTime = remaining }
>   chosenPeriod = dp' { startTime = fromGregorian 2006 10 12 12 0 0
>                      , pDuration = remaining
>                      , peId = 101 }
>   s2 = makeSession minMaxS [w3] [dp, chosenPeriod]
>   hist2 = periods s2

Utilities:


> mkPeriod s time = defaultPeriod { session = s
>                                 , startTime = fst time
>                                 , duration = snd time
>                                 , pDuration = snd time
>                                 }

> getSchedulableSession :: Session
> getSchedulableSession = head $ sessions getSchedulableProject

> getSchedulableProject :: Project
> getSchedulableProject = makeProject proj' (100*60) (100*60) [s']
>   where
>     -- simplest session that can be scheduled at anytime
>     -- TBF: hour angle limit fails on this occassionally - shouldn't happen
>     proj' = defaultProject { pAllottedT = 100*60 }
>     s' = defaultSession { sAllottedT = 100*60
>                         , sAllottedS = 100*60
>                         , minDuration = 2*60
>                         , maxDuration = 100*60
>                         , frequency = 2.0
>                         , receivers = [[Rcvr1_2]]
>                         , ra = 0.0 
>                         , dec = 1.5 --1.2217 -- always up
>                          }
