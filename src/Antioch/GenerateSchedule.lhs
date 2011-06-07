> module Antioch.GenerateSchedule where

> import Antioch.Generators
> import Antioch.Types
> import Antioch.DateTime
> import Antioch.Filters        (truncateHistory)
> import Antioch.Utilities
> import Antioch.Score          (elevation, radecel2ha)
> import System.Random   
> import Maybe                  (isNothing, fromJust, isJust)
> import Data.List 
> import Test.QuickCheck hiding (frequency)

This is a top level function for not just producing a set of Projects (w/
their related child Sessions), but also pre-scheduled periods of different
Session types (Fixed, Windowed, and the special case of Maintenance).
The user can specify:
   * the time range over which we want our pre-scheduled periods 
   * whether to create Maintenance periods
   * the rough ratio between Open, Fixed, and Windowed Session hours.
   * how much extra time to give to Open sessions as 'backlog'

> genSimTime :: DateTime -> Int -> Bool -> (Float, Float, Float) -> Int -> Gen [Project]
> genSimTime start days useMaint (open, fixed, windowed) backlogHrs | (open + fixed + windowed) > 1.01 = return [] 
>                                                                   | otherwise = do
>     -- create the maintenance schedule first to see how much time it is
>     maint <- genMaintenanceProj start days
>     let schedule = if useMaint then concatMap periods $ sessions maint else []
>     let maintHrs = if useMaint then (/60) . fromIntegral . sum $ map duration schedule else 0.0
>     -- the remaining time now gets split up by type
>     let simHrs = days*24
>     let hrs'' = (fromIntegral simHrs) - maintHrs
>     let hrs' = if hrs'' < 0 then 0 else hrs''
>     -- how much for fixed and windowed is straightforward
>     let fixedHrs    = round $ fixed    * hrs'
>     let windowedHrs = round $ windowed * hrs'
>     -- how much for open must include the backlog
>     let openHrs'    = round $ open     * hrs'
>     let openHrs     = openHrs' + backlogHrs
>     let (year, _, _, _, _, _) = toGregorian start
>     oProjs <- genProjectsByHrs year openHrs 
>     -- Because we must build a schedule that has no overlaps,
>     -- it's easiest to first assign the periods to the schedule,
>     -- then assign sessions & projects to these periods
>     -- First, the windowed periods; Note that we start off with
>     -- a Session Id of 1.
>     winPeriods <- genWindowedSchedule start days schedule windowedHrs
>     wProjs <- genWindowedProjects  winPeriods 1
>     let schedule' = sort $ schedule ++ (concat winPeriods)
>     -- Now, the fixed periods
>     fixedPeriods <- genFixedSchedule start days schedule' fixedHrs
>     -- Where are we at in terms of the session id?
>     let maxId = if length wProjs == 0 then 1 else maximum $ map sId $ concatMap sessions wProjs
>     fProjs <- genFixedProjects fixedPeriods (maxId+1)
>     -- Finally, put all the projects togethor
>     return $ oProjs ++ wProjs ++ fProjs ++ if useMaint then [maint] else []

Generate projects such that the sum of their allotted hours is equal to 
our greater then the specified amount.

> genProjectsByHrs :: Int -> Int -> Gen [Project]
> genProjectsByHrs _ 0 = return []
> genProjectsByHrs year hrs | hrs < 0 = genProjectsByHrs year 0
>                           | otherwise = do
>   p <- genProjectForYear year
>   pp <- genProjectsByHrs year $ hrs - ((`div` 60) . pAllottedS $ p)
>   return $ p : pp

When we create periods first, then assign Sessions/Projects to them,
as we do when creating the pre-scheduled periods (Fixed,Windowed), we 
need to make sure that those Sessions have valid ra/decs for the periods
that we've already created.
Here our strategy is to have a go at creating a randomly assigned ra/dec
to the given period - if we fail too many times, just make the source
always up and use that.

> genRaDecFromPeriod :: Period -> Gen (Radians, Radians)
> genRaDecFromPeriod p = do
>   (ra, dec) <- tryGenRaDecFromPeriod p 0
>   (cra, cdec) <- genCircumpolarRaDec
>   return $ if (isNothing ra) || (isNothing dec) then (cra, cdec) else (fromJust ra, fromJust dec)

> genCircumpolarRaDec :: Gen (Float, Float)
> genCircumpolarRaDec = do
>   ra  <- choose (0.0, 2*pi)
>   dec <- choose (60.0, 85.0)
>   return $ (ra, deg2rad dec)

Once attempts hits the tolerance level, return a failure.

> tryGenRaDecFromPeriod :: Period -> Int -> Gen (Maybe Float, Maybe Float)
> tryGenRaDecFromPeriod p attempts | attempts > 100 = return $ (Nothing, Nothing)
>                                  | otherwise = do
>     s <- skyType
>     (ra, dec) <- genRaDec s
>     if validRaDec ra dec p then return $ (Just ra, Just dec) else tryGenRaDecFromPeriod p (attempts+1)

> validRaDec :: Float -> Float -> Period -> Bool
> validRaDec ra dec p = all (==True) $ map (validRaDecTime ra dec) dts
>   where
>     dts = [(startTime p)
>         ,  (addMinutes 15 (startTime p))
>         .. (addMinutes (duration p) (startTime p))]

Horizon defined at 5 degrees.

> validRaDecTime :: Float -> Float -> DateTime -> Bool
> validRaDecTime ra dec dt = validElev $ elevation dt s'
>   where
>     s' = defaultSession {ra = ra, dec = dec}

> validElev :: Radians -> Bool
> validElev el | ((deg2rad 5.0) <= el) && (el <= (deg2rad 90.0)) = True
>              | otherwise = False

Horizon defined at 0 degrees.

> validRaDecTime' :: Float -> Float -> DateTime -> Bool
> validRaDecTime' ra dec dt = validElev $ elevation dt s'
>   where
>     s' = defaultSession {ra = ra, dec = dec}

> validElev' :: Radians -> Bool
> validElev' el | ((deg2rad 0.0) <= el) && (el <= (deg2rad 90.0)) = True
>              | otherwise = False

For a given list of periods, create appropriate projects & sessions for them.
The id is passed along so that each session can have a unique id.
For now keep it real simple - a single proj & sess for each period

> genFixedProjects :: [Period] -> Int -> Gen [Project]
> genFixedProjects [] _ = return []
> genFixedProjects (p:ps) id = do
>   proj <- genFixedProject p id
>   projs <- genFixedProjects ps (id+1)
>   return $ proj : projs --genFixedProjects ps

For a given period, create a project & fixed session for it.
The id is passed along to give the session a unique id.

> genFixedProject :: Period -> Int -> Gen Project
> genFixedProject p id = do
>   -- this proj. should be from the same semester as the period
>   let (year, _, _, _, _, _) = toGregorian . startTime $ p
>   proj'' <- genProjectForYear year
>   let proj' = proj'' { semester = dt2semester . startTime $ p }
>   let total = duration p
>   -- genSessionFixed provides default values for things like sAllottedT,
>   -- but we really want these based off the periods that are pre-generated
>   s'' <- genSessionFixed
>   (ra', dec') <- genRaDecFromPeriod p
>   -- min/max duration is randomly created for fixed sessions
>   let minDur = minDuration s''
>   let maxDur = maxDuration s''
>   let dur = duration p
>   let s' = s'' { sName = "FixedS(" ++ (show id) ++ ")"
>                , sId = id
>                , project = proj'
>                , sAllottedS = total
>                , sAllottedT = total
>                -- NOTE: here is where Dana can vary the min/max dur
>                -- but keep the period length legal
>                --, minDuration = min dur minDur
>                --, maxDuration = max dur maxDur
>                -- otherwise, min/max matches the period length
>                , minDuration = dur
>                , maxDuration = dur
>                , ra = ra' 
>                , dec = dec' 
>                }
>   let s = makeSession s' [] [p]
>   return $ makeProject proj' total total [s]

Here we randomly generate periods in the given year that DON'T cause
overlaps until we run out of time.

> genFixedSchedule :: DateTime -> Int -> [Period] -> Int -> Gen [Period]
> genFixedSchedule _ _ _ 0 = return []
> genFixedSchedule start days ps hrs | hrs < 0 = genFixedSchedule start days ps 0
>                                    | otherwise = do
>     p <- genFixedPeriod start days
>     --pp <- genFixedSchedule year ps $ hrs' p
>     --return $ p : pp
>     let ps' = sort $ ps ++ [p]
>     --case (internalConflicts (sort $ ps ++ [p])) of
>     case (internalConflicts ps') of
>       False -> do
>           pp <- genFixedSchedule start days ps' $ hrs' p
>           return $ p : pp
>       True -> genFixedSchedule start days ps hrs
>   where
>     hrs' p = hrs - ((`div` 60) . duration $ p)

Generate a period that starts with the given time range.

> genFixedPeriod :: DateTime -> Int -> Gen Period
> genFixedPeriod start days = do
>   day <- choose (1, days)
>   hour <- choose (0, 23)
>   qtrs <- choose (1*4, 8*4) -- 1 to 8 hrs 
>   let dur = qtrs * 15
>   let start = start' day hour
>   -- just to get the sem right
>   let proj = defaultProject { semester = dt2semester start }
>   let sess = defaultSession { project = proj }
>   return $ defaultPeriod { startTime = start
>                          , duration  = dur
>                          --, pState    = Scheduled
>                          , pState    = Pending
>                          , pDuration = 0
>                          , session   = sess -- just to get the sem right
>                          , pForecast = start 
>                          }
>     where
>   start' day hour = addMinutes ((day*24*60)+(hour*60)) start

Very much like genFixedSchedule, here we create a list of list of periods:
that is, each sub-list of periods should belong to a single windowed
session, and should be regularly spaced.  Just like in genFixedSchedule,
we randomly try to insert a periodic list of periods into the schedule
until we can do this without causing overlaps, then repeat the process
until we've run out of time.

> genWindowedSchedule :: DateTime -> Int -> [Period] -> Int -> Gen [[Period]]
> genWindowedSchedule _ _ _ 0 = return []
> genWindowedSchedule start days ps hrs | hrs < 0 = genWindowedSchedule start days ps 0
>                                       | otherwise = do
>     wp <- genWindowedPeriods start days
>     let ps' = sort $ ps ++ wp 
>     case (internalConflicts ps') of
>       False -> do
>           pp <- genWindowedSchedule start days ps' $ hrs' wp
>           return $! wp:pp
>       True -> genWindowedSchedule start days ps hrs
>   where
>     hrs' wp = hrs - ((`div` 60) . sum $ map duration wp)

Generates the number of windows, and the spacing between their start
dates.  Scales appropriately for how long our simulation is.
Always make sure that you don't return intervalDays larger then 
the given number of days.

> genWindowSpacing :: Int -> Gen (Int, Int)
> genWindowSpacing days | days > 65 = do
>   numWindows <- choose (3,10)
>   intervalDays <- choose (15, 60)
>   return (numWindows, intervalDays)
>                       | days > 30 && days <= 65 = do
>   numWindows <- choose (3,10)
>   intervalDays <- choose (15, days)
>   return (numWindows, intervalDays)
>                       | days > 10 && days <= 30 = do -- days < 30
>   let minDays = days `div` 4 
>   numWindows <- choose (3,10)
>   intervalDays <- choose (minDays, days)
>   return (numWindows, intervalDays)
>                       | otherwise = return (1,days) -- days < 10


Randomly generate a list of periods that are separated by a regular
interval, that fit into the given time range.  These will become the
default periods for a Windowed Session's Windows, and those windows will
be built around these periods.  Also make sure that these periods have
valid elevations for the randomly generated ra/decs of their session.

> genWindowedPeriods :: DateTime -> Int -> Gen [Period]
> genWindowedPeriods start days = do
>   -- scale the windows appropriately
>   (numWindows, intervalDays) <- genWindowSpacing days
>   -- what day number, since 'start' should the list of default periods
>   -- start on?  
>   day <- choose (2, days - 1) 
>   let dayStart = addMinutes (day*24*60) start
>   -- instead of creating the random ra/dec directly, create a whole
>   -- windowed session (w/ ra/dec) that will then get attached 
>   -- to this period
>   s <- genSessionWindowed
>   let (ra', dec') = (ra s, dec s)
>   -- for the ra/dec, produce period starttimes & durations that are
>   -- valid (produce valid elevations)
>   trs <- getValidTransitRanges dayStart intervalDays numWindows end (ra', dec')
>   return $! map (mkPeriod s) trs 
>     where
>   end = addMinutes (days*24*60) start
>   getStart day hour = addMinutes ((day*24*60)+(hour*60)) start
>   mkPeriod sess (dt, dur) = defaultPeriod { startTime = dt
>                                   , duration  = dur
>                                   , session   = sess
>                                   , pForecast = dt 
>                                   , pDuration = 0
>                                   , pState    = Pending }

For the given ra/dec, produce period starttimes & durations that are valid
(that is, they produce valid elevations), from the given number of periods
needed, and their spacing.

> getValidTransitRanges :: DateTime -> Int -> Int -> DateTime -> (Radians, Radians) -> Gen [(DateTime, Minutes)]
> getValidTransitRanges startDay intervalDays numWindows endDay (ra,dec) = do
>   mapM (getValidTransitRange (ra, dec)) $ filter (<endDay) days
>     where
>       days = [addMinutes (d*24*60) startDay | d <- [0, intervalDays .. ((numWindows-1) * intervalDays)]]

> prop_getValidTransitRanges =
>     forAll (genRaDec 'g') $ \(ra, dec) ->
>     forAll genStartTime $ \dt ->
>     let end = addHours (365*24) dt
>         trs = generate 0 g $ getValidTransitRanges dt int num end (ra, dec)
>     in (length trs) > 1
>     -- TBF why does this stronger test fail?
>     --in all (validRaDecTime' ra dec) . concat . map times $ trs
>   where 
>     g = mkStdGen 1
>     int = 15
>     num = 5
>     times (dt, dur) = [dt, (addMinutes 15 dt) .. (addMinutes dur dt)]
     
For the given ra/dec, return a time period that surrounds the transit
for this ra/dec trimmed to keep the elevations valid (source above the
horizon). Note the duration could be forced to zero.

> getValidTransitRange :: (Radians, Radians) -> DateTime -> Gen (DateTime, Minutes)
> getValidTransitRange (ra, dec) day = do
>   durQtrs <-  if haQtrs <= 0
>               then return 0
>               else choose ((1*4), min (8*4) haQtrs) -- 1 to 8 hours
>   let start = (qtrs2mins $ (-durQtrs) `div` 2) `addMinutes` dtTrans
>   let duration = qtrs2mins durQtrs
>   return (start, duration)
>     where
>       dtTrans = roundToQuarter $ lstHours2utc day (rad2hrs ra)
>       el = 5.0
>       haQtrs  = round2quarter . round . (*60.0) . rad2hrs . (*2.0) . radecel2ha (ra, dec) . deg2rad $ el
>       qtrs2mins = (*quarter)

> prop_raDecTransitsGiveValidElevations = forAll (genRaDec 'g') $ \(ra', dec') -> (validElev $  elevation (lstHours2utc dt (rad2hrs ra')) defaultSession {ra = ra', dec = dec'}) 
>     where
>       dt = fromGregorian 2010 2 2 0 0 0

Each sub-list of periods needs to get assigned a windowed session & project.
In addition, each single period needs to be within a newly created window.
For now keep it real simple - a single proj & sess for each set of periods

> genWindowedProjects :: [[Period]] -> Int -> Gen [Project]
> genWindowedProjects [] _ = return []
> genWindowedProjects (wp:wps) id = do
>   proj <- genWindowedProject wp id
>   projs <- genWindowedProjects wps (id+1)
>   return $ proj : projs --genFixedProjects ps

> genWindowedProject :: [Period] -> Int -> Gen Project
> genWindowedProject wp id = do
>   -- first generate the windows that encompass each default period
>   ws <- genWindows wp
>   -- now get the project & session all setup
>   -- the project should be from the same semester as the first window 
>   let wstart = fst . head . wRanges . head $ ws
>   let (year, _, _, _, _, _) = toGregorian wstart
>   proj'' <- genProjectForYear year
>   let sem = dt2semester wstart
>   let proj' = proj'' { pId = id, pName = "WinP", semester = sem }
>   let total = sum $ map duration wp
>   -- grab the session that was used to create the periods, since
>   -- we used it's ra/dec to determine the periods
>   let s'' = session . head $ wp
>   let (ra', dec') = (ra s'', dec s'')
>   -- randomly generated min/max durations for windowed session
>   let minDur = minDuration s''
>   let maxDur = maxDuration s''
>   -- default period durations may vary slightly, since we need
>   -- to have them always tracking a source above the horizon
>   let pMinDur = minimum $ map duration wp
>   let pMaxDur = maximum $ map duration wp
>   let s' = s'' { sName = "WinS(" ++ (show id) ++ ")"
>                , sId   = id
>                , project = proj'
>                , sAllottedS = total
>                , sAllottedT = total
>                -- NOTE: here is where Dana can choose to vary the durations 
>                --, make sure the period length is still legal
>                --, minDuration = min dur minDur 
>                --, maxDuration = max dur maxDur
>                -- otherwise, min/max matches the period lengths
>                , minDuration = pMinDur
>                , maxDuration = pMaxDur
>                , ra = ra' 
>                , dec = dec'
>                }
>   let s = makeSession s' ws wp 
>   return $ makeProject proj' total total [s]

From an (almost) evenly spaced list of periods, create the list of windows.
The periods are *not* separated by an integer number of days because
they are adjusted to cover the session's source's transit.
So, get an idea of what the spacing is between periods, use that to choose
(randomly) the gap between windows, and then use this info to create 
the window ranges surround each window.
We, by default, place each default period right near the end of it's win.


> genWindows :: [Period] -> Gen [Window]
> genWindows [] = return $ []
> genWindows ps@(ph:pt) = do
>     -- randomly choose the width of the windows (in days), somewhere
>     -- between its max. width, and something smaller.
>     winWidth <- choose (maxWinWidthDays `div` 2, maxWinWidthDays)
>     return $ map (makeWindow winWidth) ps
>   where
>     minsPerDay = 24*60
>     days = (*minsPerDay)
>     -- if there happens to be only one period, then just make the window
>     -- arbitrarrily of any size
>     psDiffDays = if pt == [] then 10 else (diffMinutes (startTime . head $ pt) (startTime ph)) `div` minsPerDay
>     -- build in a saftey buffer of two days between windows
>     maxWinWidthDays = psDiffDays - 2
>     --maxWinWidthDays = psDiffDays 

Given the default period and the window's width (in days), returns the
Window object appropriate.

> makeWindow :: Int -> Period -> Window
> makeWindow winWidth defaultPeriod = defaultWindow { wRanges = [(start, end)]
>                                                   , wTotalTime = duration defaultPeriod
>                                                   } 
>   where
>     minsPerDay = 24*60
>     days = (*minsPerDay)
>     -- give the window day more days after the period ends
>     (y, m, d) = toGregorian' $ addMinutes (days 2) (periodEndTime defaultPeriod)  
>     -- but make sure window ranges fall on integer days
>     end = fromGregorian' y m d
>     start = addMinutes (-(days winWidth)) end

Self-test to be called in unit tests and simulations.
As a general guideline, these windows should also get past 
many of the criteria in Filters.lhs: activeWindows:
   - hasTime
   - iNotComplete

> validSimulatedWindows :: Session -> Bool
> validSimulatedWindows s = all (==True) [onePeriodEach, (not . windowConflicts $ windows s), haveTime, areNotComplete]
>   where
>     onePeriodEach = all (==True) $ map (windowHasOnePeriod ps) ws   
>     hasTime w = (wTotalTime w) >= quarter
>     haveTime = all (==True) $ map hasTime ws
>     ps = periods s
>     ws = windows s
>     isNotComplete w = not . wComplete $ w
>     areNotComplete = all (==True) $ map isNotComplete ws

> windowHasOnePeriod :: [Period] -> Window -> Bool
> windowHasOnePeriod ps w = 1 == (length $ filter (==True) $ map (flip periodInWindow w) ps)

> allValidSimWindows :: [Session] -> Bool
> allValidSimWindows wss = all (==True) $ map (validSimulatedWindows) wss

Creates a maintenance project with a year's worth of pre-scheduled periods
reflecting a realistic maintenance schedule.

> genMaintenanceProj :: DateTime -> Int -> Gen Project
> genMaintenanceProj start days = do
>   let project' = mkMaintProject start
>   let session'' = mkMaintSession project'
>   mntPeriods <- genMaintenancePeriods start days session''
>   let totalTime = sum $ map duration mntPeriods
>   let session' = session'' { sAllottedT = totalTime, sAllottedS = totalTime } 
>   let session = makeSession session' [] mntPeriods
>   return $ makeProject project' totalTime totalTime [session]

Creates the appropriate periods for maintenance within the given time range.
Note that this is done by simply generating all the periods for each year
that falls in the time range, and filtering out what's not needed.

> genMaintenancePeriods :: DateTime -> Int -> Session -> Gen [Period]
> genMaintenancePeriods start days s = do
>   let end = addMinutes (days*24*60) start
>   let (lastYear, _, _, _, _, _) = toGregorian end
>   let (firstYear, _, _, _, _, _) = toGregorian start
>   ps <- mapM (genMaintenancePeriodsByYear s) [firstYear .. lastYear]
>   return $ truncateHistory (concat ps) start days
>     where
>       

Generate all the periods for maintenance in a given year.

> genMaintenancePeriodsByYear :: Session -> Int -> Gen [Period]
> genMaintenancePeriodsByYear s year = do
>   let summerMaint = createSummerMaintenance year s
>   springMaint <- genWeeklyMaintPeriods springStart springEnd s
>   fallMaint <- genWeeklyMaintPeriods fallStart fallEnd s
>   return $ nub . sort $ springMaint ++ summerMaint ++ fallMaint
>     where
>       springStart = fromGregorian year 1 1 0 0 0  
>       springEnd   = fromGregorian year 6 1 0 0 0  
>       fallStart = fromGregorian year 9 15 0 0 0  
>       fallEnd   = fromGregorian (year+1) 1 1 0 0 0  

> mkMaintProject :: DateTime -> Project
> mkMaintProject start = defaultProject { pName = "Maintenance"
>                                      , semester = dt2semester start 
>                                      , sessions = []}

> mkMaintSession :: Project -> Session
> mkMaintSession p = defaultSession { sName = "Maintenance"
>                                   , sId = 0 
>                                   , project = p
>                                   , frequency = 2.0
>                                   , band = L
>                                   , ra = 0.1
>                                   , periods = []
>                                   , dec = 1.5 -- this is just always up
>                                   , receivers = [[Rcvr1_2]]
>                                   , sType = Fixed }

For non-summer months, generate weekly maintenance periods.
NOTE: this will make an 8 hour maintenance day every 7 days - but ideally 
we want it to be randomly placed in the middle 5 days of each week.

> genWeeklyMaintPeriods :: DateTime -> DateTime -> Session -> Gen [Period]
> genWeeklyMaintPeriods start end s = return $ filter (\p -> (startTime p) < end) $ map (mkMaintPeriod s (8*60)) $ dts numWeeks
>   where
>     numWeeks = round $ (fromIntegral $ end - start) / (60*60*24*7)
>     dts numWeeks = [(week*7*24*60) `addMinutes` start | week <- [0 .. numWeeks]]

We don't care about getting the days of the week correclty, as long
as we have:
   * during the summer months we have 4 days of 10 hour maintenance followed 
     by three free days
   * during the non-summer months, one random 8 hour maintenance day in the 
     middle 5 days of each 7 days (week)

> createSummerMaintenance :: Int -> Session -> [Period]
> createSummerMaintenance year s = concatMap mkMaintWeek $ weekDts summerStart numWeeks
>   where
>     summerStart = fromGregorian year 6 1 0 0 0 
>     summerEnd   = fromGregorian year 9 1 0 0 0 
>     numWeeks = round $ (fromIntegral $ summerEnd - summerStart) / (60*60*24*7)
>     weekDts start numWeeks = [(week*7*24*60) `addMinutes` start | week <- [0 .. numWeeks]]
>     dayDts start = [(day*24*60) `addMinutes` start | day <- [1 .. 4]]
>     mkMaintWeek start = map (mkMaintPeriod s (10*60)) $ dayDts start

> mkMaintPeriod :: Session -> Minutes -> DateTime -> Period
> mkMaintPeriod s dur date = defaultPeriod {startTime = start
>                                         , session = s
>                                         , duration = dur
>                                         , pForecast = start 
>                                         , pState = Scheduled 
>                                          }
>   where
>     -- all maintenance periods start at 8 AM ET
>     (year, month, day, _, _, _) = toGregorian date
>     start = fromGregorian year month day (8+4) 0 0 -- UTC!

