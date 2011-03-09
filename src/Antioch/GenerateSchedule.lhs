> module Antioch.GenerateSchedule where

> import Antioch.Generators
> import Antioch.Types
> import Antioch.DateTime
> import Antioch.Filters        (filterHistory)
> import Antioch.Utilities      (periodInWindow, dt2semester, deg2rad)
> import Antioch.Score          (elevation)
> import Maybe                  (isNothing, fromJust)
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
>     -- Becasuse we must build a schedule that has no overlaps,
>     -- it's easiest to first assign the periods to the schedule,
>     -- then assign sessions & projects to these periods
>     -- First, the windowed periods; Note that we start off with
>     -- a Session Id of 1.
>     winPeriods <- genWindowedSchedule start days schedule windowedHrs
>     wProjs <- genWindowedProjects  winPeriods 1 --genWindowedProjects winPeriods
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

> validRaDecTime :: Float -> Float -> DateTime -> Bool
> validRaDecTime ra dec dt = validElev $ elevation dt s'
>   where
>     s' = defaultSession {ra = ra, dec = dec}

> validElev :: Radians -> Bool
> validElev el | ((deg2rad 5.0) <= el) && (el <= (deg2rad 90.0)) = True
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
>   -- TBF: genSessionFixed will figure things like sAllottedT, but we
>   -- really want these based off the periods that are pre-generated
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
>                , dec = dec' -- this is just always up
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
>                          , pState    = Scheduled
>                          , session   = sess -- just to get the sem right
>                          , pForecast = start 
>                          }
>     where
>   start' day hour = addMinutes ((day*24*60)+(hour*60)) start

Very much like genFixedSchedule, here we create a list of list of periods: that is, 
each sub-list of periods should belong to a single windowed session, and should be
regularly spaced.  Just like in genFixedSchedule, we randomly try to insert 
a periodic list of periods into the schedule until we can do this without causing
overlaps, then repeat the process until we've run out of time.

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

Randomly generate a list of periods that are separated by a regular interval, that
fit into the given time range.

> genWindowedPeriods :: DateTime -> Int -> Gen [Period]
> genWindowedPeriods start days = do
>   numWindows <- choose (3,10)
>   intervalDays <- choose (15, 60)
>   let sizeDays' = numWindows * intervalDays
>   let sizeDays = if (sizeDays' >= days) then days - 3 else sizeDays'
>   day <- choose (1, days - 1) --sizeDays)
>   hour <- choose (0, 23)
>   qtrs <- choose (1*4, 8*4) -- 1 to 8 hrs
>   let duration = qtrs * 15
>   let firstStart = getStart day hour
>   let dts = filter (<end) $ [addMinutes (d*24*60) firstStart | d <- [0, intervalDays .. (numWindows * intervalDays)]]
>   return $! map (mkPeriod duration) dts 
>     where
>   end = addMinutes (days*24*60) start
>   getStart day hour = addMinutes ((day*24*60)+(hour*60)) start
>   mkPeriod dur dt = defaultPeriod { startTime = dt
>                                   , duration  = dur
>                                   , pForecast = dt 
>                                   , pState    = Pending }

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
>   -- the project should be from the same semester as the first periods
>   let (year, _, _, _, _, _) = toGregorian . startTime . head $ wp
>   proj'' <- genProjectForYear year
>   let sem = dt2semester . startTime . head $ wp
>   let proj' = proj'' { pName = "WinP", semester = sem }
>   let total = sum $ map duration wp
>   s'' <- genSessionWindowed
>   -- randomly generated min/max durations for windowed session
>   let minDur = minDuration s''
>   let maxDur = maxDuration s''
>   let dur = duration . head $ wp -- they all have the same length
>   -- TBF: technically, we need to find an ra/dec that is valid for 
>   -- all periods, but I hope the LST drift doesn't get us off too bad.
>   (ra', dec') <- genRaDecFromPeriod (head wp)
>   let s' = s'' { sName = "WinS(" ++ (show id) ++ ")"
>                , sId   = id
>                , project = proj'
>                , sAllottedS = total
>                , sAllottedT = total
>                -- NOTE: here is where Dana can choose to vary the durations 
>                --, make sure the period length is still legal
>                --, minDuration = min dur minDur 
>                --, maxDuration = max dur maxDur
>                -- otherwise, min/max matches the period length
>                , minDuration = dur
>                , maxDuration = dur
>                , ra = ra' 
>                , dec = dec'
>                }
>   ws <- genWindows wp
>   let s = makeSession s' ws wp 
>   return $ makeProject proj' total total [s]

From an evenly spaced list of periods, create the list of windows

> genWindows :: [Period] -> Gen [Window]
> genWindows [] = return $ []
> genWindows ps@(ph:pt) = do
>     durDays <- choose (div maxWidthDays 2, maxWidthDays)
>     let dur = 24*60*durDays
>     let (phYear, phMonth, phDay, _, _, _) = toGregorian . startTime $ ph
>     let dayStart = fromGregorian phYear phMonth phDay 0 0 0
>     let dts = [addMinutes (-dur) $ addMinutes (pDiff*pi) dayStart | pi <- [0..numPs - 1]] 
>     return $ map (mkWindow dur) dts
>   where
>     days = (*(24*60))
>     pDiff = if pt == [] then days 10 else diffMinutes (startTime . head $ pt) (startTime ph)
>     -- a window can't be more then one day less then the separation between
>     maxWidthDays = (pDiff - days 2) `div` (24*60)
>     numPs = length ps
>     mkWindow dur dt = defaultWindow {wRanges = [(dt, addMinutes (dur + days 2) dt)]}

Self-test to be called in unit tests and simulations

> validSimulatedWindows :: Session -> Bool
> validSimulatedWindows s = onePeriodEach && (not . windowConflicts $ windows s)
>   where
>     onePeriodEach = all (==True) $ map (windowHasOnePeriod ps) ws   
>     ps = periods s
>     ws = windows s

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
>   return $ filterHistory (concat ps) start days
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
>                                      , semester = sem sem'
>                                      , sessions = []}
>   where
>     (year, _, _, _, _, _) = toGregorian start 
>     sem' = (show $ year - 2000) ++ "A"
>     sem s = if (length s) == 2 then "0" ++ s else s
>      

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

