> module Antioch.DSSData where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Score
> import Antioch.Reservations
> import Antioch.Utilities (hrs2rad, deg2rad, printList)
> import Antioch.Settings (dssDataDB)
> import Data.List (groupBy, sort, nub)
> import Data.Char (toUpper)
> import Maybe (fromJust)
> import Database.HDBC
> import Database.HDBC.PostgreSQL

> connect :: IO Connection
> connect = handleSqlError $ connectPostgreSQL cnnStr 
>   where
>     cnnStr = "dbname=" ++ dssDataDB ++ " user=dss"

> getProjects :: IO [Project]
> getProjects = do
>     cnn <- connect
>     projs' <- fetchProjectData cnn
>     projs <- mapM (populateProject cnn) projs' 
>     return projs

TBF: get rid of the allotment table join here, and refactor it out to a 
separate query, to deal with multiple allotments (different grades)

> fetchProjectData :: Connection -> IO [Project]
> fetchProjectData cnn = handleSqlError $ do
>   result <- quickQuery' cnn query []
>   return $ toProjectDataList result
>     where
>       query = "SELECT p.id, p.pcode, s.semester, p.thesis, p.complete FROM semesters AS s, projects AS p WHERE s.id = p.semester_id ORDER BY p.pcode"
>       toProjectDataList = map toProjectData
>       toProjectData (id:pcode:semester:thesis:comp:[]) = 
>         defaultProject {
>             pId = fromSql id 
>           , pName = fromSql pcode 
>           , semester = fromSql semester  
>           , thesis = fromSql thesis 
>           , pClosed = fromSql comp
>         }

> populateProject :: Connection -> Project -> IO Project
> populateProject cnn project = do
>     sessions' <- getSessions (pId project) cnn
>     sessions <- mapM (populateSession cnn) sessions'
>     -- TBF: only for 09B! Then get observer blackouts!
>     blackouts <- getProjectBlackouts (pId project) cnn 
>     let project' = project { pBlackouts = blackouts }
>     -- project times
>     allotments <- getProjectAllotments (pId project') cnn
>     let project'' = setProjectAllotments project' allotments
>     -- project observers (will include observer blackouts!)
>     observers <- getProjectObservers (pId project) cnn
>     let project''' = setProjectObservers project'' observers
>     return $ makeProject project'' (pAlloted project''') sessions 

The scheduling algorithm does not need to know all the details about the observers
on a project - it only needs a few key facts, which are defined in the Observer
data structure.  These facts come from two sources:
   1. DSS Database:
      * observer sanctioned flag 
      * observer black out dates
   2. BOS web service:
      * observer on site dates (GB reservation date)

TBF: currently no observer black out date tables
TBF: We currently cannot link info in the DSS database to the id's used in the
BOS web services to retrieve reservation dates.

> getProjectObservers :: Int -> Connection -> IO [Observer]
> getProjectObservers projId cnn = handleSqlError $ do
>     -- 0. TBF: get the usernames (or other ID?) associated with this project
>     -- 1. Use these to lookup the needed info from the BOS web service.
>     -- obs <- getReservationInfo obs'
>     -- 1. Use these to lookup the needed info from the DSS database
>     -- obs' <- get
>     -- return obs
>     return []

> setProjectObservers :: Project -> [Observer] -> Project
> setProjectObservers proj obs = proj { observers = obs }


TBF: Let's say it again.  This is only for scheduling 09B.  Then we'll
want to ditch this, and get the observer blackouts.

> getProjectBlackouts :: Int -> Connection -> IO [DateRange]
> getProjectBlackouts projId cnn = handleSqlError $ do 
>   result <- quickQuery' cnn query xs 
>   return $ toBlackoutList result 
>     where
>       query = "SELECT pb.start_date, pb.end_date FROM project_blackouts_09b AS pb WHERE pb.project_id = ?"
>       xs = [toSql projId]
>       toBlackoutList = map toDateRange
>       toDateRange (start:end:[]) = (sqlToDateTime start, sqlToDateTime end)

We must query for the allotments separately, because if a Project has alloted
time for more then one grade (ex: 100 A hrs, 20 B hrs), then that will be
two allotments, and querying w/ a join will duplicate the project.

> getProjectAllotments :: Int -> Connection -> IO [(Minutes, Grade)]
> getProjectAllotments projId cnn = handleSqlError $ do 
>   result <- quickQuery' cnn query xs 
>   return $ toAllotmentList result 
>     where
>       query = "SELECT a.total_time, a.grade FROM allotment AS a, projects AS p, projects_allotments AS pa WHERE p.id = pa.project_id AND a.id = pa.allotment_id AND p.id = ?"
>       xs = [toSql projId]
>       toAllotmentList = map toAllotment
>       toAllotment (time:fltGrade:[]) = (fromSqlMinutes time, toGradeType fltGrade)

TBF: WTF! In Antioch, do we need to be taking into account grades at the 
project level?  For now, we are ignoring grade and summing the different
hours togethor to get the total time.

> setProjectAllotments :: Project -> [(Minutes, Grade)] -> Project
> setProjectAllotments p [] = p
> setProjectAllotments p (x:xs) = setProjectAllotments (p {pAlloted = (pAlloted p) + (fst x)} ) xs

TBF: if a session is missing any of the tables in the below query, it won't
get picked up!!!
TBF, BUG: Session (17) BB261-01 has no target, so is not getting imported.

> getSessions :: Int -> Connection -> IO [Session]
> getSessions projId cnn = handleSqlError $ do 
>   result <- quickQuery' cnn query xs 
>   let ss' = toSessionDataList result
>   ss <- mapM (updateRcvrs cnn) ss' 
>   return ss
>     where
>       query = "SELECT s.id, s.name, s.min_duration, s.max_duration, s.time_between, s.frequency, a.total_time, a.grade, t.horizontal, t.vertical, st.enabled, st.authorized, st.backup, st.complete, type.type FROM sessions AS s, allotment AS a, targets AS t, status AS st, session_types AS type WHERE a.id = s.allotment_id AND t.session_id = s.id AND s.status_id = st.id AND s.session_type_id = type.id AND s.project_id = ?"
>       xs = [toSql projId]
>       toSessionDataList = map toSessionData
>       toSessionData (id:name:mind:maxd:between:freq:time:fltGrade:h:v:e:a:b:c:sty:[]) = 
>         defaultSession {
>             sId = fromSql id 
>           , sName = fromSql name
>           , frequency   = fromSql freq
>           , minDuration = fromSqlMinutes mind
>           , maxDuration = fromSqlMinutes maxd
>           , timeBetween = fromSqlMinutes between
>           , sAlloted    = fromSqlMinutes time 
>           , ra = fromSql h -- TBF: assume all J200? For Carl's DB, YES!
>           , dec = fromSql v  
>           , grade = toGradeType fltGrade 
>           , receivers = [] -- TBF: does scoring support the logic structure!
>           , periods = [] -- TBF, no history in Carl's DB
>           , enabled = fromSql e
>           , authorized = fromSql a
>           , backup = fromSql b
>           , band = deriveBand $ fromSql freq
>           , sClosed = fromSql c
>           , sType = toSessionType sty
>         }
>        -- TBF: need to cover any other types?

Since the Session data structure does not support Nothing, when we get NULLs
from the DB (Carl didn't give it to us), then we need some kind of default
value of the right type.

> fromSqlInt SqlNull = 0
> fromSqlInt x       = fromSql x

> fromSqlMinutes         :: SqlValue -> Minutes
> fromSqlMinutes SqlNull = 0
> fromSqlMinutes x       = sqlHrsToMinutes x

> sqlHrsToHrs' :: SqlValue -> Float
> sqlHrsToHrs' hrs = fromSql hrs

> hrsToMinutes :: Float -> Minutes
> hrsToMinutes hrs = floor $ 60.0 * hrs

> sqlHrsToMinutes :: SqlValue -> Minutes
> sqlHrsToMinutes hrs = hrsToMinutes . sqlHrsToHrs' $ hrs


TBF: is this totaly legit?  and should it be somewhere else?

> deriveBand :: Float -> Band
> deriveBand freq | freq <= 2.0                  = L
> deriveBand freq | freq > 2.00 && freq <= 3.95  = S
> deriveBand freq | freq > 3.95 && freq <= 5.85  = C
> deriveBand freq | freq > 5.85 && freq <= 8.00  = X
> deriveBand freq | freq > 8.00 && freq <= 10.0  = U
> deriveBand freq | freq > 12.0 && freq <= 15.4  = A
> deriveBand freq | freq > 18.0 && freq <= 26.0  = K
> deriveBand freq | freq > 26.0 && freq <= 40.0  = Q
> deriveBand freq | freq > 40.0 && freq <= 50.0  = S
> deriveBand freq | otherwise = W -- shouldn't get any of these!

> toSessionType :: SqlValue -> SessionType
> toSessionType val = read . toUpperFirst $ fromSql val
>   where
>     toUpperFirst x = [toUpper . head $ x] ++ tail x

> toGradeType :: SqlValue -> Grade
> toGradeType val = if (fromSql val) == (3.0 :: Float) then GradeB else GradeA 

Given a Session, find the Rcvrs for each Rcvr Group.
This is a separate func, and not part of the larger SQL in getSessions
in part because if there are *no* rcvrs, that larger SQL would not return
*any* result (TBF: this bug is still there w/ the tragets)

> updateRcvrs :: Connection -> Session -> IO Session
> updateRcvrs cnn s = do
>   rcvrGroups <- getRcvrGroups cnn s
>   cnfRcvrs <- mapM (getRcvrs cnn s) rcvrGroups
>   return $ s {receivers = cnfRcvrs}

> getRcvrGroups :: Connection -> Session -> IO [Int]
> getRcvrGroups cnn s = do
>   result <- quickQuery' cnn query xs 
>   return $ toRcvrGrpIds result
>   where
>     xs = [toSql . sId $ s]
>     query = "SELECT rg.id FROM receiver_groups AS rg WHERE rg.session_id = ?"
>     toRcvrGrpIds = map toRcvrGrpId 
>     toRcvrGrpId [x] = fromSql x

> getRcvrs :: Connection -> Session -> Int -> IO ReceiverGroup
> getRcvrs cnn s id = do
>   result <- quickQuery' cnn query xs 
>   return $ toRcvrList s result
>   where
>     xs = [toSql id]
>     query = "SELECT r.name FROM receivers as r, receiver_groups_receivers as rgr WHERE rgr.receiver_id = r.id AND rgr.receiver_group_id = ?"
>     toRcvrList s = map (toRcvr s)
>     toRcvr s [x] = toRcvrType s x

TBF: is what we'ere doing here w/ the rcvr and frequency legal?

> toRcvrType :: Session -> SqlValue -> Receiver
> toRcvrType s val = read . fromSql $ val
> --toRcvrType s val = if (fromSql val) == ("Rcvr18_26" :: String) then findRcvr18_26 s else read . fromSql $ val
>   where
> --    findRcvr18_26 s = if frequency s < 22.0 then Rcvr18_22 else Rcvr22_26 

Here, we gather additional information about a session: opportunities, periods,
observing parameters, etc.
TBF: why aren't we getting the rcvr info here?

> populateSession :: Connection -> Session -> IO Session
> populateSession cnn s = do
>     s' <- setObservingParameters cnn s
>     ps <- getPeriods cnn s'
>     return $ makeSession s' ps

The following recursive patterns work for setting the observing params
that are one-to-one between the DB and the Session (ex: Night Time -> low rfi).  However,
we'll need to handle as a special case some params that we want to take from
the DB and collapse into simpler Session params (ex: LST ranges).

> setObservingParameters :: Connection -> Session -> IO Session
> setObservingParameters cnn s = do
>   result <- quickQuery' cnn query xs 
>   --return $ setObservingParameters' s result
>   let s' = setObservingParameters' s result
>   s'' <- setLSTExclusion cnn s'
>   return s''
>     where
>       xs = [toSql . sId $ s]
>       query = "select p.name, p.type, op.string_value, op.integer_value, op.float_value, op.boolean_value, op.datetime_value from observing_parameters as op, parameters as p WHERE p.id = op.parameter_id AND op.session_id = ?" 

> setObservingParameters' :: Session -> [[SqlValue]] -> Session
> setObservingParameters' s sqlRows = foldl setObservingParameter s sqlRows 

TBF: for now, just set:
   * low rfi flag

> setObservingParameter :: Session -> [SqlValue] -> Session
> setObservingParameter s (pName:pType:pStr:pInt:pFlt:pBool:pDT) | n == "Night-time Flag" = s { lowRFI = fromSql pBool }    
>                                                                | otherwise = s
>   where
>     n = fromSql pName

The DB's observing parameters may support both LST Exclusion flags *and*
LST Inclusion flags, where as our Session's only support the LST Exclusion
flags - so we'll have to collapse the DB's 2 types into our 1.

> setLSTExclusion :: Connection -> Session -> IO Session
> setLSTExclusion cnn s = do
>   result <- quickQuery' cnn query xs --Exclusion
>   let s' = addLSTExclusion' True s result
>   result <- quickQuery' cnn query' xs --Inclusion
>   return $ addLSTExclusion' False s' result
>     where
>       -- TBF: for some reason, I need to have 'Exclude' & 'Include' in the
>       -- query strings: putting it in xs causes an SQL error ???
>       xs = [toSql . sId $ s]
>       query = "select p.name, op.float_value from observing_parameters as op, parameters as p WHERE p.id = op.parameter_id AND p.name LIKE 'LST Exclude%' AND op.session_id = ?" 
>       query' = "select p.name, op.float_value from observing_parameters as op, parameters as p WHERE p.id = op.parameter_id AND p.name LIKE 'LST Include%' AND op.session_id = ?" 

The 'ex' flag determines whether we are importing LST Exclusion ranges
or Inclusion ranges.

> addLSTExclusion' :: Bool -> Session -> [[SqlValue]] -> Session
> addLSTExclusion' _ s []        = s
> addLSTExclusion' ex s sqlValues = s { lstExclude = (lstExclude s) ++ [lstRange ex sqlValues] }  

If we are importing the inclusion range, then reversing the endpoints makes
it an exclusion range.

> lstRange :: Bool -> [[SqlValue]] -> (Float, Float)
> lstRange ex sqlValues = if ex then (low, hi) else (hi, low) 
>   where
>     (low, hi) = lstRangeLow ex sqlValues $ lstRangeHi ex sqlValues

> lstRangeHi :: Bool -> [[SqlValue]] -> Float
> lstRangeHi ex sqlValues = lstRangeHi' . head $ filter (isLSTName n) sqlValues
>   where
>     n = if ex then "LST Exclude Hi" else "LST Include Hi"
>     lstRangeHi' (pName:pHi:[]) = fromSql pHi

> isLSTName :: String -> [SqlValue] -> Bool
> isLSTName name (pName:pFloat:[]) = (fromSql pName) == name

> lstRangeLow :: Bool -> [[SqlValue]] -> Float -> (Float, Float)
> lstRangeLow ex sqlValues hiValue = (lowValue, hiValue)
>   where
>     n = if ex then "LST Exclude Low" else "LST Include Low"
>     lowValue = lstRangeLow' . head $ filter (isLSTName n) sqlValues
>     --isLow (pName:pFloat:[]) = (fromSql pName) == "LST Exclude Low"
>     lstRangeLow' (pName:pLow:[]) = fromSql pLow

Two ways to get Periods from the DB:
   * The Periods Table: this is a history of what ever has been scheduled 
   * The Opportunities Table: this is currently how a fixed session's period
     "to be scheduled" is saved - as a single opportunity in a single window.
     However, if we run overlapping simulations, we may have opportunities
     that have already been created into period table records.  So, we'll
     need to ignore these.

> getPeriods :: Connection -> Session -> IO [Period]
> getPeriods cnn s = do
>     dbPeriods <- fetchPeriods cnn s 
>     optPeriods <- periodsFromOpts cnn s
>     -- the 'nub' removes opportunities that may alreay have been represented
>     -- in the period table
>     -- NOTE: remember that equality between Periods only relies on 
>     -- Session ID, start, and duration.
>     return $ sort . nub $ dbPeriods ++ optPeriods

> fetchPeriods :: Connection -> Session -> IO [Period]
> fetchPeriods cnn s = do 
>   result <- quickQuery' cnn query xs 
>   return $ toPeriodList result
>   where
>     xs = [toSql . sId $ s]
>     query = "SELECT id, session_id, start, duration, score, forecast, backup FROM periods WHERE session_id = ?"
>     toPeriodList = map toPeriod
>     toPeriod (id:sid:start:durHrs:score:forecast:backup:[]) =
>       defaultPeriod { startTime = sqlToDateTime start --fromSql start
>                     , duration = fromSqlMinutes durHrs
>                     , pScore = fromSql score
>                     , pForecast = fromSql forecast
>                     , pBackup = fromSql backup
>                     }

> sqlToDateTime :: SqlValue -> DateTime
> sqlToDateTime dt = fromJust . fromSqlString . fromSql $ dt

Opportunities for Fixed Sessions should be honored via Periods

> periodsFromOpts :: Connection -> Session -> IO [Period]
> periodsFromOpts cnn s = periodsFromOpts' cnn s -- TBF: ignore types for now!!!
> {-
> periodsFromOpts cnn s | sType s == Open = return [] 
>                       | sType s == Windowed = return [] 
>                       | sType s == Fixed = periodsFromOpts' cnn s
> -}

> periodsFromOpts' :: Connection -> Session -> IO [Period]
> periodsFromOpts' cnn s = do
>   result <- quickQuery' cnn query xs 
>   return $ toPeriodList result
>   where
>     xs = [toSql . sId $ s]
>     query = "SELECT opportunities.window_id, windows.required, opportunities.start_time, opportunities.duration FROM windows, opportunities where windows.id = opportunities.window_id and windows.session_id = ?"
>     toPeriodList = map toPeriod
>     toPeriod (wid:wreq:start:durHrs:[]) = 
>       defaultPeriod { startTime = sqlToDateTime start --fromSql start
>                     , duration = fromSqlMinutes durHrs
>                     , pForecast = sqlToDateTime start -- undefined is bad!
>                     }

Write Telescope Periods to the database.
TBF: there are no checks here to make sure we aren't adding periods that
are already in the DB.

> putPeriods :: [Period] -> IO ()
> putPeriods ps = do
>   cnn <- connect
>   result <- mapM (putPeriod cnn) ps
>   commit cnn

> putPeriod :: Connection -> Period -> IO [[SqlValue]] 
> putPeriod cnn p = do
>   quickQuery' cnn query xs 
>     where
>       xs = [toSql . sId . session $ p
>           , toSql $ (toSqlString . startTime $ p) 
>           , minutesToSqlHrs . duration $ p
>           , toSql . pScore $ p
>           , toSql . toSqlString . pForecast $ p
>           , toSql . pBackup $ p
>             ]
>       query = "INSERT INTO periods VALUES (DEFAULT, ?, ?, ?, ?, ?, ?);"

> minutesToSqlHrs :: Minutes -> SqlValue
> minutesToSqlHrs mins = toSql $ (/(60.0::Float)) . fromIntegral $ mins 
