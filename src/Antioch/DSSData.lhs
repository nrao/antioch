> module Antioch.DSSData where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Score
> import Antioch.Utilities (hrs2rad, deg2rad, printList)
> import Data.List (groupBy, sort)
> import Data.Char (toUpper)
> import Maybe (fromJust)
> import Database.HDBC
> import Database.HDBC.PostgreSQL

> connect :: IO Connection
> connect = handleSqlError $ connectPostgreSQL "dbname=dss_pmargani user=dss"

> getProjects :: IO [Project]
> getProjects = do
>     cnn <- connect
>     projs' <- fetchProjectData cnn
>     projs <- mapM (populateProject cnn) projs' 
>     return projs

> fetchProjectData :: Connection -> IO [Project]
> fetchProjectData cnn = handleSqlError $ do
>   result <- quickQuery' cnn query []
>   return $ toProjectDataList result
>     where
>       query = "SELECT projects.id, projects.pcode, semesters.semester, projects.thesis, allotment.total_time FROM semesters, allotment, projects, projects_allotments WHERE semesters.id = projects.semester_id AND projects.id = projects_allotments.project_id AND allotment.id = projects_allotments.allotment_id ORDER BY projects.pcode"
>       toProjectDataList = map toProjectData
>       toProjectData (id:pcode:semester:thesis:time:[]) = 
>         defaultProject {
>             pId = fromSql id 
>           , pName = fromSql pcode 
>           , semester = fromSql semester  
>           , thesis = fromSql thesis 
>           , timeTotal = (*60) $ fromSql time 
>         }

> populateProject :: Connection -> Project -> IO Project
> populateProject cnn project = do
>     sessions' <- getSessions (pId project) cnn
>     sessions <- mapM (populateSession cnn) sessions'
>     -- TBF: only for 09B! Then get observer blackouts!
>     blackouts <- getProjectBlackouts (pId project) cnn 
>     let project' = project { pBlackouts = blackouts }
>     return $ makeProject project' (timeTotal project') sessions --blackouts

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

TBF: if a session is missing any of the tables in the below query, it won't
get picked up!!!

> getSessions :: Int -> Connection -> IO [Session]
> getSessions projId cnn = handleSqlError $ do 
>   result <- quickQuery' cnn query xs 
>   let ss' = toSessionDataList result
>   ss <- mapM (updateRcvrs cnn) ss' 
>   return ss
>     where
>       query = "SELECT sessions.id, sessions.name, sessions.min_duration, sessions.max_duration, sessions.time_between, sessions.frequency, allotment.total_time, allotment.grade, targets.horizontal, targets.vertical, status.enabled, status.authorized, status.backup, session_types.type FROM sessions, allotment, targets, status, session_types WHERE allotment.id = sessions.allotment_id AND targets.session_id = sessions.id AND sessions.status_id = status.id AND sessions.session_type_id = session_types.id AND sessions.project_id = ?"
>       xs = [toSql projId]
>       toSessionDataList = map toSessionData
>       toSessionData (id:name:mind:maxd:between:freq:time:fltGrade:h:v:e:a:b:sty:[]) = 
>         defaultSession {
>             sId = fromSql id 
>           , sName = fromSql name
>           , frequency   = fromSql freq
>           , minDuration = fromSqlMinutes mind
>           , maxDuration = fromSqlMinutes maxd
>           , timeBetween = fromSqlMinutes between
>           , totalTime   = fromSqlMinutes time 
>           , ra = fromSql h -- TBF: assume all J200? For Carl's DB, YES!
>           , dec = fromSql v  
>           , grade = toGradeType fltGrade 
>           , receivers = [] -- TBF: does scoring support the logic structure!
>           , periods = [] -- TBF, no history in Carl's DB
>           , enabled = fromSql e
>           , authorized = fromSql a
>           , backup = fromSql b
>           , band = deriveBand $ fromSql freq
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
> toGradeType val = if (fromSql val) == (3.0 :: Float) then GradeA else GradeB 

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
> toRcvrType s val = if (fromSql val) == ("Rcvr18_26" :: String) then findRcvr18_26 s else read . fromSql $ val
>   where
>     findRcvr18_26 s = if frequency s < 22.0 then Rcvr18_22 else Rcvr22_26 

> populateSession :: Connection -> Session -> IO Session
> populateSession cnn s = do
>     ps <- getPeriods cnn s
>     return $ makeSession s ps

> getPeriods :: Connection -> Session -> IO [Period]
> getPeriods cnn s = do
>     dbPeriods <- fetchPeriods cnn s 
>     optPeriods <- periodsFromOpts cnn s
>     return $ sort $ dbPeriods ++ optPeriods

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
>   --let r = toPeriodList result
>   --print r
>   --return r
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
