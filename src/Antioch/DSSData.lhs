> module Antioch.DSSData where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Score
> import Antioch.Utilities (hrs2rad, deg2rad)
> import Data.List (groupBy, sort)
> import Data.Char (toUpper)
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
>     return $ makeProject project (timeTotal project) sessions

> getSessions :: Int -> Connection -> IO [Session]
> getSessions projId cnn = handleSqlError $ do 
>   result <- quickQuery' cnn query xs 
>   return $ toSessionDataList result
>     where
>       query = "SELECT sessions.id, sessions.name, sessions.min_duration, sessions.max_duration, sessions.time_between, sessions.frequency, allotment.total_time, allotment.grade, targets.horizontal, targets.vertical, status.enabled, status.authorized, status.backup, session_types.type FROM sessions, allotment, targets, status, session_types WHERE allotment.id = sessions.allotment_id AND targets.session_id = sessions.id AND sessions.status_id = status.id AND sessions.session_type_id = session_types.id AND sessions.project_id = ?"
>       xs = [toSql projId]
>       toSessionDataList = map toSessionData
>       toSessionData (id:name:mind:maxd:between:freq:time:fltGrade:h:v:e:a:b:sty:[]) = 
>         defaultSession {
>             sId = fromSql id 
>           , sName = fromSql name
>           , frequency   = fromSql freq
>           , minDuration = (*60) $ fromSqlInt mind
>           , maxDuration = (*60) $ fromSqlInt maxd
>           , timeBetween = (*60) $ fromSqlInt between
>           , totalTime   = (*60) $ fromSql time 
>           , ra = hrs2rad . fromSql $ h -- TBF: assume all J200? For Carl's DB, YES!
>           , dec = deg2rad . fromSql $ v 
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

> populateSession :: Connection -> Session -> IO Session
> populateSession cnn s = do
>     ps <- getPeriods cnn s
>     return $ makeSession s ps

> getPeriods :: Connection -> Session -> IO [Period]
> getPeriods cnn s = do
>     dbPeriods <- fetchPeriods cnn s 
>     optPeriods <- periodsFromOpts cnn s
>     return $ sort $ dbPeriods ++ optPeriods

TBF: no Period table in the DB yet.

> fetchPeriods :: Connection -> Session -> IO [Period]
> fetchPeriods cnn s = return []

> periodsFromOpts :: Connection -> Session -> IO [Period]
> periodsFromOpts cnn s = return [] 
