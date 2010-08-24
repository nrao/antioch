> module Antioch.RunHistWeatherOpt (runHistWeatherOpt, runFillStringencyOpt) where

> import Antioch.DateTime
> import Antioch.HistoricalWeather
> import Antioch.HistWeatherOpt
> import Antioch.Settings     (weatherDB)
> import System.Cmd
> import System.Environment
> import System.Directory
> import Data.Maybe
> import System.Posix
> import Database.HDBC
> import Database.HDBC.PostgreSQL
> import System.CPUTime

This high-level module is the top level for a 'poor man's parallel' version
of fillStringencyTable.  Remember that for each receiver, freq, elevation,
and observing type, it's stringency value is the ratio of all the timestamps we
have data for, over the number of timestamps were certain scoring limits
pass.

We experimented with breaking this giant calculation up by timestamp, observing
type, frequency (w/ receiver), and discovered that only by breaking it up by
timestamp could we get improved results.  Unfortunately, unlike for the other
varibles, when calculating the stringencies over different timestamps, we must
then combine the final results, which accounts for most of the below code.

The basic idea follows the MapReduce pattern:  
Map - calculate stringencies over different time periods independently
Reduce - when all calcs are finished, combine the results (average them)

Specifically, this means that we fire off the programs for calcing the string's
over different time periods using system calls, wait for the temporary files
that they produce to be completed, then parse the files, average the results,
and stick them in the DB.

It's a lot of code, but it's simple and it works: using just two cores, one
can calculate stringencies over a year in just 5.5 hours (an improvement over
24 hours).

> runHistWeatherOpt start end numCores = do
>   print $ "Updating historical weather in " ++ (show weatherDB)
>   cnn <- handleSqlError $ connectDB
>   print "truncating table t_sys"
>   truncateTable cnn "t_sys"
>   print "filling table t_sys"
>   fillTsysTable cnn
>   print "truncating table stringency"
>   truncateTable cnn "stringency"
>   disconnect cnn
>   runFillStringencyOpt start end numCores
>   print $ "Historical weather update complete."

> runFillStringencyOpt start end numCores = do
>   startTime <- getCurrentTime
>   -- split this up over the number of cores we think we can use
>   let dtRanges = getDateRanges start end numCores
>   print $ map (\(a, b) -> (toSqlString a, toSqlString b)) dtRanges
>   -- remove the previous files
>   let cmd = "rm -rf stringency*.txt"
>   print cmd
>   system cmd 
>   -- fire off the different threads: MAP
>   mapM runHistWeatherProcess dtRanges
>   -- wait till the files show up
>   waitForFiles dtRanges
>   print "files finally showed up!"
>   -- parse the files and combine results: REDUCE
>   -- and write these results to the DB
>   startTime2 <- getCurrentTime
>   reduceResult $ getFileNames dtRanges
>   endTime <- getCurrentTime
>   print $ "overall execution time (secs): " ++ (show $ endTime - startTime)
>   print $ "parsing execution time (secs): " ++ (show $ endTime - startTime2)

Split up the given time range by n.

> getDateRanges :: DateTime -> DateTime -> Int -> [(DateTime, DateTime)]
> getDateRanges start end numRanges = [(addMinutes' (i*intervalMins) start,addMinutes' ((i+1)*intervalMins) start) | i <- [0 .. numRanges - 1]]
>   where
>     intervalMins = (diffMinutes' end start) `div` numRanges

Fire off a process for filling stringincies using the given time range by
making a system call.

> runHistWeatherProcess :: (DateTime, DateTime) -> IO ()
> runHistWeatherProcess (start, end) = do
>     let cmd = "genstring \"" ++ (toSqlString start) ++ "\" \"" ++  (toSqlString end)  ++ "\" &"
>     print cmd
>     system cmd
>     print "fired off cmd"
>     

> getFileNames dts = map (getFileName . fst) dts

-------- Reduce the separate results ---------------------

Don't return until the temporary files appear and contain the expected
number of lines.

> waitForFiles dts = do
>   there <- filesReady $ getFileNames dts
>   case there of
>       True -> return ()
>       False -> do
>           print "waiting ..."
>           sleep 5
>           waitForFiles dts
>   where
>     files = getFileNames dts

> filesReady :: [String] -> IO (Bool)
> filesReady files = do
>   allFiles <- getDirectoryContents "."
>   fileStati <- mapM (fileExistsAndReady allFiles) files
>   return $ all (==True) fileStati 

> fileExistsAndReady allFiles file = case elem file allFiles of
>                                      False -> return False
>                                      True  -> fileReady file

> fileReady file = do
>   contents <- readFile file
>   print ("fileReady: ", fileExpLength, (length $ lines contents))
>   return $ fileExpLength <= (length $ lines contents)

------------ Combine the results from the separate parrallel processess -------

> reduceResult :: [String] -> IO ()
> reduceResult files = do
>   separateStrs <- mapM parseFile files
>   let reducedStrs = reduceResult' separateStrs
>   putStringencies reducedStrs 
>   print "All Done!"

> parseFile :: String -> IO ([(Int, Int, Int, Int, Float)])
> parseFile file = do
>     fileContents <- readFile file 
>     return $ map read $ lines fileContents

TBF: is there a better way to do this: now we can only use 2 or 3 cores
, which is actually pretty realistic, but still ...

Combination of results is simple: take the average of the stringencies

> reduceResult' :: [[(Int, Int, Int, Int, Float)]] -> [(Int, Int, Int, Int, Float)]
> reduceResult' strs = case (length strs) of 
>                          2 -> zipWith  averageStrs  (strs!!0) (strs!!1)
>                          3 -> zipWith3 averageStrs3 (strs!!0) (strs!!1) (strs!!2)
>                          --otherwise -> zipWith averageStrs (strs!!0) (strs!!1)

> averageStrs :: (Int, Int, Int, Int, Float) -> (Int, Int, Int, Int, Float) -> (Int, Int, Int, Int, Float)
> averageStrs (r1,f1,e1,o1, s1) (r2,f2,e2,o2,s2) = (r1,f1,e1,o1, (s1 + s2) / 2)

> averageStrs3 :: (Int, Int, Int, Int, Float) -> (Int, Int, Int, Int, Float) -> (Int, Int, Int, Int, Float) -> (Int, Int, Int, Int, Float)
> averageStrs3 (r1,f1,e1,o1, s1) (r2,f2,e2,o2,s2) (r3,f3,e3,o3,s3) = (r1,f1,e1,o1, (s1 + s2 + s3) / 3)

------- DB Stuff ----------------

> --connectDB = connectPostgreSQL $ "dbname=" ++ weatherDB ++ " user=dss"

> putStringencies :: [(Int, Int, Int, Int, Float)] -> IO ()
> putStringencies strs = do
>   cnn <- handleSqlError $ connectDB
>   mapM (putStringency'' cnn) strs
>   print "Wrote to DB"
>     where
>       putStringency'' cnn (r,f,e,o,s) = putStringency' cnn r f e o s

> putStringency' cnn rcvrId freq elev obstypeId str = do
>     run cnn query (xs rcvrId obstypeId str)
>     commit cnn
>     return ()
>   where
>     -- NOTE: use the INSERT sql if the table was truncated.
>     query = "INSERT INTO stringency (total, frequency, receiver_id, elevation, observing_type_id) VALUES (?, ?, ?, ?, ?)"
>     --query = "UPDATE stringency SET total = ? where frequency = ? AND receiver_id = ? AND elevation = ? AND observing_type_id = ?"
>     xs rcvrId obsTypeId str = [toSql str, toSql freq, toSql rcvrId, toSql elev, toSql obsTypeId] 
