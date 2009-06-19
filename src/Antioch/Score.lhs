> module Antioch.Score where

> import Antioch.DateTime
> import Antioch.Generators
> import Antioch.Types
> import Antioch.TimeAccounting
> import Antioch.Utilities
> import Antioch.Weather
> import Control.Monad.RWS.Strict
> import Data.Array
> import Data.Array.IArray  (amap)
> import Data.Array.ST
> import Data.Foldable      (foldr')
> import Data.List
> import Data.Maybe         (fromMaybe)
> import Test.QuickCheck hiding (frequency)
> import System.IO.Unsafe (unsafePerformIO)
> import System.Random

Ranking System from Memo 5.2, Section 3

3.1 Observing Efficiency

> efficiency, efficiencyHA :: DateTime -> Session -> Scoring (Maybe Float)
> efficiency   dt = fmap (fmap fst) . calcEfficiency dt
> efficiencyHA dt = fmap (fmap snd) . calcEfficiency dt

> calcEfficiency      :: DateTime -> Session -> Scoring (Maybe (Float, Float))
> calcEfficiency dt s = do
>     let trx = receiverTemperature dt s
>     tk  <- kineticTemperature dt s
>     w   <- weather
>     zod <- zenithOpticalDepth dt s
>     minTsysPrime' <- liftIO $ minTSysPrime w (frequency s) elevation
>     return $ do
>         tk' <- tk
>         zod' <- zod
>         minTsysPrime'' <- minTsysPrime'
>         let [eff, effTransit] = map (calcEff trx tk' minTsysPrime'' zod') [za, zat]
>         return (eff, eff / effTransit)
>   where
>     za  = zenithAngle dt s
>     zat = zenithAngleAtTransit s
>     -- gaurd against Weather server returning nothing for el's < 5.0.
>     elevation = max (deg2rad 5.0) (pi/2 - za)
>            
>     calcEff trx tk minTsysPrime' zod za = (minTsysPrime' / tsys') ^2
>       where
>         -- Round off to the nearest degree to align with hist. min. opacities
>         rndZa = deg2rad . realToFrac . round . rad2deg $ za
>         -- Equation 4 & 6
>         opticalDepth = zod / (cos . min 1.5 $ rndZa)
>
>         -- Equation 7
>         tsys  = trx + 5.7 + tk * (1 - exp (-opticalDepth))
>
>         tsys' = exp opticalDepth * tsys

> receiverTemperature      :: DateTime -> Session -> Float
> receiverTemperature dt s =
>     case dropWhile (\(x, _) -> x <= freq) freqBand of
>         (x : _) -> snd x
>         []      -> 60.0
>   where 
>         -- in real data, freqs can be < 2.0; the DB data is gaurded against
>         -- this, but not this function.
>         getFrequency s = if frequency s < 2.0 then 2.0 else frequency s
>         freq = fromIntegral . round . getFrequency $ s
>         freqBand =  [ (1.73,  6.0)
>                     , (3.95, 10.0)
>                     , (5.85,  5.0)
>                     , (10.0, 13.0)
>                     , (15.4, 14.0)
>                     , (26.5, 21.0)
>                     , (40.0, 35.0)
>                     , (50.0, 60.0)
>                      ]


> kineticTemperature      :: DateTime -> Session -> Scoring (Maybe Float)
> kineticTemperature dt s = do
>     w <- weather
>     liftIO $ tsys w dt (frequency s)

> zenithOpticalDepth      :: DateTime -> Session -> Scoring (Maybe Float)
> zenithOpticalDepth dt s = do
>     w <- weather
>     liftIO $ opacity w dt (frequency s)

> zenithAngle            :: DateTime -> Session -> Radians
> zenithAngle dt s = zenithAngleHA s $ lst - ra s
>   where
>     lst = hrs2rad . utc2lstHours $ dt

> zenithAngleAtTransit   :: Session -> Radians
> zenithAngleAtTransit s = zenithAngleHA s 0.0

> zenithAngleHA      :: Session -> Radians -> Radians
> zenithAngleHA s ha =
>     -- Equation 5
>     acos $ sin gbtLat' * sin dec' + cos gbtLat' * cos dec' * cos ha
>   where
>     dec' = dec s
>     gbtLat' = realToFrac gbtLat

TBF: this was moved from Statistic to here, but it needs a better home.

> elevationFromZenith :: Period -> Float
> elevationFromZenith p =
>     90 - rad2deg (zenithAngle dt (session p))
>   where 
>     dt = periodHalfTime p

> observingEfficiency        :: ScoreFunc
> atmosphericOpacity         :: ScoreFunc
> surfaceObservingEfficiency :: ScoreFunc
> trackingEfficiency         :: ScoreFunc

> observingEfficiency = score [atmosphericOpacity, surfaceObservingEfficiency, trackingEfficiency]

TBF:  atmosphericOpacity is a bad name, perhaps atmosphericEfficiency

> atmosphericOpacity      dt s = efficiency dt s >>= \eff -> atmosphericOpacity' eff dt s
> atmosphericOpacity' eff dt s = factor "atmosphericOpacity" eff

> surfaceObservingEfficiency dt s = factor "surfaceObservingEfficiency" . Just $
>     if isDayTime dt
>     then
>         -- Equation 9
>         exp (-(k * frequency s ^ 2 * epsilonFactor))
>         -- Equation 10
>         -- exp (-((fromIntegral . round . frequency $ s)/69.2) ^ 2)
>     else
>         1.0
>   where
>     c = 299792485.0
>     epsilonDay   = 0.46
>     epsilonNight = 0.39
>     epsilonFactor = epsilonDay ^ 2 - epsilonNight ^ 2
>     k = 32.0 * pi ^ 2 * 1e12 / (c ^ 2)

> theta :: Float -> Float
> theta f = 740.0 / f

> rmsTE :: DateTime -> Float
> rmsTE dt = if isDayTime dt then sigmaDay else sigmaNight
>   where
>     sigmaDay = 3.3
>     sigmaNight = 2.8

> trackingEfficiency dt s = do
>   w     <- weather
>   meas  <- measuredWind 
>   wind' <- if meas
>            then liftIO $ w2_wind w dt
>            else liftIO $ wind w dt 
>   factor "trackingEfficiency" $ calculateTE wind' dt s

> calculateTE :: Maybe Float -> DateTime -> Session -> Maybe Float
> calculateTE wind dt s = do
>     wind' <- wind
>     let rmsTE' = rmsTrackingError dt wind'
>     -- Equation 12
>     return $ (1.0 + 4.0 * log 2.0 * (rmsTE' / theta') ^ 2) ^^ (-2)
>   where
>     theta' = theta . frequency $ s

> minimumObservingConditions  :: DateTime -> Session -> Scoring (Maybe Bool)
> minimumObservingConditions dt s = do
>    w  <- weather
>    w' <- liftIO $ newWeather w (Just dt)
>    local (\env -> env { envWeather = w', envMeasuredWind = True}) $ do
>      let minObs = minObservingEff (frequency s) 
>      fs <- observingEfficiency dt s
>      let obsEff' = eval fs
>      [(_, Just trkErrLimit)] <- trackingErrorLimit dt s
>      -- new MOC
>      let minObs' = exp(-0.05 + 1.5*log(minObs))
>      let obsEffOK = obsEff' >= minObs'
>      -- old MOC
>      --let obsEffOK = obsEff' >= minObs - 0.1
>      let trkErrOK = trkErrLimit >= 1
>      return $ Just (obsEffOK && trkErrOK)

3.2 Stringency

> stringency                 :: ScoreFunc
> stringency _ s = do
>     w <- weather
>     stringency' <- liftIO $ totalStringency w (frequency s) elevation
>     factor "stringency" stringency'
>   where
>     elevation = pi/2 - zenithAngleAtTransit s

3.3 Pressure Feedback

Generate a scoring function having the pressure factors.

> genFrequencyPressure          :: [Session] -> Scoring ScoreFunc
> genFrequencyPressure sessions = genFrequencyPressure' factors
>   where
>     bins    = initBins (minBound, maxBound) band sessions
>     factors = binsToFactors bins

> genFrequencyPressure' factors = do
>     tell [FreqPressureHistory factors]
>     return $ frequencyPressure factors band

> genRightAscensionPressure          :: [Session] -> Scoring ScoreFunc
> genRightAscensionPressure sessions = genRightAscensionPressure' factors
>   where
>     accessor s = (round . rad2hr . ra $ s) `mod` 24
>     bins    = initBins (0, 23) accessor sessions
>     factors = binsToFactors bins

> genRightAscensionPressure' factors = do
>     tell [RaPressureHistory factors]
>     return $ rightAscensionPressure factors accessor
>   where
>     accessor s = (round . rad2hr . ra $ s) `mod` 24

Select the appropriate pressure factor from the array of pressures.

> frequencyPressure :: Ix a => Array a Float -> (Session -> a) -> ScoreFunc
> frequencyPressure fs f _ a =
>     factor "frequencyPressure" . Just $ sqrt (fs ! f a)

> rightAscensionPressure     :: Ix a => Array a Float -> (Session -> a) -> ScoreFunc
> rightAscensionPressure fs f _ a =
>     factor "rightAscensionPressure" . Just $ (fs ! f a) ** 0.3

Creates an array indexed by band or hour angle with the hours total and used
for each slice for computing pressures.

> initBins             :: Ix a => (a, a) -> (Session -> a) -> [Session] -> Array a (Int, Int)
> initBins bounds f xs = runSTArray $ initBins' bounds f . filter (\s-> (grade s) >= GradeA) $ xs

> initBins' bounds f xs = do
>     arr <- newArray bounds (0, 0)
>     for xs $ \x -> do
>         let bin = f x
>         (t, c) <- readArray arr bin
>         writeArray arr bin $! (t + sAlloted x, c + sUsed x)
>     return arr
>   where
>     for xs f = foldr ((>>) . f) (return ()) xs

Translates the total/used times pairs into pressure factors.

> binsToFactors :: Ix a => Array a (Int, Int) -> Array a Float
> binsToFactors = amap toFactor
>   where
>     -- Equations 19 and 21
>     toFactor (n, d) = 1.0 + asFactor n - asFactor d
>     asFactor i      = if i > 0 then log (fromIntegral i / 60.0) else 0.0

3.4 Performance Limits

> minObservingEff :: Frequency -> Float
> minObservingEff freq  =
>     -- Equation 23
>     avgEff - 0.02 - 0.1*(1.0 - avgEff)
>   where
>     nu0 = 12.8
>     r = min 50 freq / nu0
>     -- Equation 22
>     avgEff = sum [x * cos (y*r) |
>                  (x, y) <- zip [0.74, 0.155, 0.12, -0.03, -0.01] [0..]]

> observingEfficiencyLimit  :: ScoreFunc
> hourAngleLimit            :: ScoreFunc
> zenithAngleLimit          :: ScoreFunc
> trackingErrorLimit        :: ScoreFunc
> atmosphericStabilityLimit :: ScoreFunc

> observingEfficiencyLimit dt s = do
>     obsEff <- observingEfficiency dt s
>     let obsEff' = eval obsEff
>     if obsEff' < minObsEff
>         -- Equation 24
>         then fac $ exp (-((obsEff' - minObsEff) ^ 2) / (2.0 * sigma ^ 2))
>         else fac 1.0
>   where
>     sigma = 0.02
>     minObsEff = minObservingEff . frequency $ s
>     fac = factor "observingEfficiencyLimit" . Just

> hourAngleLimit        dt s = efficiencyHA dt s >>= \effHA -> hourAngleLimit' effHA dt s
> hourAngleLimit' effHA dt s =
>     boolean "hourAngleLimit" . fmap (\effHA' -> effHA' >= criterion) $ effHA
>   where
>     criterion = sqrt . (* 0.5) . minObservingEff . frequency $ s

> zenithAngleLimit dt s =
>    boolean "zenithAngleLimit" . Just $ zenithAngle dt s < deg2rad 85.0

> trackingErrorLimit dt s = do
>     w     <- weather
>     meas  <- measuredWind
>     wind' <- if meas
>              then liftIO $ w2_wind w dt
>              else liftIO $ wind w dt
>     boolean "trackingErrorLimit" $ calculateTRELimit wind' dt s 
>     

> calculateTRELimit :: Maybe Float -> DateTime -> Session -> Maybe Bool
> calculateTRELimit wind dt s = do
>         wind' <- wind
>         -- Equation 26
>         let fv = rmsTrackingError dt wind' / (theta . frequency $ s)
>         return $ fv <= maxErr
>   where
>     maxErr = 0.2 

>  -- Equation 11
> rmsTrackingError dt w = sqrt (rmsTE dt ^ 2 + (abs w / 2.1) ^ 4)

> atmosphericStabilityLimit _ _ =
>                            factor "atmosphericStabilityLimit" . Just $ 1.0

3.5 Other factors

> projectCompletion, thesisProject, scienceGrade :: ScoreFunc

> projectCompletion _ s = let
>     weight = 1000.0
>     total = fromIntegral (pAlloted . project $ s)
>     left  = total - fromIntegral (pUsed  . project $ s)
>     percent = if total <= 0.0 then 0.0 else 100.0*(total - left)/total
>     in factor "projectCompletion" . Just $
>     if percent <= 0.0 then 1.0 else 1.0 + percent/weight

> thesisProject _ s = factor "thesisProject" . Just $
>     if thesis . project $ s then 1.05 else 1.0

> scienceGrade _ s = factor "scienceGrade" . Just $
>     case grade s of
>         GradeA -> 1.0
>         GradeB -> 0.9
>         GradeC -> 0.1

3.x Other Factors *not* listed in Memo 5.2

Checks that all receiver groups needed by the given session will be available
at the given time.  Sessions store their desired receivers in Conjugate 
Normal Form (CNF).  For example: receivers = [K OR L] AND [K OR S] is CNF for
saying, "This Session needs to be run w/ the Ka receiver, or, if that's not
available, the L *and* S receivers".

In CNF, each reciever group (AND'd []'s) must be evaluated as true for a 
given point in time for this score factor to evaluate as True.  For example,
if for a given point in time, K is not up, but L and S are, then this score
factor will be True.

> receiver                                  :: ScoreFunc
> receiver dt Session { receivers = rcvrs } = do
>     rs <- receiverSchedule
>     boolean "receiver" . Just $ receiver' dt rcvrs rs 

Interpret an empty ReceiverSchedule, not as an accident, but as meaning
that we don't really care about receivers - effectively, all receivers are
up, all the time.

> receiver' :: DateTime -> [ReceiverGroup] -> ReceiverSchedule -> Bool
> receiver' _   _    [] = True
> receiver' dt rcvrs rs = evalCNF scheduled rcvrs 
>   where
>     scheduled = getReceivers dt rs
>     evalCNF av rs = all (==True) $ map (\rg -> any (==True) $ map (\r -> elem r av) rg) rs

Returns list of receivers that will be up at the given time.

> getReceivers :: DateTime -> ReceiverSchedule -> [Receiver]
> getReceivers dt rsched =
>     case takeWhile (\(x, _) -> x <= dt) rsched of
>         [] -> []
>         xs -> snd $ last xs 

More Scoring Factors not covered in Memo 5.2

Is there an observer available for this time and session?

> observerAvailable :: ScoreFunc
> observerAvailable dt s = boolean "observerAvailable" . Just $ obsAvailable dt s

> obsAvailable :: DateTime -> Session -> Bool
> obsAvailable dt s = not $ all (==True) $ map (isBlackedOut dt) (obs s)
>   where 
>     obs s = observers . project $ s
>     isBlackedOut dt obs = any (==True) $ map (inDateRange dt) (blackouts obs)

TBF WTF TBD: delete this after 09B!!!
For scheduling 09B, we cannot use observerAvailable, because Carl's database
does not track observers in the same way.  Instead, we will be working
with project blackouts.  After we are done with 09B, deprecate this!!!

> projectAvailable :: ScoreFunc
> projectAvailable dt s = boolean "projectAvailable" . Just $ projectAvailable' dt s

If the datetime given falls within one of the session's project's blackout 
dates, then the project is not available for observing at this time.

> projectAvailable' :: DateTime -> Session -> Bool
> projectAvailable' dt s = not $ any (==True) $ map (inDateRange dt) (pBlackouts . project $ s)

The low rfi flag is used for avoiding RFI that is rampent during the daytime.

> needsLowRFI :: ScoreFunc
> needsLowRFI dt s = boolean "needsLowRFI" . Just $ needsLowRFI' dt s

> needsLowRFI' :: DateTime -> Session -> Bool
> needsLowRFI' dt s = if lowRFI s then (not . isHighRFITime $ dt) else True

Sessions can specify any number of LST ranges in which they do not want
to observe at.

> lstExcepted :: ScoreFunc
> lstExcepted dt s = boolean "lstExcepted" . Just $ lstExcepted' dt s

> lstExcepted' :: DateTime -> Session -> Bool
> lstExcepted' dt s = if ((length . lstExclude $ s) == 0) then True else checkLst dt $ lstExclude s

Does the given datetime fall within any of the given exclusion ranges?
Note that an exclusion range can wrap around: that is, if, in (a, b), b < a, 
this is a wrap around.  Example: [(16.0, 12.0)] - the exlusion range starts
at 16, goes up to 24, and wraps around up again to 12.

> checkLst :: DateTime -> [(Float, Float)] -> Bool
> checkLst dt ranges = not $ any (inRange lst) ranges
>   where 
>     lst = utc2lstHours dt 
>     inRange x range = if ((fst range) <= (snd range)) then ((fst range) <= x && x <= (snd range)) else ((snd range) >= x) || (x >= (fst range))

A session should not be scheduled too close to previosly scheduled periods,
as specified by the timeBetween session attribute.  
NOTE: this does not keep strategies like Pack from disobeying 'timeBetween',
this must be handled inside of Pack, but does keep Pack from scheduling to 
close to periods scheduled in previous calls to Pack.

> enoughTimeBetween :: ScoreFunc
> enoughTimeBetween dt s = boolean "enoughTimeBetween" . Just $ enoughTimeBetween' dt s

TBF: what to really do in case of overlaps?

> enoughTimeBetween' :: DateTime -> Session -> Bool
> enoughTimeBetween' dt s | (timeBetween s) == 0 = True
>                         | (length . periods $ s) == 0 = True
>                         | overlapsPeriod dt s = False -- TBF WTF?
>                         | otherwise = (timeBetweenRecentPeriod dt s) >= (timeBetween s)
>   where
>     overlapsPeriod dt s = any (inPeriod dt) (periods s)
>     inPeriod dt p = (dt >= (startTime p)) && (dt < (endTime p))

We must handle not just the expected case: we are querying a time after
the last scheduled period has ended.  We must deal with periods in the
future as well.

> timeBetweenRecentPeriod :: DateTime -> Session -> Minutes
> timeBetweenRecentPeriod dt s = minimum $ map (absoluteTimeDiff dt) $ times s
>   where
>     absoluteTimeDiff dt1 dt2 = abs $ diffMinutes dt1 dt2
>     times s = concatMap (\p -> [startTime p, endTime p]) $ periods s

Some receivers are up for a limited time only.  Sessions that need these
types of receivers and have an A grade will get a boost so that they
have a better chance of being scheduled while the receiver is available.

> receiverBoost :: ScoreFunc
> receiverBoost _ s = factor "receiverBoost" . Just $ if receiverBoost' s then 1.5 else 1.0

> receiverBoost' :: Session -> Bool
> receiverBoost' s =  
>   any (==True) $ map (\rg -> all (==True) $ map (\r -> elem r boostRcvrs) rg) rgs
>   where
>     rgs = receivers s
>     boostRcvrs = [Rcvr_1070] -- PF2; TBF: how to specify this dynamically?

Scoring utilities

> scoreLocal :: Weather -> ScoreFunc -> Session -> DateTime -> Scoring Score
> scoreLocal w' sf s dt = local (\env -> env { envWeather = w', envMeasuredWind = True}) $ do
>       fs <- sf dt s
>       return $ eval fs 

Compute the score for a given session at given time, but replacing weather

> scoreForTime  :: ScoreFunc -> DateTime -> Session -> Scoring Score 
> scoreForTime sf dt s = do
>     w  <- weather
>     w' <- liftIO $ newWeather w (Just dt)
>     scoreLocal w' sf s dt

Compute the average score for a given session over an interval:
   * modfiy the weather to start at the time given
   * reject sessions that have quarters of score zero
This is for use when determining best backups to run.

> 
> avgScoreForTime  :: ScoreFunc -> DateTime -> Minutes -> Session -> Scoring Score 
> avgScoreForTime sf dt dur s = do
>     w  <- weather
>     w' <- liftIO $ newWeather w (Just dt)
>     scores <- mapM (scoreLocal w' sf s) times 
>     case length scores of
>       0 -> return 0.0
>       otherwise -> return $ sumScores scores / (fromIntegral . length $ scores)
>   where
>     -- TBF:  Using the measured wind speed for scoring in the future is unrealistic, but damn convent!
>     numQtrs = dur `div` quarter
>     times = [(q*quarter) `addMinutes'` dt | q <- [0..(numQtrs-1)]]
>     sumScores scores = case dropWhile (>0.0) scores of
>         [] -> sum scores
>         otherwise -> 0.0 -- don't allow zero-scored quarters
> 

These methods for scoring a session are to be used in conjunction with
Schedule's 'best' function.

> type BestScore = ScoreFunc -> DateTime -> Session -> Scoring Score

> firstScore :: BestScore
> firstScore sf dt s = do
>     factors <- sf dt s
>     return $ eval factors

Compute the average score for a given session over an interval.

> averageScore :: BestScore
> averageScore sf dt s = do
>     score <- totalScore sf dt dur s
>     return $! score / fromIntegral (dur `div` quarter)
>   where
>     dur = minDuration s

Compute the total score for a given session over an interval.

> totalScore :: ScoreFunc -> DateTime -> Minutes -> Session -> Scoring Score
> totalScore sf dt dur s = do
>     scores <- mapM (liftM eval . flip sf s) times
>     return $! addScores scores
>   where
>     times  = map (`addMinutes'` dt) [0, quarter .. dur-1]

Add a set of scores, with the added complication that if any
individual score is zero then the end result must also be zero.

> addScores :: [Score] -> Score
> addScores = fromMaybe 0.0 . foldr' step (Just 0.0)
>   where
>     step s Nothing   = Nothing
>     step s (Just x)
>         | s < 1.0e-6 = Nothing
>         | otherwise  = Just $! x + s

> type Factor   = (String, Maybe Score)
> type Factors  = [Factor]

> type ReceiverSchedule = [(DateTime, [Receiver])]

This is the environment that the Scoring Monad is carrying around
to avoid long lists of repetitive parameters.

> data ScoringEnv = ScoringEnv {
>     envWeather      :: Weather
>   , envReceivers    :: ReceiverSchedule
>   , envMeasuredWind :: Bool
>   }

Just an easy way to pull the stuff like weather or the receiver schedule
out of ScoringEnv, e.g., the weather function returns the weather in
the Scoring Monad, as in the action "w <- weather".

> weather :: Scoring Weather
> weather = asks envWeather

> receiverSchedule :: Scoring ReceiverSchedule
> receiverSchedule = asks envReceivers

> measuredWind :: Scoring Bool
> measuredWind = asks envMeasuredWind

The Scoring monad encapsulates the concept of a scoring action,
all the scoring functions live in the monad so they can
execute scoring actions.

A Trace collects/logs information about the execution of a monad.

> data Trace = Timestamp DateTime
>            | FreqPressureHistory (Array Band Float)
>            | RaPressureHistory (Array Int Float)
>            | Cancellation Period
>            deriving (Eq, Show)

> type Scoring = RWST ScoringEnv [Trace] () IO

A scoring action returns its results inside the Scoring monad,
runScoring allows one to extract those results from the monad
resulting in simple types rather than monadic types.

> runScoring      :: Weather -> ReceiverSchedule -> Scoring t -> IO t
> runScoring w rs = liftM fst . runScoring' w rs

> runScoring'        :: Weather -> ReceiverSchedule -> Scoring t -> IO (t, [Trace])
> runScoring' w rs f = evalRWST f (ScoringEnv w rs False) ()

This allows us to run scoring multiple times, all within the same trace.  Mainly
useful for simulation.

> -- runScoring''        :: Weather -> ReceiverSchedule -> Scoring t -> IO t
> -- runScoring'' w rs f = runReaderT f $ ScoringEnv w rs

Because ScoreFunc returns lists of factors, this function allows
us to easily return a list.

> factor          :: String -> Maybe Score -> Scoring Factors
> factor name val = return [(name, val)]

Sub-class of scoring actions that return a list of factors
as listed in Memo 5.2.

> type ScoreFunc = DateTime -> Session -> Scoring Factors 

> instance Show (a -> b) where
>     show _ = "ScoreFunc"

> concatMapM   :: (Functor m, Monad m) => (a -> m [b]) -> [a] -> m [b]
> concatMapM f = fmap concat . mapM f

Composite pattern on subpartitions ofscoring functions, e.g., political factors.

> score         :: [ScoreFunc] -> ScoreFunc
> score fs dt s = concatMapM (\f -> f dt s) fs

Provides a means of scoring a session on subsets of the factors.

> ignore       :: [String] -> Factors -> Factors
> ignore names = filter $ \(n, _) -> not (n `elem` names)
  
Need to translate a session's factors into the final product score.

> eval :: Factors -> Score
> eval = foldr' step 1.0
>   where
>     step (_, Nothing) s = 0.0
>     step (_, Just f)  s
>         | s < 1.0e-6    = 0.0
>         | otherwise     = s * f

> genScore          :: [Session] -> Scoring ScoreFunc
> genScore sessions = do
>     raPressure   <- genRightAscensionPressure sessions
>     freqPressure <- genFrequencyPressure sessions
>     genScore' raPressure freqPressure

> genScore' raPressure freqPressure = return $ \dt s -> do
>     effs <- calcEfficiency dt s
>     score [
>         scienceGrade
>       , thesisProject
>       , projectCompletion
>       , stringency
>       , (atmosphericOpacity' . fmap fst) effs
>       , surfaceObservingEfficiency
>       , trackingEfficiency
>       , raPressure
>       , freqPressure
>       , observingEfficiencyLimit
>       , (hourAngleLimit' . fmap snd) effs
>       , zenithAngleLimit
>       , trackingErrorLimit
>       , atmosphericStabilityLimit
>       , receiver
>       , projectAvailable -- TBF: only for 09B, then use observerAvailable!!!
>       , needsLowRFI
>       , lstExcepted
>       , enoughTimeBetween
>       ] dt s

Convenience function for translating go/no-go into a factor.

> boolean :: String -> Maybe Bool -> Scoring Factors
> boolean name = factor name . fmap (\b -> if b then 1.0 else 0.0)

Convenience function for scoring a Session over it's Period's duration
Note: The score recorded in the period (pScore) is the average over it's 
duration.  So, we should be able to reproduce that using this function, 
the original pool of sessions (for the correct pressures), and the forecast 
used to generate pScore (using the time pScore was calculated for, pForecast).

> scorePeriod :: Period -> ScoreFunc -> Scoring [Factors]
> scorePeriod p sf = mapM (scorePeriod' sf) dts
>   where
>     scorePeriod' sf dt = sf dt (session p)
>     dts = [(i*quarter) `addMinutes'` (startTime p) | i <- [0..((duration p) `div` quarter)]]

Quick Check properties:

> prop_efficiency = forAll genProject $ \p ->
>   let es = map calcEff (sessions p) in normalized es  
>   where
>     calcEff s = unsafePerformIO $ do
>       w <- theWeather
>       w' <- newWeather w (Just $ fromGregorian 2006 10 14 9 15 2)
>       let dt = fromGregorian 2006 10 15 12 0 0
>       Just result <- runScoring w' [] (efficiency dt s)
>       return result

> prop_surfaceObservingEfficiency = forAll genProject $ \p ->
>   let es = map calcEff (sessions p) in normalized es  
>   where
>     calcEff = getScoringResult surfaceObservingEfficiency

> prop_trackingEfficiency = forAll genProject $ \p ->
>   let es = map calcEff (sessions p) in normalized es  
>   where
>     calcEff = getScoringResult trackingEfficiency

> prop_stringency = forAll genProject $ \p ->
>   let es = map getStringency (sessions p) in greaterThenOne es  
>   where
>     getStringency = getScoringResult stringency

> prop_observingEfficiencyLimit = forAll genProject $ \p ->
>   let es = map getObsEffLimit (sessions p) in normalized es  
>   where
>     getObsEffLimit = getScoringResult observingEfficiencyLimit

> prop_hourAngleLimit = forAll genProject $ \p -> checkBoolScore p hourAngleLimit

> prop_zenithAngleLimit = forAll genProject $ \p -> checkBoolScore p zenithAngleLimit

> prop_trackingErrorLimit = forAll genProject $ \p -> checkBoolScore p trackingErrorLimit

> prop_atmosphericStabilityLimit = forAll genProject $ \p -> checkBoolScore p atmosphericStabilityLimit

> prop_frequencyPressure = forAll genProject $ \p ->
>   let es = map (getScoringResult fp) (sessions p) in greaterOrEqToOne es
>     where
>       fp = getPressureFunction genFrequencyPressure
>       greaterOrEqToOne xs = dropWhile (>=1) xs == []

> prop_rightAscensionPressure = forAll genProject $ \p ->
>   let es = map (getScoringResult fp) (sessions p) in greaterOrEqToOne es
>     where
>       fp = getPressureFunction genRightAscensionPressure
>       greaterOrEqToOne xs = dropWhile (>=1) xs == []

Utilities for QuickCheck properties:

> getPressureFunction f = unsafePerformIO $ do
>     g <- getStdGen
>     let sessions = generate 0 g $ genSessions 100
>     runScoring undefined [] $ f sessions

> checkBoolScore p sf = let es = map (getScoringResult sf) (sessions p) in areBools es

> getScoringResult sf s = unsafePerformIO $ do
>     w <- theWeather
>     w' <- newWeather w (Just $ fromGregorian 2006 4 15 0 0 0) 
>     let dt = fromGregorian 2006 4 15 16 0 0
>     [(_, Just result)] <- runScoring w' [] (sf dt s)
>     return result

Used for checking that some scoring factors are 0 <= && <= 1, etc.

> normalized :: [Float] -> Bool
> normalized xs = dropWhile normal xs == []
>   where
>     normal x = 0.0 <= x && x <= 1.0

> areBools :: [Float] -> Bool
> areBools xs = dropWhile isBool xs == []
>   where
>     isBool x = 0.0 == x || x == 1.0

> greaterThenOne :: [Float] -> Bool 
> greaterThenOne xs = dropWhile (>1) xs == []
