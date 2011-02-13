> {-# OPTIONS -XFlexibleContexts #-}

> module Antioch.Score where

> import Antioch.DateTime
> import Antioch.Generators
> import Antioch.Types
> import Antioch.TimeAccounting
> import Antioch.Utilities
> import Antioch.Weather
> import Antioch.ReceiverTemperatures
> import Antioch.Receiver
> import Control.Monad.RWS.Strict
> import Control.Monad      (liftM2)
> import Data.Array
> import Data.Array.IArray  (amap)
> import Data.Array.ST
> import Data.Foldable      (foldr')
> import Data.List
> import Data.Maybe         (fromMaybe, isJust, isNothing, fromJust, catMaybes)
> import Test.QuickCheck hiding (frequency)
> import System.IO.Unsafe (unsafePerformIO)
> import System.Random

Ranking System from Memo 5.2, Section 3

Lowest Frequency having Forecasts

> lff :: Frequency
> lff = 2.0

3.1 Observing Efficiency

> efficiency, efficiencyHA :: DateTime -> Session -> Scoring (Maybe Float)
> efficiency   dt = fmap (fmap fst) . calcEfficiency dt
> efficiencyHA dt = fmap (fmap snd) . calcEfficiency dt

Equation 3

> calcEfficiency :: DateTime -> Session -> Scoring (Maybe (Float, Float))
> calcEfficiency dt s = do
>     rt <- receiverTemperatures
>     trx <- liftIO $ getRcvrTemperature rt s
>     tk  <- kineticTemperature dt s 
>     w   <- weather
>     zod <- zenithOpacity dt s 
>     let rcvr = getPrimaryReceiver s
>     minTsysPrime' <- liftIO $ maybe (return Nothing) (minTSysPrime w (frequency s) elevation') rcvr
>     return $ do
>         tk' <- tk
>         zod' <- zod
>         trx' <- trx
>         minTsysPrime'' <- minTsysPrime' >>= Just . (*xf)
>         let [tsys, tsysTransit] = map (tSysPrime' trx' tk' zod') [za, zat] 
>         let [eff, effTransit] = map (\t -> (minTsysPrime'' / t)^2) [tsys, tsysTransit]
>         return (eff, eff / effTransit)
>   where
>     za  = zenithAngle dt s
>     zat = zenithAngleAtTransit s
>     elevation' = (pi/2 - za)
>     xf = xi s
>            

For given input, gather the necessary intermediate values needed from the
resources: weather and receiver temperatures. Then pass them on to the 
function that actually computes tsys':

> tSysPrime :: Weather -> ReceiverTemperatures -> Receiver -> Float -> Float -> DateTime -> Scoring (Maybe Float)
> tSysPrime w rt rcvr freq elev dt = do
>   trx' <- liftIO $ getReceiverTemperature rt (Just rcvr) freq
>   tk' <- liftIO $ tsys w dt freq
>   zod' <- zenithOpacity' dt freq
>   let za = pi/2 - elev 
>   return $ do 
>       tk <- tk'
>       zod <- zod'
>       trx <- trx'
>       return $ tSysPrime' trx tk zod za

This is for use both in scoring a session and when calculating
the historical weather (i.e. stringency and min. eff. system temp.)

Numerator or denominator of Equation 3

> tSysPrime' :: Float -> Float -> Float -> Float -> Float
> tSysPrime' trx tk zod za = exp atmosphericOpacity' * tsys
>   where
>     atmosphericOpacity' = atmosphericOpacity zod za
>     tsys  = systemNoiseTemperature' trx tk atmosphericOpacity'

> minTsys' :: Weather -> DateTime -> Session -> IO (Maybe Float)
> minTsys' w dt s = do
>     mts' <- liftIO $ maybe (return Nothing) (minTSysPrime w (frequency s) (elevation dt s)) rcvr 
>     return $ do
>         mts' >>= Just . (*xf)
>     where
>       xf = xi s
>       rcvr = getPrimaryReceiver s

Equation 4

> atmosphericOpacity :: Float -> Radians -> Float
> atmosphericOpacity zod za = zod / (cos . min 1.5 $ rndZa)
>   where
>     -- Round off to the nearest degree to align with hist. min. opacities
>     rndZa = deg2rad . realToFrac . round . rad2deg $ za

Equation 7

> systemNoiseTemperature' :: Float -> Float -> Float -> Float
> systemNoiseTemperature' trx tk atmosphericOpacity' =  trx + 5.7  + tk * (1 - exp (-atmosphericOpacity'))

> systemNoiseTemperature :: Weather -> ReceiverTemperatures -> DateTime -> Session -> IO (Maybe Float)
> systemNoiseTemperature w rt dt s = runScoring w [] rt $ do
>     zod <- zenithOpacity dt s
>     tk  <- kineticTemperature dt s
>     trx <- liftIO $ getRcvrTemperature rt s
>     let za  = zenithAngle dt s
>     return $ if (isJust trx) then liftM2 (\x y ->
>         let atmosphericOpacity' = atmosphericOpacity y za in
>         systemNoiseTemperature' (fromJust trx) x atmosphericOpacity') tk zod else Nothing

TBF what problem is this function solving, and why rndZa???

> systemNoiseTemperaturePrime :: Weather -> ReceiverTemperatures -> DateTime -> Session -> IO (Maybe Float)
> systemNoiseTemperaturePrime w rt dt s = runScoring w [] rt $ do
>     zod <- zenithOpacity dt s
>     tk  <- kineticTemperature dt s
>     trx <- liftIO $ getRcvrTemperature rt s
>     let za  = zenithAngle dt s
>     let rndZa = deg2rad . realToFrac . round . rad2deg $ za
>     return $ if (isJust trx) then liftM2 (\x y ->
>         let atmosphericOpacity' = atmosphericOpacity y za in
>         (exp atmosphericOpacity') * (systemNoiseTemperature' (fromJust trx) x atmosphericOpacity')) tk zod else Nothing

> kineticTemperature      :: DateTime -> Session -> Scoring (Maybe Float)
> kineticTemperature dt s = do
>     w <- weather
>     liftIO $ tsys w dt (frequency s)

Equation 3a

> zenithOpacityDryAir :: Maybe Float -> Frequency -> Maybe Float
> zenithOpacityDryAir zod f = do
>   zod' <- zod
>   return $ k + (zod' - k) * (f/lff)^^2
>   where
>     k = 0.0075

> zenithOpacity :: DateTime -> Session -> Scoring (Maybe Float)
> zenithOpacity dt s = zenithOpacity' dt (frequency s)

> zenithOpacity' :: DateTime -> Frequency -> Scoring (Maybe Float)
> zenithOpacity' dt f = do
>     w <- weather
>     zod <- liftIO $ opacity w dt lff
>     if f < lff then return $ zenithOpacityDryAir zod f
>                else liftIO $ opacity w dt f

> hourAngle :: DateTime -> Session -> Radians
> hourAngle dt s = lst - ra'
>   where
>     lst = hrs2rad . utc2lstHours $ dt
>     ra' = ra s

> elevation :: DateTime -> Session -> Radians
> elevation dt s = pi/2 - zenithAngle dt s

> zenithAngle            :: DateTime -> Session -> Radians
> zenithAngle dt s = zenithAngleHA s . hourAngle dt $ s

> zenithAngleAtTransit   :: Session -> Radians
> zenithAngleAtTransit s = zenithAngleHA s 0.0

> zenithAngleHA      :: Session -> Radians -> Radians
> zenithAngleHA s ha = zenithAngle' dec' ha
>   where
>     dec' = dec s

Equation 5

> zenithAngle' :: Radians -> Radians -> Radians
> zenithAngle' dec ha = acos $ sin gbtLat' * sin dec + cos gbtLat' * cos dec * cos ha
>   where
>     gbtLat' = realToFrac gbtLat

TBF: this was moved from Statistic to here, but it needs a better home.

> elevationFromZenith :: Period -> Float
> elevationFromZenith p =
>     90.0 - rad2deg (zenithAngle dt (session p))
>   where 
>     dt = periodHalfTime p

> observingEfficiency        :: ScoreFunc
> atmosphericEfficiency         :: ScoreFunc
> surfaceObservingEfficiency :: ScoreFunc
> trackingEfficiency         :: ScoreFunc

> observingEfficiency = score [atmosphericEfficiency, surfaceObservingEfficiency, trackingEfficiency]

> atmosphericEfficiency      dt s = efficiency dt s >>= \eff -> atmosphericEfficiency' eff dt s
> atmosphericEfficiency' eff dt s = do
>     let eff' = maybe Nothing (Just . min 1.0) eff
>     factor "atmosphericEfficiency" eff'

Equation 9

> surfaceObservingEfficiency' :: DateTime -> Frequency -> Score
> surfaceObservingEfficiency' dt f = 
>     if isPTCSDayTime roundToHalfPast dt
>     then
>         exp (-(k * f^2 * epsilonFactor))
>     else
>         1.0
>   where
>     c = 299792485.0
>     -- As of 2009-12-16: day = 400, night = 340 (microns)
>     epsilonDay   = 0.30
>     epsilonNight = 0.25
>     epsilonFactor = epsilonDay ^ 2 - epsilonNight ^ 2
>     k = 32.0 * pi^2 * 1e12 / (c ^ 2)

> surfaceObservingEfficiency dt s = factor "surfaceObservingEfficiency" . Just $ surfaceObservingEfficiency' dt (frequency s)

Equation 14

> halfPwrBeamWidth :: Frequency -> Frequency
> halfPwrBeamWidth f = 740.0 / f

> rmsTE :: DateTime -> Float
> rmsTE dt = if isPTCSDayTime roundToHalfPast dt then trErrSigmaDay else trErrSigmaNight

> trErrSigmaNight, trErrSigmaDay :: Float
> trErrSigmaNight = 2.8
> trErrSigmaDay   = 3.3

> trackingEfficiency dt s = do
>   wind <- getRealOrForecastedWind dt
>   factor "trackingEfficiency" $ trackingObservingEfficiency wind dt (usesMustang s) (frequency s)

> trackingObservingEfficiency :: Maybe Float -> DateTime -> Bool -> Frequency -> Maybe Float
> trackingObservingEfficiency wind dt mustang freq = do
>     wind' <- wind
>                                                          -- Equation:
>     let f = trackErr dt wind' freq                       -- from 13
>     let fmin = trErrSigmaNight / (halfPwrBeamWidth freq) -- 13a
>     let fv = trackErrArray wind' freq                    -- from 16
>     let fvmin = epsilonZero / (halfPwrBeamWidth freq)    -- 17b
>     if mustang then return $ renormalize fvmin fv        -- 17a
>                else return $ renormalize fmin f          -- 12a
>   where
>     renormalize fn fd = ((calculateTE fn) / (calculateTE fd))^2

Base of exponential Equation 12

> calculateTE :: Frequency -> Float
> calculateTE f = 1.0 + 4.0 * log lff * f ^ 2


Minium Observing Conditions (MOC).  Note that what weather data is used is
up to the client calling this function.  For instance, in simulations we
set the origin of the weather to one hour before the start of the period, 
ensure that subsequent Scoring calls will use this weather, *then* call 
this function.

TBF: equation ? in Memo 5.?

The final boolean value is a comparison of the average of the non-overhead
quarter factors compared to the adjusted Min. Obs. Efficiency.  Those
factors are observing efficiency (factors) and tracking error limit.

> minimumObservingConditions  :: DateTime -> Minutes -> Session -> Scoring (Maybe Bool)
> minimumObservingConditions dt dur s | numQtrs <= getOverhead s = return Nothing
>                                     | otherwise = do
>     let minObs = adjustedMinObservingEff $ minObservingEff . frequency $ s
>     fcts <- mapM (minObsFactors s) $ drop (getOverhead s) dts
>     let effProducts = map (\(fs, tr) -> ((eval fs) * tr)) fcts
>     let meanEff = (sum effProducts) / (fromIntegral . length $ effProducts) 
>     return $ Just (meanEff >= minObs)
>   where
>     dts = [(15 * m) `addMinutes` dt | m <- [0 .. (dur `div` 15) - 1]]
>     numQtrs = dur `div` 15

Calculate the factors that make up the moc at the given time (quarter).
Note that we return part of the tuple as factors instead of evaluating here
to ease the debugging process.

> minObsFactors :: Session -> DateTime -> Scoring (Factors, Float)
> minObsFactors s dt = do
>      fss <- observingEfficiency dt s
>      trkErrLimit <- trackingErrorLimit dt s 
>      return (fss, eval trkErrLimit)

> adjustedMinObservingEff :: Float -> Float
> adjustedMinObservingEff minObs = exp(-0.05 + 1.5*log(minObs))

Periods from Elective Sessions should not run if they don't pass
MOC, unless they are gauranteed, and this is the last period in 
the elective group.

> goodElective :: Period -> Scoring (Bool)
> goodElective p | isNotElective p = return True
>                | isScheduledElective p = return True
>                | isGuaranteedElective p = return True
>                | otherwise = do
>   -- check for gauranteed?
>   moc <- minimumObservingConditions dt dur s
>   case moc of
>     Nothing -> return False
>     Just moc'  -> return moc'
>   where
>     isNotElective p = (sType . session $ p) /= Elective
>     isElective p = (sType . session $ p) == Elective
>     isScheduledElective p = (isElective p) && (pState p == Scheduled)
>     isGuaranteedElective p = (isElective p) && (guaranteed . session $ p) && (isLastPeriodOfElective p) 
>     dt = startTime p
>     dur = duration p
>     s = session p


The last periods in a group of periods (Electives) needs special 
consideration: if it's session is NOT gauranteed time, then there's
a chance even the last periods won't observe.

> isLastPeriodOfElective :: Period -> Bool
> isLastPeriodOfElective p = isLastPeriod p elec
>   where 
>     pid = peId p
>     elecs = electives . session $ p
>     periodInElective e = any (==pid) (ePeriodIds e) 
>     elecs' = filter periodInElective elecs  
>     elec = if (length elecs') == 1 then Just . head $ elecs' else Nothing

> isLastPeriod :: Period -> Maybe Electives -> Bool
> isLastPeriod p me | isNothing me = False
>                   | otherwise    = (peId p) == (last . ePeriodIds . fromJust $ me)

Default Periods of Windows from non-guaranteed Sessions should not
run if they don't pass MOC.  So we must enforce this matrix:

|             |  *guaranteed*                    | *non-guaranteed* |
| has default |	The default period is scheduled. | The default period is scheduled if it meets minimum observing conditions |
| no default | NA                                | As previosuly in the window, the session must compete for a time slot. |

> goodDefaultPeriod :: Period -> Scoring (Bool)
> goodDefaultPeriod p | isNotWindowed p = return True
>                     | isScheduledWindow p = return True
>                     | isGuaranteedWindow p = return True
>                     | otherwise = do
>   moc <- minimumObservingConditions dt dur s
>   case moc of
>     Nothing -> return False
>     Just moc'  -> return moc'
>   where
>     isNotWindowed p = (sType . session $ p) /= Windowed
>     isWindowed p = (sType . session $ p) == Windowed
>     isScheduledWindow p = (isWindowed p) && (pState p == Scheduled)
>     isGuaranteedWindow p = (isWindowed p) && (guaranteed . session $ p) 
>     dt = startTime p
>     dur = duration p
>     s = session p

3.2 Stringency

> stringency                 :: ScoreFunc
> stringency _ s = do
>     w <- weather
>     jstr <- liftIO $ stringency' w s
>     let str' = do
>         str <- jstr
>         return $ str ** 1.0
>     factor "stringency" str' 


> stringency' :: Weather -> Session -> IO (Maybe Float)
> stringency' w s = do
>   case rcvr of
>     Nothing  -> return Nothing
>     (Just r) -> totalStringency w freq elevation' r obsType
>   where
>     freq = frequency s
>     elevation' = pi/2 - zenithAngleAtTransit s
>     rcvr = getPrimaryReceiver s
>     obsType = oType s       

3.3 Pressure Feedback

Generate a scoring function having the pressure factors.

> genFrequencyPressure :: DateTime -> [Session] -> Scoring ScoreFunc
> genFrequencyPressure dt sessions = do
>     -- liftIO $ print bins
>     -- liftIO $ print factors
>     genFrequencyPressure' factors bins
>   where
>     bins    = initBins dt (minBound, maxBound) band sessions
>     factors = binsToFactors bins

> genFrequencyPressure' :: (MonadWriter [Trace] m) => Array Band Float -> Array Band (Int, Int) -> m ScoreFunc
> genFrequencyPressure' factors bins = do
>     tell [FreqPressureHistory factors]
>     -- we also write the components of the pressures (n, d) for debugging.
>     tell [FreqPressureBinHistory bins]
>     return $ frequencyPressure factors band

> genRightAscensionPressure :: DateTime -> [Session] -> Scoring ScoreFunc
> genRightAscensionPressure dt sessions = genRightAscensionPressure' accessor factors
>   where
>     accessor s = (round . rad2hrs . ra $ s) `mod` 24
>     bins    = initBins dt (0, 23) accessor sessions
>     factors = binsToFactors bins

> genRightAscensionPressure' :: (MonadWriter [Trace] m) => (Session -> Int) -> Array Int Float -> m ScoreFunc
> genRightAscensionPressure' accessor factors = do
>     tell [RaPressureHistory factors]
>     return $ rightAscensionPressure factors accessor

Select the appropriate pressure factor from the array of pressures.

> frequencyPressure :: Ix a => Array a Float -> (Session -> a) -> ScoreFunc
> frequencyPressure fs f _ a =
>     factor "frequencyPressure" . Just $ sqrt (fs ! f a)

> rightAscensionPressure     :: Ix a => Array a Float -> (Session -> a) -> ScoreFunc
> rightAscensionPressure fs f _ a =
>     factor "rightAscensionPressure" . Just $ (fs ! f a) ** 0.3

Creates an array indexed by band or hour angle with the hours total and used
for each slice for computing pressures.

> initBins :: Ix a => DateTime -> (a, a) -> (Session -> a) -> [Session] -> Array a (Int, Int)
> initBins dt bounds f xs = runSTArray $ initBins' dt bounds f $ xs

For a specific RA or band we need:
  n = approved observing hours
  d = hours already done
  r = remaining hours
i.e., n = d + r, where pressure = 1 + ln( n / d).

All of this applies to a given semester and grade B or higher 
non-maintenance sessions.

The value d is easy, simply add up all the time billed for every period
completed for the current semester across ALL sessions.

The value n is more difficult because the "approved" hours -- depending
on the definition -- changes over the semester.  During the semester,
sessions go in and/or out of being completed, authorized, and enabled.
So which ones to add up for the total approved hours for a given
RA or band?

Our solution is to derive n indirectly by first computing r, which
will guarantee the relationship n = d + r.  For all authorized and not
completed sessions, the value r is the allotted time minus the sum
of all completed time billed, i.e., residue.
Now pressure = 1 + ln( (d + r) / d )

Note:
    - As sessions become completed, their hours still are used in the
      computation, but now via the factor d instead of being distributed
      between d and r.
    - Unauthorizing a session will reduce the allotted time, but the hours
      done continue to factor into pressure.
    - Sessions used for computing d are only limited by grade, and their
      periods are limited to the semester.
    - Sessions used for computing r are limited to authorized and
      not completed, and their periods are limited to the semester.
    - Note that used time is computed twice:
        * for d which uses *all* past periods from the current semester
        * for use in the residue which excludes unauthorized and completed
          sessions from the current or all-past semesters depending on
          whether time available is a function of total or semester
          allotment
    - Unlike the computation for determining if time-allotted has been
      fully consumed, time computations for pressure ignore project time
      because it is not specific to RA or band.

The result is that the computing of pressures become somewhat dynamic
without ignoring  successful observation time.

> initBins' dt bounds f xs = do
>     arr <- newArray bounds (0, 0)
>     for xs $ \x -> do
>         let bin = f x
>         (t, c) <- readArray arr bin
>         -- the following tuple is (n, d) in the equation 1 + log (n/d)
>         writeArray arr bin $! (t + rho x + sPastS dt x, c + sPastS dt x)
>     return arr
>   where
>     for xs f = foldr ((>>) . f) (return ()) xs
>     rho s
>       -- the max prevents against negative remainders, i.e.,
>       -- over-scheduled sessions
>       | isActive s = max 0 (residue dt s)
>       | otherwise  = 0
>     isActive s = (authorized s) && (not . sComplete' $ s)
>     sComplete' s = (sTerminated s) || ((residue dt s) < quarter)

> residue :: DateTime -> Session -> Minutes
> residue dt s = min allot_sem allot_tot
>     where
>       allot_sem = (sAllottedS s) - (sPastS dt s)
>       allot_tot = (sAllottedT s) - (sPastT dt s)

Translates the total/used times pairs into pressure factors.

> binsToFactors :: Ix a => Array a (Int, Int) -> Array a Float
> binsToFactors = amap toFactor
>   where
>     -- Equations 19 and 21
>     toFactor (n, d) = 1.0 + asFactor n - asFactor d
>     asFactor i      = if i > 0 then log (fromIntegral i / 60.0) else 0.0

3.4 Performance Limits

Equation 23

> minObservingEff :: Frequency -> Float
> minObservingEff freq  =
>     avgObservingEff' - 0.02 - 0.1*(1.0 - avgObservingEff')
>   where
>     avgObservingEff' = avgObservingEff freq

> avgObservingEff, avgObservingEffLo, avgObservingEffHi :: Frequency -> Float

> avgObservingEff f
>     -- not exactly right according to 5.3, but more pragmatic
>     | f <= 52.0  = avgObservingEffLo f
>     | otherwise  = avgObservingEffHi f

Equation 22

> avgObservingEffLo f = sum [x * cos (y*f/nu0) |
>                  (x, y) <- zip [0.74, 0.155, 0.12, -0.03, -0.01] [0..]]
>   where
>     nu0 = 12.8::Frequency

Equation 22a

> avgObservingEffHi f = sum [x * cos ((y*f - nu)/nu1) |
>                  (x, y) <- zip [0.5, 0.0, 0.0, 0.0, 0.0] [0..]]
>   where
>     nu = 92.0::Frequency
>     nu1 = 15.3::Frequency

> observingEfficiencyLimit  :: ScoreFunc
> hourAngleLimit            :: ScoreFunc
> zenithAngleLimit          :: ScoreFunc
> trackingErrorLimit        :: ScoreFunc
> atmosphericStabilityLimit :: ScoreFunc

> observingEfficiencyLimit dt s = do
>     obsEff <- observingEfficiency dt s
>     let obsEff' = eval obsEff
>     fac $ observingEfficiencyLimit' obsEff' minObsEff $ frequency s
>   where
>     minObsEff = minObservingEff . frequency $ s
>     fac = factor "observingEfficiencyLimit" . Just

Equation 24 

> observingEfficiencyLimit' :: Float -> Float -> Frequency -> Float
> observingEfficiencyLimit' obsEff minObsEff freq = if obsEff < minObsEff then exp (-((obsEff - minObsEff) ^ 2) / (2.0 * sigma ^ 2)) else 1.0
>   where
>     sigma = 0.02

> hourAngleLimit        dt s | isJust . elLimit $ s = elevationLimit dt s
>                            | otherwise = efficiencyHA dt s >>= \effHA -> hourAngleLimit' effHA dt s

> hourAngleLimit' effHA dt s | isJust . elLimit $ s = elevationLimit dt s
>                            | otherwise = boolean "hourAngleLimit" . fmap (\effHA' -> effHA' >= criterion) $ effHA
>   where
>     criterion = sqrt . (* 0.5) . minObservingEff . frequency $ s

> elevationLimit dt s = boolean "hourAngleLimit" . Just $ elevationLimit' dt s

> elevationLimit' :: DateTime -> Session -> Bool
> elevationLimit' dt s | isJust . elLimit $ s = el >= lim
>                      | otherwise            = True
>   where
>     lim = fromJust . elLimit $ s
>     el  = elevation dt s

> zenithAngleLimit dt s =
>    boolean "zenithAngleLimit" . Just $ zenithAngle dt s < deg2rad 85.0

For scheduling, use the specified tracking errors below.
Use different constants for MOC.

> trackingErrorLimit dt s = do
>     -- If it is decided to *always* attempt to use W2 wind (like in sims)
>     -- here is the code to toggle
>     -- w <- weather
>     -- wind' <- liftIO $ gbt_wind w dt
>     wind' <- getRealOrForecastedWind dt
>     boolean "trackingErrorLimit" $ calculateTRELimit maxTrackErr maxTrackErrArray wind' dt s 
>       where
>         maxTrackErr      = 0.2  -- Equation 25
>         maxTrackErrArray = 0.4  -- Equation 26
>     

Equation 13

> trackErr :: DateTime -> Float -> Frequency -> Float
> trackErr dt w f = rmsTrackingError dt w / (halfPwrBeamWidth f)

Equation 16

> trackErrArray :: Float -> Frequency -> Float
> trackErrArray w f = variableTrackingError w / (halfPwrBeamWidth f)

> calculateTRELimit :: Float -> Float -> Maybe Float -> DateTime -> Session -> Maybe Bool
> calculateTRELimit maxTrackErr maxTrackErrArray wind dt s = do
>     wind' <- wind
>     let f  = trackErr dt wind' (frequency s)
>     let fv = trackErrArray wind' (frequency s)
>     let limit = if usesMustang s then if fv <= maxTrackErrArray then True
>                                                                 else False
>                                  else if f  <= maxTrackErr then True
>                                                            else False
>     return limit

Equation 11

> rmsTrackingError :: DateTime -> Float -> Float
> rmsTrackingError dt w = do trackingError w (rmsTE dt)

Equation 15

> variableTrackingError :: Float -> Float
> variableTrackingError w = trackingError w epsilonZero

Scale the wind speed by 1.5 to account for weather differences between 
2003 (when calibration was performed) and 2009 (current weather station)

> trackingError :: Float -> Float -> Float
> trackingError w te = sqrt $ te ^ 2 + (abs w / (2.1 * 1.5)) ^ 4

> epsilonZero :: Float
> epsilonZero = 1.2

> atmosphericStabilityLimit dt s = do
>   w <- weather
>   di <- liftIO $ irradiance w dt
>   boolean "atmosphericStabilityLimit" $ calculateAtmStabilityLimit di (oType s) (frequency s) 

> calculateAtmStabilityLimit :: Maybe Float -> ObservingType -> Frequency -> Maybe Bool
> calculateAtmStabilityLimit di ot f = do
>   di' <- di
>   return $ if ot == Continuum &&
>               f > 2.0 &&
>               di' >= 330 then False
>                          else True

3.5 Other factors

> projectCompletion, thesisProject, scienceGrade :: ScoreFunc

> projectCompletion _ s = let
>     weight = 1000.0
>     total = fromIntegral (pAllottedT . project $ s)
>     left  = total - fromIntegral (pCommittedT  . project $ s)
>     percent = if total <= 0.0 then 0.0 else 100.0*(total - left)/total
>     in factor "projectCompletion" . Just $
>     if percent <= 0.0 then 1.0 else 1.0 + percent/weight

> thesisProject _ s = factor "thesisProject" . Just $
>     if thesis . project $ s then 1.05 else 1.0

> grade2Score :: Grade -> Score
> grade2Score g = g / 4.0

> scienceGrade dt s = factor "scienceGrade" . Just $ result
>   where
>     result
>       | haveTime dt s    = grade2Score . grade $ s
>       | otherwise        = 0.51
>     sem = dt2semester dt
>     pAvail = pAvailS sem . project $ s
>     sAvail = sAvailS sem s
>     haveTime dt s = sAvail > 0 && pAvail > 0

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
>     evalCNF av rs = all (\rg -> any (\r -> elem r av) rg) rs

> inWindows :: DateTime -> (Session -> [Window]) -> Session -> Score
> inWindows dt f s
>       | sType s == Open           = 1.0
>       | sType s == Fixed          = 1.0
>       | sType s == Elective       = 1.0
>       | any (inWindow dt) $ f s   = 1.0
>       | otherwise                 = 0.0

A single window can have multiple date ranges associated with, all of
which we need to check.

> inWindow :: DateTime -> Window -> Bool
> inWindow dt w = any (==True) $ map (inTimeRange' dt) $ wRanges w
>   where 
>     inTimeRange' dt (start, end) = inTimeRange dt start (diffMinutes end start)

> availWindows :: Session -> [Window]
> availWindows = filter (not . wComplete) . windows

> inAvailWindows :: ScoreFunc
> inAvailWindows dt s = factor "inWindows" . Just . inWindows dt availWindows $ s

> inAnyWindows :: ScoreFunc
> inAnyWindows dt s = factor "inWindows" . Just . inWindows dt windows $ s

Returns list of receivers that will be up at the given time.

> getReceivers :: DateTime -> ReceiverSchedule -> [Receiver]
> getReceivers dt rsched = 
>     case takeWhile (\(x, _) -> x <= dt) rsched of
>         [] -> []
>         xs -> snd $ last xs 

More Scoring Factors not covered in Memo 5.2

Is there an observer on site for this time and session?
Important, because on site observers get a boost.

> observerOnSite :: ScoreFunc
> observerOnSite dt s = factor "observerOnSite" . Just $ if (obsOnSite dt s) then 1.5 else 1.0

> obsOnSite :: DateTime -> Session -> Bool
> obsOnSite dt s = any (isOnSite dt) (obs s)
>   where 
>     obs s = observers . project $ s
>     isOnSite dt o = any (inDateRange dt) (reservations o) 

Is there an observer available for this time and session?
Rules (observer available if): 
   * if a single observer is on site (ignore their black outs)
   * if a single sancioned observer is not blacked out

> observerAvailable :: ScoreFunc
> observerAvailable dt s = boolean "observerAvailable" . Just $ obsAvailable dt s

> obsAvailable :: DateTime -> Session -> Bool
> obsAvailable dt s = ((obsOnSite dt s) || (remoteObsAvailable dt s)) && (requiredFriendsAvailable dt s)

> remoteObsAvailable :: DateTime -> Session -> Bool
> remoteObsAvailable dt s = not $ allObsBlackedOut dt obs
>   where
>     obs = filter sanctioned $ observers . project $ s

Note that this will return True if there are NO observers, but this case
is handled by previously filtering out observerless sessions.

> allObsBlackedOut :: DateTime -> [Observer] -> Bool
> allObsBlackedOut dt obs = all (isBlackedOut dt) obs
>   where 
>     isBlackedOut dt obs = any (inDateRange dt) (blackouts obs)

> requiredFriendsAvailable :: DateTime -> Session -> Bool
> requiredFriendsAvailable dt s = not $ anyObsBlackedOut dt friends
>   where
>     friends = requiredFriends . project $ s

Is anyone (of type Observer) blacked out for this time?

> anyObsBlackedOut :: DateTime -> [Observer] -> Bool
> anyObsBlackedOut dt obs | obs == [] = False
>                         | otherwise = any (isBlackedOut dt) obs
>   where 
>     isBlackedOut dt observer = any (inDateRange dt) (blackouts observer)

Project Blackouts are a simple version of user blackouts: if the 
datetime lands in any one of them, you score zero.

> projectBlackout :: ScoreFunc
> projectBlackout dt s = boolean "projectBlackout" . Just $ projectBlackout' dt s

> projectBlackout' :: DateTime -> Session -> Bool
> projectBlackout' dt s | bs == []  = True
>                       | otherwise = not $ any (inDateRange dt) bs
>   where
>     bs = pBlackouts . project $ s

The low rfi flag is used for avoiding RFI that is rampent during the daytime.

> needsLowRFI :: ScoreFunc
> needsLowRFI dt s = do
>     isLow <- liftIO $ needsLowRFI' dt s
>     boolean "needsLowRFI" . Just $ isLow

> needsLowRFI' :: DateTime -> Session -> IO Bool
> needsLowRFI' dt s = do
>     if lowRFI s
>         then isHighRFITime dt
>         else return True

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
> receiverBoost _ s = factor "receiverBoost" . Just $ if receiverBoost' s then 1.05 else 1.0

> receiverBoost' :: Session -> Bool
> receiverBoost' s | (grade s) < 3.8     = False
>                  | otherwise           =
>   any (\rg -> all (\r -> elem r boostRcvrs) rg) rgs
>   where
>     rgs = receivers s
>     boostRcvrs = [Rcvr_1070, Rcvr_342, Rcvr_450, Rcvr_800] --TBF: may change 

Scoring utilities

Score the given session at the given time:
   * using the passed in weather
   * possibly using measured wind speeds (w2 flag)

> scoreLocal :: Weather -> ScoreFunc -> Session -> DateTime -> Bool -> Scoring Score
> scoreLocal w' sf s dt w2 = local (\env -> env { envWeather = w', envMeasuredWind = w2}) $ do
>       fs <- sf dt s
>       return $ eval fs 

Compute the score for a given session at given time, but:
   * replacing weather w/ one for the given time
   * possibly using measured wind speeds (w2 flag)

> scoreForTime  :: ScoreFunc -> DateTime -> Bool -> Session -> Scoring Score 
> scoreForTime sf dt w2 s = do
>     w  <- weather
>     w' <- liftIO $ newWeather w (Just dt)
>     scoreLocal w' sf s dt w2

Compute the average score for a given session over an interval:
   * modify the weather to start at the time given
   * use measured wind speeds instead of forecasts
   * reject sessions that have quarters of score zero
This is for use when determining best backups to run in simulations.
Note: because this is not used in real scheduling then the fact
that it does not assume zero for the overhead quarters does not matter.

> avgScoreForTimeRealWind  :: ScoreFunc -> DateTime -> Minutes -> Session -> Scoring Score 
> avgScoreForTimeRealWind sf dt dur s = do
>     w  <- weather
>     w' <- liftIO $ newWeather w (Just dt)
>     scores <- mapM (\t -> scoreLocal w' sf s t True) times 
>     case length scores of
>       0 -> return 0.0
>       otherwise -> return $ sumScores scores / (fromIntegral . length $ scores)
>   where
>     -- TBF:  Using the measured wind speed for scoring in the future
>     -- is unrealistic, but damn convenient!
>     numQtrs = dur `div` quarter
>     times = [(q*quarter) `addMinutes` dt | q <- [0..(numQtrs-1)]]
>     sumScores scores = case dropWhile (>0.0) scores of
>         [] -> sum scores
>         otherwise -> 0.0 -- don't allow zero-scored quarters
> 

Computes the mean score of a range of non-zero quarterly scores where
the first score is ignored, e.g., if a sessions quarterly scores across
an hour are [a, b, c, d] then:

minutes              weighted mean score              weighted mean score (VLB)
-------              -------------------              -------------------------
0:15                   0                                0
0:30                   b/2                              0
0:45                   (b + c)/3                        c/3
1:00                   (b + c + d)/4                    (c + d)/4

> weightedMeanScore:: ObservingType -> [Score] -> Score
> weightedMeanScore ot ss = case ot of
>                   Vlbi      ->  weightedMeanScoreTail2 ss
>                   otherwise ->  weightedMeanScoreTail  ss

> weightedMeanScoreTail:: [Score] -> Score
> weightedMeanScoreTail ss = case ss of
>                   []      ->  0.0
>                   (_:[])  ->  0.0
>                   (_:rem) ->  (sum rem) / (fromIntegral . length $ ss)

> weightedMeanScoreTail2:: [Score] -> Score
> weightedMeanScoreTail2 ss = case ss of
>                   []        ->  0.0
>                   (_:[])    ->  0.0
>                   (_:_:[])  ->  0.0
>                   (_:_:rem) ->  (sum rem) / (fromIntegral . length $ ss)

> activeScores :: Session -> [Score] -> [Score]
> activeScores s ss = drop (getOverhead s) ss

TBF The fact that we have to pass in session is a kluge resulting from the
fact that we have not tied the knots properly among projects, sessions,
and periods.

> scorePeriod :: Period -> Session -> [Session] -> Weather -> ReceiverSchedule -> ReceiverTemperatures -> IO Score
> scorePeriod p s ss w rs rt = do
>   scores <- mapM scorePeriod' $ dts
>   let retval = if 0.0 `elem` (activeScores s scores)
>                then 0.0
>                else weightedMeanScore (oType s) scores
>   return retval
>     where
>   st = startTime p
>   scorePeriod' dt = do
>     fs <- runScoring w rs rt $ genPeriodScore st ss >>= \f -> f dt s
>     return $ eval fs
>   dts = [(i*quarter) `addMinutes` st | i <- [0..(((duration p) `div` quarter)-1)]]

FYI, this function has no unit tests, possibly because it is never used!
TBF WTF OMG BBQ: scorePeriod & scoreSession look like they could really 
share a lot of code, simply by passing in the Score ScoreFunc (genScore 
vs. genPeriodScore).

> scoreSession :: DateTime -> Minutes -> Session -> [Session] -> Weather -> ReceiverSchedule -> ReceiverTemperatures -> IO Score
> scoreSession st dur s ss w rs rt = do
>   scores <- mapM scoreSession' $ dts
>   -- TBF should this check only be done on that tail?
>   let retval = if 0.0 `elem` (activeScores s scores)
>                then 0.0
>                else weightedMeanScore (oType s) scores
>   return retval
>     where
>   scoreSession' dt = do
>     fs <- runScoring w rs rt $ genScore st ss >>= \f -> f dt s
>     return $ eval fs
>   dts = [(i*quarter) `addMinutes` st | i <- [0..((dur `div` quarter)-1)]]

These methods for scoring a session are to be used in conjunction with
Schedule's 'best' function.

> type BestScore = ScoreFunc -> DateTime -> Session -> Scoring Score

> firstScore :: BestScore
> firstScore sf dt s = do
>     factors <- sf dt s
>     return $ eval factors

Compute the average score for a given session over an interval.
Note: because this is not used in scheduling with Pack then the fact
that it does not assume zero for the first quarter does not matter.

> averageScore :: BestScore
> averageScore sf dt s = do
>     score <- totalScore sf dt dur s
>     return $! score / fromIntegral (dur `div` quarter)
>   where
>     dur = minDuration s

TBF: reverse the names of these two averageScore functions.
This is the more generic one, using dur that is passed in.

> averageScore' :: ScoreFunc -> DateTime -> Minutes -> Session -> Scoring Score 
> averageScore' sf dt dur s = do
>     score <- totalScore sf dt dur s
>     return $! score / fromIntegral (dur `div` quarter)

Compute the total score for a given session over an interval.
Note: because this is not used in scheduling with Pack then the fact
that it does not assume zero for the overhead quarters does not matter.

> totalScore :: ScoreFunc -> DateTime -> Minutes -> Session -> Scoring Score
> totalScore sf dt dur s = do
>     scores <- mapM (liftM eval . flip sf s) times
>     return $! addScores scores
>   where
>     times  = map (`addMinutes` dt) [0, quarter .. dur-1]

Add a set of scores, with the added complication that if any
individual score is zero then the end result must also be zero.

> addScores :: [Score] -> Score
> addScores = fromMaybe 0.0 . foldr' step (Just 0.0)
>   where
>     step s Nothing   = Nothing
>     step s (Just x)
>         | s <= 0.0 = Nothing
>         | otherwise  = Just $! x + s

For a start time, optional minimum/maximum durations, and session,
find the duration that yields the highest score.
Note if an alternate duration is provided then the smaller of
the provided duration and the session's duration is used.

> bestDuration :: ScoreFunc -> DateTime -> Maybe Minutes -> Maybe Minutes -> Session -> Scoring Nominee
> bestDuration sf dt lower upper session = do
>     -- scores' :: [Score]         -- all quarterly scores
>     scores' <- mapM (liftM eval . flip sf session) times
>     let ovhd = getOverhead session
>     -- scores :: [Score]          -- initial non-zero scores skipping overhead
>     let scores = (replicate ovhd 0.0) ++ (takeWhile (>0.0) . drop ovhd $ scores')
>     let sums = scanl1 (+) scores
>     --  sds :: [(Score, Minutes)] -- period sums and durations
>     let sds = dropWhile (\sd -> (snd sd) < shortest) [(s, d) | (s, d) <- zip sums durs]
>     --  mds :: [(Score, Minutes)] -- period means and durations
>     let mds = [(s / (fromIntegral $ (d `div` 15)), d) | (s, d) <- sds]
>     let result = foldl findBest (0.0, 0) mds
>     return $ (session, fst result, snd result)
>   where
>     shortest = maybe (minDuration session) (min . minDuration $ session) lower
>     timeLeft = min (pAvailT . project $ session) (sAvailT session)
>     longest = min timeLeft $ maybe (maxDuration session) (min . maxDuration $ session) upper
>     durs   = [quarter, 2*quarter .. longest]
>     times  = map (`addMinutes` dt) [0, quarter .. (longest - quarter)]
>     findBest x y = if (fst x) > (fst y) then x else y

For a start time, optional minimum/maximum durations, and a list
of sessions, generate a list of the associated best durations -- if
non-zero scored -- for each session.

> type Nominee = (Session, Score, Minutes)

> bestDurations :: ScoreFunc -> DateTime -> Maybe Minutes -> Maybe Minutes -> [Session] -> Scoring [Nominee]
> bestDurations  _  _     _     _   [] = do
>     return []
> bestDurations sf dt lower upper (s:ss) = do
>     result <- bestDuration sf dt lower upper s
>     remainder <- bestDurations sf dt lower upper ss
>     return $ result : remainder

> type Factor   = (String, Maybe Score)
> type Factors  = [Factor]

> type ReceiverSchedule = [(DateTime, [Receiver])]

This is the environment that the Scoring Monad is carrying around
to avoid long lists of repetitive parameters.

> data ScoringEnv = ScoringEnv {
>     envWeather      :: Weather
>   , envReceivers    :: ReceiverSchedule
>   , envMeasuredWind :: Bool
>   , envRcvrTemps    :: ReceiverTemperatures
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

> receiverTemperatures :: Scoring ReceiverTemperatures
> receiverTemperatures = asks envRcvrTemps

The Scoring monad encapsulates the concept of a scoring action,
all the scoring functions live in the monad so they can
execute scoring actions.

A Trace collects/logs information about the execution of a monad.

> data Trace = Timestamp DateTime
>            | FreqPressureHistory (Array Band Float)
>            | FreqPressureBinHistory (Array Band (Int, Int))
>            | RaPressureHistory (Array Int Float)
>            | Cancellation Period
>            | WindowPeriods (Window, Maybe Period, Period)
>            deriving (Eq, Show)

> type Scoring = RWST ScoringEnv [Trace] () IO

A scoring action returns its results inside the Scoring monad,
runScoring allows one to extract those results from the monad
resulting in simple types rather than monadic types.

> runScoring      :: Weather -> ReceiverSchedule -> ReceiverTemperatures -> Scoring t -> IO t
> runScoring w rs rt = liftM fst . runScoring' w rs rt

> runScoring'        :: Weather -> ReceiverSchedule -> ReceiverTemperatures -> Scoring t -> IO (t, [Trace])
> runScoring' w rs rt f = evalRWST f (ScoringEnv w rs False rt) ()

This allows us to run scoring multiple times, all within the same trace.
Mainly useful for simulation.

> -- runScoring''        :: Weather -> ReceiverSchedule -> Scoring t -> IO t
> -- runScoring'' w rs f = runReaderT f $ ScoringEnv w rs

Because ScoreFunc returns lists of factors, this function allows
us to easily return a list.

> factor          :: String -> Maybe Score -> Scoring Factors
> factor name val = return [(name, val)]

Sub-class of scoring actions that return a list of factors

> type ScoreFunc = DateTime -> Session -> Scoring Factors 

> instance Show (a -> b) where
>     show _ = "ScoreFunc"

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
>     step (_, Just f)  s = s * f

> genScore          :: DateTime -> [Session] -> Scoring ScoreFunc
> genScore dt sessions = do
>     raPressure   <- genRightAscensionPressure dt sessions
>     freqPressure <- genFrequencyPressure dt sessions
>     genScore' raPressure freqPressure

> genScore' raPressure freqPressure = return $ \dt s -> do
>     effs <- calcEfficiency dt s
>     score (scoringFactors effs raPressure freqPressure) dt s

> positionFactors :: Session -> DateTime -> IO Factors
> positionFactors s dt = do
>   let ha' = rad2hrs . hourAngle dt $ s
>   let el' = rad2deg . elevation dt $ s
>   --                     hours                    degrees
>   return [("hourAngle", Just ha'), ("elevation", Just el')]

> subfactorFactors :: Session -> Weather -> ReceiverTemperatures -> DateTime -> IO Factors
> subfactorFactors s w rt dt = do
>   sysNoiseTemp <- systemNoiseTemperature w rt dt s
>   sysNoiseTempPrime <- systemNoiseTemperaturePrime w rt dt s
>   minSysNoiseTempPrime <- minTsys' w dt s
>   return [("sysNoiseTemp",      sysNoiseTemp)
>         , ("sysNoiseTempPrime", sysNoiseTempPrime)
>         , ("minSysNoiseTempPrime", minSysNoiseTempPrime)]

> weatherFactors :: Session -> Weather -> DateTime -> IO Factors
> weatherFactors s w dt = do
>   wind' <- wind w dt
>   wind'' <- wind_mph w dt
>   irradiance' <- irradiance w dt
>   opacity' <- opacity w dt freq
>   tsys' <- tsys w dt freq 
>   return [("wind_mph", wind''), ("wind_ms", wind')
>         , ("irradiance", irradiance')
>         , ("opacity", opacity'), ("tsys", tsys')
>          ]
>     where
>   freq = frequency s

> scoreFactors :: Session -> Weather -> [Session] -> DateTime -> Minutes -> ReceiverSchedule -> IO [Factors]
> scoreFactors s w ss st dur rs = do
>   rt <- getReceiverTemperatures
>   fs <- runScoring w rs rt $ genPeriodScore st ss
>   let score' w dt = runScoring w rs rt $ do
>       sf <- fs dt s
>       return sf
>   factors <- mapM (score' w) times
>   return factors
>     where
>       times = [(15*q) `addMinutes` st | q <- [0..(numQtrs-1)]]
>       numQtrs = dur `div` 15

> scoreElements :: Session -> Weather -> ReceiverTemperatures -> [Session] -> DateTime -> Minutes -> ReceiverSchedule -> IO [Factors]
> scoreElements s w rt ss st dur rs = do
>   fs <- runScoring w rs rt $ genPeriodScore st ss
>   let score' w dt = runScoring w rs rt $ do
>       sf <- fs dt s
>       return sf
>   pfactors <- mapM (positionFactors s) times
>   wfactors <- mapM (weatherFactors s w) times
>   ffactors <- mapM (subfactorFactors s w rt) times
>   sfactors <- mapM (score' w) times
>   return $ zipWith4 (\a b c d -> a ++ b ++ c ++ d) pfactors wfactors ffactors sfactors
>     where
>       times = [(15*q) `addMinutes` st | q <- [0..(numQtrs-1)]]
>       numQtrs = dur `div` 15

sfactors :: Maybe (Float, Float) -> ScoreFunc -> ScoreFunc -> [ScoreFunc]
sfactors effs rap fp = scoringFactors effs rap fp

This version is used for scheduling, i.e., all factors must be accounted
for to generate new periods.

> scoringFactors :: Maybe (Score, Float) -> ScoreFunc -> ScoreFunc -> [ScoreFunc]
> scoringFactors effs raPressure freqPressure =
>        [
>         stringency
>       , (atmosphericEfficiency' . fmap fst) effs
>       , surfaceObservingEfficiency
>       , trackingEfficiency
>       , raPressure
>       , freqPressure
>       , observingEfficiencyLimit
>       , (hourAngleLimit' . fmap snd) effs
>       , zenithAngleLimit
>       , trackingErrorLimit
>       , atmosphericStabilityLimit
>       , scienceGrade
>       , thesisProject
>       , projectCompletion
>       , observerOnSite
>       , receiver
>       , needsLowRFI
>       , lstExcepted
>       , enoughTimeBetween
>       , observerAvailable
>       , projectBlackout
>       , inAvailWindows
>        ]

This version exists for the nominees panel.  The commented out scoring factors
may or may not be re-activated depending on the user's choices in the
vacancy control panel.

> genPartScore          :: DateTime -> [ScoreFunc] -> [Session] -> Scoring ScoreFunc
> genPartScore dt sfs sessions = do
>     raPressure   <- genRightAscensionPressure dt sessions
>     freqPressure <- genFrequencyPressure dt sessions
>     genPartScore' sfs raPressure freqPressure

> genPartScore' sfs raPressure freqPressure = return $ \dt s -> do
>     effs <- calcEfficiency dt s
>     score ([
>         stringency
>       , (atmosphericEfficiency' . fmap fst) effs
>       , surfaceObservingEfficiency
>       , trackingEfficiency
>       , raPressure
>       , freqPressure
>       , observingEfficiencyLimit
>       , (hourAngleLimit' . fmap snd) effs
>       , zenithAngleLimit
>       , trackingErrorLimit
>       , atmosphericStabilityLimit
>       , scienceGrade
>       , thesisProject
>       , projectCompletion
>       , observerOnSite
>       , receiver
>       --, needsLowRFI
>       , lstExcepted
>       --, enoughTimeBetween
>       --, observerAvailable
>       , projectBlackout
>       , inAvailWindows
>       ] ++ sfs) dt s

> genPeriodScore          :: DateTime -> [Session] -> Scoring ScoreFunc
> genPeriodScore dt sessions = do
>     raPressure   <- genRightAscensionPressure dt sessions
>     freqPressure <- genFrequencyPressure dt sessions
>     genPeriodScore' raPressure freqPressure

> genPeriodScore' raPressure freqPressure = return $ \dt s -> do
>     effs <- calcEfficiency dt s
>     score (periodFactors effs raPressure freqPressure) dt s

This version allows the obtaining of scores for periods that may
already exist, i.e., do not let their existence generate uninteresting
scores of zero.

> periodFactors :: Maybe (Score, Float) -> ScoreFunc -> ScoreFunc -> [ScoreFunc]
> periodFactors effs raPressure freqPressure =
>        [
>         stringency
>       , (atmosphericEfficiency' . fmap fst) effs
>       , surfaceObservingEfficiency
>       , trackingEfficiency
>       , raPressure
>       , freqPressure
>       , observingEfficiencyLimit
>       , (hourAngleLimit' . fmap snd) effs
>       , zenithAngleLimit
>       , trackingErrorLimit
>       , atmosphericStabilityLimit
>       , scienceGrade
>       , thesisProject
>       , projectCompletion
>       , observerOnSite
>       , receiver
>       , needsLowRFI
>       , lstExcepted
>       --, enoughTimeBetween
>       , observerAvailable
>       , projectBlackout
>       , inAnyWindows
>        ]

Convenience function for translating go/no-go into a factor.

> boolean :: String -> Maybe Bool -> Scoring Factors
> boolean name = factor name . fmap (\b -> if b then 1.0 else 0.0)

Uses the datetime used to construct the weather object to determine whether
to return forecasted wind values, or wind values from weather station 2.
If weather station 2 values are not available, the function falls back
to using forecasted values.

> getRealOrForecastedWind :: DateTime -> Scoring (Maybe Float)
> getRealOrForecastedWind dt = do
>   w <- weather
>   let wDt = forecast w
>   let dt' = roundToHour dt
>   wind' <- if dt' <= wDt
>            then do
>                mw <- liftIO $ gbt_wind w dt
>                uw <- if mw == Nothing
>                      then liftIO $ wind w dt
>                      else return mw
>                return uw
>            else liftIO $ wind w dt
>   return wind'

Convenience function for factoring a Session over it's Period's duration
Note: The score recorded in the period (pScore) is the average over it's 
duration.  So, we should be able to reproduce that using this function, 
the original pool of sessions (for the correct pressures), and the
forecast used to generate pScore (using the time pScore was calculated
for, pForecast).

> factorPeriod :: Period -> ScoreFunc -> Scoring [Factors]
> factorPeriod p sf = mapM (factorPeriod' sf) dts
>   where
>     factorPeriod' sf dt = sf dt (session p)
>     dts = [(i*quarter) `addMinutes` (startTime p) | i <- [0..((duration p) `div` quarter)]]

Basic Utility that attempts to emulate the Beta Test's Scoring Tab:

> scoringInfo :: Session -> [Session] -> DateTime -> Minutes -> ReceiverSchedule -> IO ()
> scoringInfo s ss dt dur rs = do
>   w <- liftIO $ getWeather Nothing
>   rt <- getReceiverTemperatures
>   factors <- scoreFactors s w ss dt dur rs
>   let scores = map eval factors
>   elements <- scoreElements s w rt ss dt dur rs
>   let info = printFactors $ zip times $ zip scores elements
>   let report = "Scoring Info for session: " ++ (sName s) ++ "\n\n" ++ info
>   putStrLn report
>   writeFile "scoringInfo.txt" report
>     where
>       times = [(15*q) `addMinutes` dt | q <- [0..numQtrs]]
>       numQtrs = dur `div` 15

> printFactors :: [(DateTime, (Score, Factors))] -> String
> printFactors factors = concatMap factorsToString factors 

> factorsToString :: (DateTime, (Score, Factors)) -> String
> factorsToString dtFactors = (toSqlString dt) ++ ":\nscore: " ++ (show score) ++ "\n" ++(concatMap factorToString factors) ++ "\n"
>   where
>     dt      = fst dtFactors
>     factors = snd . snd $ dtFactors
>     score   = fst . snd $ dtFactors

> factorToString :: Factor -> String
> factorToString factor = (show factor) ++ "\n" 

TBF: this is a cheap way of checking the receiver type.
We need to be checking for filled arrays ...

> usesMustang :: Session -> Bool
> usesMustang s = Rcvr_PAR `elem` (concat $ receivers s)

Quick Check properties:

> prop_efficiency = forAll genProject $ \p ->
>   let es = map calcEff (sessions p) in normalized es  
>   where
>     calcEff s = unsafePerformIO $ do
>       rt <- getReceiverTemperatures
>       w <- theWeather
>       w' <- newWeather w (Just $ fromGregorian 2006 10 14 9 15 2)
>       let dt = fromGregorian 2006 10 15 12 0 0
>       Just result <- runScoring w' [] rt (efficiency dt s)
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
>       dt = fromGregorian 2006 6 1 0 0 0
>       fp = getPressureFunction (genFrequencyPressure dt)
>       greaterOrEqToOne xs = dropWhile (>=1) xs == []

> prop_rightAscensionPressure = forAll genProject $ \p ->
>   let es = map (getScoringResult fp) (sessions p) in greaterOrEqToOne es
>     where
>       dt = fromGregorian 2006 6 1 0 0 0
>       fp = getPressureFunction (genRightAscensionPressure dt)
>       greaterOrEqToOne xs = dropWhile (>=1) xs == []

Utilities for QuickCheck properties:

> getPressureFunction f = unsafePerformIO $ do
>     g <- getStdGen
>     let sessions = generate 0 g $ genSessions 100
>     runScoring undefined [] undefined $ f sessions

> checkBoolScore p sf = let es = map (getScoringResult sf) (sessions p) in areBools es

> getScoringResult sf s = unsafePerformIO $ do
>     rt <- getReceiverTemperatures
>     w <- theWeather
>     w' <- newWeather w (Just $ fromGregorian 2006 4 15 0 0 0) 
>     let dt = fromGregorian 2006 4 15 16 0 0
>     [(_, Just result)] <- runScoring w' [] rt (sf dt s)
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
