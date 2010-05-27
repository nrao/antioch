> {-# OPTIONS -XFlexibleContexts #-}

> module Antioch.Score where

> import Antioch.DateTime
> import Antioch.Generators
> import Antioch.Types
> import Antioch.TimeAccounting
> import Antioch.Utilities
> import Antioch.Weather
> import Control.Monad.RWS.Strict
> import Control.Monad      (liftM2)
> import Data.Array
> import Data.Array.IArray  (amap)
> import Data.Array.ST
> import Data.Foldable      (foldr')
> import Data.List
> import Data.Maybe         (fromMaybe, isJust, fromJust)
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
>     -- MUSTANG uses kt @ 30 GHz
>     tk  <- if not . usesMustang $ s then kineticTemperature dt s else kineticTemperature dt s { frequency = 30.0 }
>     w   <- weather
>     -- MUSTANG computes opticalDepth using zod @ 30 GHz
>     zod <- if not . usesMustang $ s then zenithOpticalDepth dt s else zenithOpticalDepth dt s { frequency = 30.0 }
>     -- MUSTANG computes minTsysPrime instead of looking it up
>     lookupMinTsysPrime <-  liftIO $ minTSysPrime w (frequency s) $ elevation'
>     let mustangMinTsysPrime = if usesMustang s then Just $ calcMustangTSysPrime za else Nothing
>     let minTsysPrime' = if usesMustang s then mustangMinTsysPrime else lookupMinTsysPrime
>     return $ do
>         tk' <- tk
>         zod' <- zod
>         minTsysPrime'' <- minTsysPrime' >>= Just . (*xf)
>         -- MUSTANG uses rcvr temp of 120.0 K
>         let [eff, effTransit] = if not . usesMustang $ s then  map (calcEff trx tk' minTsysPrime'' zod') [za, zat] else map (mustangCalcEff 120.0 tk' minTsysPrime'' zod') [za, zat]
>         return (eff, eff / effTransit)
>   where
>     za  = zenithAngle dt s
>     zat = zenithAngleAtTransit s
>     elevation' = (pi/2 - za)
>     xf = xi s
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
>     mustangCalcEff trx tk minTsysPrime' zod_30 za = (minTsysPrime' / tsys') ^2
>       where
>         -- Round off to the nearest degree to align with hist. min. opacities
>         rndZa = deg2rad . realToFrac . round . rad2deg $ za
>         -- Equation 4 & 6
>         opticalDepth = ((zod_30 - 0.023)*6.0) / (cos . min 1.5 $ rndZa)
>
>         -- Equation 7
>         tsys  = trx + 5.7 + tk * (1 - exp (-opticalDepth))
>
>         tsys' = exp opticalDepth * tsys

From ProjectRequest23Q110, requirements for MUSTANG atmosphericEfficiency

> calcMustangTSysPrime :: Radians -> Float
> calcMustangTSysPrime za = exp opticalDepth * tsys
>   where
>     trx = 120.0
>     tk = 260.0
>     -- Round off to the nearest degree to align with hist. min. opacities
>     rndZa = deg2rad . realToFrac . round . rad2deg $ za
>     opticalDepth = 0.04 / (cos . min 1.5 $ rndZa)
>     -- Equation 7
>     tsys = trx + 5.7  + tk * (1 - exp (-opticalDepth))

> minTsys' :: Weather -> DateTime -> Session -> IO (Maybe Float)
> minTsys' w dt s = do
>     mts' <- minTSysPrime w (frequency s) (elevation dt s)
>     return $ do
>         mts' >>= Just . (*xf)
>     where
>       xf = xi s

> systemNoiseTemperature :: Weather -> DateTime -> Session -> IO (Maybe Float)
> systemNoiseTemperature w dt s = runScoring w [] $ do
>     zod <- zenithOpticalDepth dt s
>     tk  <- kineticTemperature dt s
>     let trx = receiverTemperature dt s
>     let za  = zenithAngle dt s
>     let rndZa = deg2rad . realToFrac . round . rad2deg $ za
>     return $ liftM2 (\x y ->
>         let opticalDepth = y / (cos . min 1.5 $ rndZa) in
>         -- Equation 7
>         trx + 5.7 + x * (1 - exp (-opticalDepth))) tk zod

> systemNoiseTemperature' :: Weather -> DateTime -> Session -> IO (Maybe Float)
> systemNoiseTemperature' w dt s = runScoring w [] $ do
>     zod <- zenithOpticalDepth dt s
>     tk  <- kineticTemperature dt s
>     let trx = receiverTemperature dt s
>     let za  = zenithAngle dt s
>     let rndZa = deg2rad . realToFrac . round . rad2deg $ za
>     return $ liftM2 (\x y ->
>         let opticalDepth = y / (cos . min 1.5 $ rndZa) in
>         -- Equation 7
>         (exp opticalDepth) * (trx + 5.7 + x * (1 - exp (-opticalDepth)))) tk zod

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

For calculating the atmospheric efficiency, MUSTANG uses

> mustangZenithOpticalDepth      :: DateTime -> Session -> Scoring (Maybe Float)
> mustangZenithOpticalDepth dt s = do
>     w <- weather
>     liftIO $ opacity w dt (frequency s)


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
> atmosphericOpacity' eff dt s = do
>     let eff' = maybe Nothing (Just . min 1.0) eff
>     factor "atmosphericOpacity" eff'

> surfaceObservingEfficiency dt s = factor "surfaceObservingEfficiency" . Just $
>     if isPTCSDayTime_V2 dt
>     then
>         -- Equation 9
>         --exp (-(k * frequency s ^ 2 * epsilonFactor))
>         exp (-(k * freq ^ 2 * epsilonFactor))
>         -- Equation 10
>         -- exp (-((fromIntegral . round . frequency $ s)/69.2) ^ 2)
>     else
>         1.0
>   where
>     c = 299792485.0
>     -- As of 2009-12-16: day = 400, night = 340 (microns)
>     epsilonDay   = 0.35
>     epsilonNight = 0.26
>     epsilonFactor = epsilonDay ^ 2 - epsilonNight ^ 2
>     k = 32.0 * pi ^ 2 * 1e12 / (c ^ 2)
>     -- TBF: temp fix for MUSTANG - make sure we use 90 GHz
>     freq = if usesMustang s then 90.0 else frequency s 

> theta :: Float -> Float
> theta f = 740.0 / f

> rmsTE :: DateTime -> Float
> rmsTE dt = if isPTCSDayTime dt then sigmaDay else sigmaNight
>   where
>     sigmaDay = 3.3
>     sigmaNight = 2.8

> trackingEfficiency dt s = do
>   wind' <- getRealOrForecastedWind dt
>   factor "trackingEfficiency" $ calculateTE wind' dt s

> calculateTE :: Maybe Float -> DateTime -> Session -> Maybe Float
> calculateTE wind dt s = do
>     wind' <- wind
>     -- Equation 12 - uses f
>     let rmsTE' = rmsTrackingError dt wind'
>     let f = (rmsTE' / theta')
>     -- Equation 27 - uses fv (TBF: temp. fix for MUSTANG)
>     -- here we differ by Eq. 27 by dividing variableTrackingError by 2
>     let fv = (variableTrackingError wind' / 2) / theta'
>     let rt = if (usesMustang s) then fv else f
>     return $ (1.0 + 4.0 * log 2.0 * rt ^ 2) ^^ (-2)
>   where
>     theta' = theta . frequency $ s


> minimumObservingConditions  :: DateTime -> Session -> Scoring (Maybe Bool)
> minimumObservingConditions dt s = do
>    w  <- weather
>    w' <- liftIO $ newWeather w (Just dt)
>    local (\env -> env { envWeather = w', envMeasuredWind = True}) $ do
>      -- TBF: temporary fix for MUSTANG
>      let minObs = if usesMustang s then 0.5 else minObservingEff . frequency $ s
>      fs <- observingEfficiency dt s
>      let obsEff' = eval fs
>      [(_, trkErrLimit')] <- trackingErrorLimit dt s
>      let trkErrLimit = case trkErrLimit' of 
>                             Just x  -> x
>                             Nothing -> 1
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
>     stringency' <- liftIO $ totalStringency w (frequency s) elevation'
>     -- TBF: temporary fix for MUSTANG
>     let stringency'' = if usesMustang s then (Just 9.0) else stringency'
>     factor "stringency" stringency''
>   where
>     elevation' = pi/2 - zenithAngleAtTransit s

3.3 Pressure Feedback

Generate a scoring function having the pressure factors.

> genFrequencyPressure :: DateTime -> [Session] -> Scoring ScoreFunc
> genFrequencyPressure dt sessions = genFrequencyPressure' factors
>   where
>     bins    = initBins dt (minBound, maxBound) band sessions
>     factors = binsToFactors bins

TBF: temporary fix for MUSTANG

> getBand :: Session -> Band
> getBand s = if usesMustang s then Q else band s

> genFrequencyPressure' :: (MonadWriter [Trace] m) => Array Band Float -> m ScoreFunc
> genFrequencyPressure' factors = do
>     tell [FreqPressureHistory factors]
>     return $ frequencyPressure factors getBand

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

All of this applies to a given semester and grade B or higher sessions.

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
of all completed time billed, i.e., expired.
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

The result is that the computing of pressures become somewhat dynamic
without ignoring  successful observation time.

> initBins' dt bounds f xs = do
>     arr <- newArray bounds (0, 0)
>     for xs $ \x -> do
>         let bin = f x
>         (t, c) <- readArray arr bin
>         writeArray arr bin $! (t + rho dt x + sPastS dt x, c + sPastS dt x)
>     return arr
>   where
>     for xs f = foldr ((>>) . f) (return ()) xs
>     rho dt s
>       -- the max prevents against negative remainders, i.e.,
>       -- over-scheduled sessions
>       | isActive s = max 0 (expired dt s)
>       | otherwise  = 0
>     isActive s = (authorized s) && (not . sComplete $ s)

> expired :: DateTime -> Session -> Minutes
> expired dt s
>   | allot_min == allot_sem = allot_sem - (sPastS dt s)
>   | otherwise              = allot_tot - (sPastT dt s)
>     where
>       allot_sem = sAllottedS s
>       allot_tot = sAllottedT s
>       allot_min = min allot_tot allot_sem

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
>     -- Note: modification to Project Note 5.2 (helpdesk-dss #1559)
>     sigma = if (frequency s) >= 18.0
>             then 0.1
>             else 0.02
>     -- TBF: temporary fix for MUSTANG
>     minObsEff = if usesMustang s then 0.5 else minObservingEff . frequency $ s
>     fac = factor "observingEfficiencyLimit" . Just

TBF: include the elevation limit pattern matching once this is sponsor tested.

> hourAngleLimit        dt s | isJust . elLimit $ s = elevationLimit dt s
>                            | otherwise = efficiencyHA dt s' >>= \effHA -> hourAngleLimit' effHA dt s'
>   where
>     s' = if usesMustang s then s { frequency = 40.0 } else s

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

> trackingErrorLimit dt s = do
>     wind' <- getRealOrForecastedWind dt
>     boolean "trackingErrorLimit" $ calculateTRELimit wind' dt s 
>     

> calculateTRELimit :: Maybe Float -> DateTime -> Session -> Maybe Bool
> calculateTRELimit wind dt s = do
>         wind' <- wind
>         -- Equation 25
>         let f25 = rmsTrackingError dt wind' / (theta . frequency $ s)
>         -- Equation 26, but with variableTracking divided by 2
>         let f26 = (variableTrackingError wind' / 2) / (theta . frequency $ s)
>         -- TBF: temporary fix for MUSTANG
>         let f = if usesMustang s then f26 else f25
>         return $ f <= maxErr
>   where
>     maxErr = 0.2 

>  -- Equation 11
> rmsTrackingError dt w = sqrt (rmsTE dt ^ 2 + (abs w / 2.1) ^ 4)

>  -- Equation 15
> variableTrackingError w = sqrt (variableTE ^ 2 + (abs w / 2.1) ^ 4)
>   where
>     variableTE = 1.2

> atmosphericStabilityLimit _ _ =
>                            factor "atmosphericStabilityLimit" . Just $ 1.0

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
>       | any inWindow . f $ s      = 1.0
>       | otherwise                 = 0.0
>   where
>     inWindow w = (wStart w) <= dt && dt <= (wEnd w)
>     -- window ends 1 minute before midnight of the last day
>     wEnd w = (wLength w) `addMinutes` (wStart w)
>     wLength w = (wDuration w) + ((60 * 24) - 1)

> availWindows :: Session -> [Window]
> availWindows = filter (not . wHasChosen) . windows

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
> obsAvailable dt s = (obsOnSite dt s) || (remoteObsAvailable dt s)

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
that it does not assume zero for the first quarter does not matter.

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
>     -- is unrealistic, but damn convient!
>     numQtrs = dur `div` quarter
>     times = [(q*quarter) `addMinutes'` dt | q <- [0..(numQtrs-1)]]
>     sumScores scores = case dropWhile (>0.0) scores of
>         [] -> sum scores
>         otherwise -> 0.0 -- don't allow zero-scored quarters
> 

Computes the mean score of a range of non-zero quarterly scores where
the first score is ignored, e.g., if a sessions quarterly scores across
an hour are [a, b, c, d] then:

minutes              weighted mean score
-------              -------------------
0:15                   0
0:30                   b/2
0:45                   (b + c)/3
1:00                   (b + c + d)/4

> weightedMeanScore:: [Score] -> Score
> weightedMeanScore ss = case ss of
>                   []      ->  0.0
>                   (_:[])  ->  0.0
>                   (_:rem) ->  (sum rem) / (fromIntegral . length $ ss)

TBF The fact that we have to pass in session is a kluge resulting from the
fact that we have not tied the knots properly among projects, sessions,
and periods.

> scorePeriod :: Period -> Session -> [Session] -> Weather -> ReceiverSchedule -> IO Score
> scorePeriod p s ss w rs = do
>   scores <- mapM scorePeriod' $ dts
>   let retval = if 0.0 `elem` scores
>                then 0.0
>                else weightedMeanScore scores
>   return retval
>     where
>   st = startTime p
>   scorePeriod' dt = do
>     fs <- runScoring w rs $ genPeriodScore st ss >>= \f -> f dt s
>     return $ eval fs
>   dts = [(i*quarter) `addMinutes'` st | i <- [0..(((duration p) `div` quarter)-1)]]

> scoreSession :: DateTime -> Minutes -> Session -> [Session] -> Weather -> ReceiverSchedule -> IO Score
> scoreSession st dur s ss w rs = do
>   scores <- mapM scoreSession' $ dts
>   let retval = if elem 0.0 scores
>                then 0.0
>                else weightedMeanScore scores
>   return retval
>     where
>   scoreSession' dt = do
>     fs <- runScoring w rs $ genScore st ss >>= \f -> f dt s
>     return $ eval fs
>   dts = [(i*quarter) `addMinutes'` st | i <- [0..((dur `div` quarter)-1)]]

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

Compute the total score for a given session over an interval.
Note: because this is not used in scheduling with Pack then the fact
that it does not assume zero for the first quarter does not matter.

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
>         | s <= 0.0 = Nothing
>         | otherwise  = Just $! x + s

For a start time, optional minimum/maximum durations, and session,
find the duration that yields the highest score.
Note if an alternate duration is provided then the smaller of
the provided duration and the session's duration is used.

> bestDuration :: ScoreFunc -> DateTime -> Maybe Minutes -> Maybe Minutes -> Session -> Scoring Nominee
> bestDuration sf dt lower upper session = do
>     scores <- mapM (liftM eval . flip sf session) times
>     let sums = case scores of
>                     []     -> []
>                     (x:[]) -> [0.0::Score]
>                     (x:xs) -> (0.0:(scanl1 (+) . takeWhile (> 0.0) $ xs))
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
>     times  = map (`addMinutes'` dt) [0, quarter .. (longest - quarter)]
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

> subfactorFactors :: Session -> Weather -> DateTime -> IO Factors
> subfactorFactors s w dt = do
>   sysNoiseTemp <- systemNoiseTemperature w dt s
>   sysNoiseTempPrime <- systemNoiseTemperature' w dt s
>   minSysNoiseTempPrime <- minTsys' w dt s
>   return [("sysNoiseTemp",      sysNoiseTemp)
>         , ("sysNoiseTempPrime", sysNoiseTempPrime)
>         , ("minSysNoiseTempPrime", minSysNoiseTempPrime)]

> weatherFactors :: Session -> Weather -> DateTime -> IO Factors
> weatherFactors s w dt = do
>   wind' <- wind w dt
>   wind'' <- wind_mph w dt
>   opacity' <- opacity w dt freq
>   tsys' <- tsys w dt freq 
>   return [("wind_mph", wind''), ("wind_ms", wind')
>         , ("opacity", opacity'), ("tsys", tsys')]
>     where
>   freq = if usesMustang s then 30.0 else frequency s

> scoreFactors :: Session -> Weather -> [Session] -> DateTime -> Minutes -> ReceiverSchedule -> IO [Factors]
> scoreFactors s w ss st dur rs = do
>   fs <- runScoring w rs $ genPeriodScore st ss
>   let score' w dt = runScoring w rs $ do
>       sf <- fs dt s
>       return sf
>   factors <- mapM (score' w) times
>   return factors
>     where
>       times = [(15*q) `addMinutes'` st | q <- [0..(numQtrs-1)]]
>       numQtrs = dur `div` 15

> scoreElements :: Session -> Weather -> [Session] -> DateTime -> Minutes -> ReceiverSchedule -> IO [Factors]
> scoreElements s w ss st dur rs = do
>   fs <- runScoring w rs $ genPeriodScore st ss
>   let score' w dt = runScoring w rs $ do
>       sf <- fs dt s
>       return sf
>   pfactors <- mapM (positionFactors s) times
>   wfactors <- mapM (weatherFactors s w) times
>   ffactors <- mapM (subfactorFactors s w) times
>   sfactors <- mapM (score' w) times
>   return $ zipWith4 (\a b c d -> a ++ b ++ c ++ d) pfactors wfactors ffactors sfactors
>     where
>       times = [(15*q) `addMinutes'` st | q <- [0..(numQtrs-1)]]
>       numQtrs = dur `div` 15

sfactors :: Maybe (Float, Float) -> ScoreFunc -> ScoreFunc -> [ScoreFunc]
sfactors effs rap fp = scoringFactors effs rap fp

This version is used for scheduling, i.e., all factors must be accounted
for to generate new periods.

> scoringFactors :: Maybe (Score, Float) -> ScoreFunc -> ScoreFunc -> [ScoreFunc]
> scoringFactors effs raPressure freqPressure =
>        [
>         stringency
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
>       , scienceGrade
>       , thesisProject
>       , projectCompletion
>       , observerOnSite
>       , receiver
>       , needsLowRFI
>       , lstExcepted
>       , enoughTimeBetween
>       , observerAvailable
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
>       , scienceGrade
>       , thesisProject
>       , projectCompletion
>       , observerOnSite
>       , receiver
>       --, needsLowRFI
>       , lstExcepted
>       --, enoughTimeBetween
>       --, observerAvailable
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
>       , scienceGrade
>       , thesisProject
>       , projectCompletion
>       , observerOnSite
>       , receiver
>       , needsLowRFI
>       , lstExcepted
>       --, enoughTimeBetween
>       , observerAvailable
>       , inAnyWindows
>        ]

Convenience function for translating go/no-go into a factor.

> boolean :: String -> Maybe Bool -> Scoring Factors
> boolean name = factor name . fmap (\b -> if b then 1.0 else 0.0)

Uses the datetime used to construct the weather object to determine whether
to return forecasted wind values, or wind values from weather station 2.

> getRealOrForecastedWind :: DateTime -> Scoring (Maybe Float)
> getRealOrForecastedWind dt = do
>   w <- weather
>   let wDt = forecast w
>   let dt' = roundToHour dt
>   wind' <- if dt' < wDt
>            then liftIO $ w2_wind w dt
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
>     dts = [(i*quarter) `addMinutes'` (startTime p) | i <- [0..((duration p) `div` quarter)]]

Basic Utility that attempts to emulate the Beta Test's Scoring Tab:

> scoringInfo :: Session -> [Session] -> DateTime -> Minutes -> ReceiverSchedule -> IO ()
> scoringInfo s ss dt dur rs = do
>   w <- liftIO $ getWeather Nothing
>   factors <- scoreFactors s w ss dt dur rs
>   let scores = map eval factors
>   elements <- scoreElements s w ss dt dur rs
>   let info = printFactors $ zip times $ zip scores elements
>   let report = "Scoring Info for session: " ++ (sName s) ++ "\n\n" ++ info
>   putStrLn report
>   writeFile "scoringInfo.txt" report
>     where
>       times = [(15*q) `addMinutes'` dt | q <- [0..numQtrs]]
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

Utility for the MUSTANG temporary fix: no matter what the combination, 
returns true if Rcvr_PAR is listed as one of it's rcvrs.
TBF: we aren't using the frequncy here, is that okay?

> usesMustang :: Session -> Bool
> usesMustang s = any (==True) $ map (any (==Rcvr_PAR)) $ receivers s

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
