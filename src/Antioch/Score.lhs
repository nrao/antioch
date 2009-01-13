> module Antioch.Score where

> import Antioch.DateTime
> import Antioch.Generators
> import Antioch.Types
> import Antioch.Utilities
> import Antioch.Weather
> import Control.Monad.Identity
> import Control.Monad.Reader
> import Data.Array
> import Data.Array.IArray (amap)
> import Data.Array.ST
> import Data.List

> type Factor   = (String, Maybe Score)
> type Factors  = [Factor]

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
>     return $ do
>         tk' <- tk
>         zod' <- zod
>         minTsysPrime' <- minTSysPrime w (frequency s) elevation
>         let [eff, effTransit] = map (calcEff trx tk' minTsysPrime' zod') [za, zat]
>         return (eff, eff / effTransit)
>   where
>     za  = zenithAngle dt s
>     zat = zenithAngleAtTransit s
>     elevation = pi/2 - zat
>            
>     calcEff trx tk minTsysPrime' zod za = (minTsysPrime' / tsys') ^2
>       where
>         -- Equation 4 & 6
>         opticalDepth = zod / (cos . min 1.5 $ za)
>
>         -- Equation 7
>         tsys  = trx + 5.7 + tk * (1 - exp (-opticalDepth))
>
>         tsys' = exp opticalDepth * tsys

> receiverTemperature      :: DateTime -> Session -> Float
> receiverTemperature dt s =
>     case dropWhile (\(x, _) -> x < freq) freqBand of
>         (x : _) -> snd x
>         []      -> 60.0
>   where 
>         freq = fromIntegral . round . frequency $ s
>         freqBand =  [ (1.73, 10.0)
>                     , (3.95, 10.0)
>                     , (5.85, 5.0)
>                     , (10.0, 13.0)
>                     , (15.4, 14.0)
>                     , (26.5, 21.0)
>                     , (40.0, 35.0)
>                     , (50.0, 60.0)
>                      ]


> kineticTemperature      :: DateTime -> Session -> Scoring (Maybe Float)
> kineticTemperature dt s = do
>     w <- weather
>     return $ tatm w dt

> zenithOpticalDepth      :: DateTime -> Session -> Scoring (Maybe Float)
> zenithOpticalDepth dt s = do
>     w <- weather
>     return $ opacity w dt (frequency s)

> zenithAngle            :: DateTime -> Session -> Radians
> zenithAngle dt s = zenithAngleHA s $ lst - ra s
>   where
>     lst = hrs2rad . utc2lstHours $ dt

> zenithAngleAtTransit   :: Session -> Radians
> zenithAngleAtTransit s = zenithAngleHA s 0.0

> zenithAngleHA          :: Session -> Radians -> Radians
> zenithAngleHA Session { dec = dec' } ha =
>     -- Equation 5
>     acos $ sin gbtLat * sin dec' + cos gbtLat * cos dec' * cos ha

> atmosphericOpacity         :: ScoreFunc
> surfaceObservingEfficiency :: ScoreFunc
> trackingEfficiency         :: ScoreFunc

> atmosphericOpacity dt s = efficiency dt s >>= factor "atmosphericOpacity"

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

> trackingEfficiency dt s = factor "trackingEfficiency" . Just $
>     -- Equation 12
>     (1.0 + 4.0 * log 2.0 * (rmsTE' / theta') ^ 2) ^ (-2)
>   where
>     rmsTE' = rmsTE dt
>     theta' = theta . frequency $ s

3.2 Stringency

> stringency                 :: ScoreFunc

> stringency _ s = do
>     w <- weather
>     factor "stringency" $ totalStringency w (frequency s) elevation
>   where
>     elevation = pi/2 - zenithAngleAtTransit s


3.3 Pressure Feedback

> initBins        :: Int -> (Session -> Int) -> [Session] -> Array Int (Int, Int)
> initBins n f xs = runSTArray (initBins' n f xs)

> initBins' n f xs = do
>     arr <- newArray (0, n-1) (0, 0)
>     for xs $ \x -> do
>         let bin = f x
>         (t, c) <- readArray arr bin
>         writeArray arr bin $! (t + totalTime x, c + totalUsed x)
>     return arr
>   where
>     for []     f = return ()
>     for (x:xs) f = f x >> for xs f

> binsToFactors :: Array Int (Int, Int) -> Array Int Float
> binsToFactors = amap toFactor
>   where
>     toFactor (n, d) = 1.0 + asFactor n - asFactor d
>     asFactor i      = if i > 0 then log (fromIntegral i / 60.0) else 0.0

> frequencyPressure          :: Array Int Float -> (Session -> Int) -> ScoreFunc
> rightAscensionPressure     :: Array Int Float -> (Session -> Int) -> ScoreFunc

> frequencyPressure fs f _ a =
>     factor "frequencyPressure" . Just $ (fs ! f a) ** 0.5

> rightAscensionPressure fs f _ a =
>     factor "rightAscensionPressure" . Just $ (fs ! f a) ** 0.3

3.4 Performance Limits

> minObservingEff :: Session -> Float
> minObservingEff Session { frequency = freq } =
>     -- Equation 23
>     avgEff - 0.02 - 0.1*(1 - avgEff)
>   where
>     nu0 = 12.8
>     r = max 50 freq / nu0
>     -- Equation 22
>     avgEff = sum [x * cos (y*r) |
>                  (x, y) <- zip [0.74, 0.155, 0.12, -0.03, -0.01] [0..]]

> observingEfficiencyLimit  :: ScoreFunc
> hourAngleLimit            :: ScoreFunc
> zenithAngleLimit          :: ScoreFunc
> trackingErrorLimit        :: ScoreFunc
> atmosphericStabilityLimit :: ScoreFunc

> observingEfficiencyLimit dt s = factor "observingEfficiencyLimit" . Just $
>     if obsEff < minObsEff
>     -- Equation 24
>     then exp (-((obsEff - minObsEff) ^ 2) / (2.0 * sigma ^ 2))
>     else 1.0
>   where
>     sigma = 0.02
>     obsEff = 1.0  -- TBF
>     minObsEff = minObservingEff s

> hourAngleLimit dt s = do
>     effHA <- efficiencyHA dt s
>     boolean "hourAngleLimit" . fmap (\effHA' -> effHA' >= criterion) $ effHA
>   where
>     criterion = sqrt . (* 0.5) . minObservingEff $ s

> zenithAngleLimit dt s =
>    boolean "zenithAngleLimit" . Just $ zenithAngle dt s < deg2rad 85.0

> trackingErrorLimit dt s = do
>     w <- weather
>     boolean "trackingErrorLimit" $ do
>         wind' <- wind w dt
>         -- Equation 26
>         let fv = rmsTrackingError wind' / (theta . frequency $ s)
>         return $ fv <= maxErr
>   where
>     maxErr = 0.2 
>     -- Equation 11
>     rmsTrackingError w = sqrt (rmsTE dt ^ 2 + (abs w / 2.1) ^ 4)

> atmosphericStabilityLimit _ _ = factor "atmosphericStabilityLimit" . Just $ 1.0

3.5 Other factors

> projectCompletion, thesisProject, scienceGrade :: ScoreFunc

> projectCompletion _ s = let
>     weight = 1000.0
>     total = fromIntegral (timeTotal . project $ s)
>     left  = fromIntegral (timeLeft  . project $ s)
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

> -- receiver                      :: ScoreFunc

> data ScoringEnv = ScoringEnv {
>     envWeather :: Weather
>   }

> weather :: Scoring Weather
> weather = asks envWeather

> type Scoring = ReaderT ScoringEnv Identity

> runScoring     :: Weather -> Scoring t -> t
> runScoring w f = runIdentity . runReaderT f $ ScoringEnv w

> factor          :: String -> Maybe Score -> Scoring Factors
> factor name val = return [(name, val)]

> type ScoreFunc = DateTime -> Session -> Scoring Factors 

> instance Show (a -> b) where
>     show _ = "ScoreFunc"

> concatMapM   :: (Functor m, Monad m) => (a -> m [b]) -> [a] -> m [b]
> concatMapM f = fmap concat . mapM f

> score         :: [ScoreFunc] -> ScoreFunc
> score fs dt a = concatMapM (\f -> f dt a) fs

> ignore       :: [String] -> Factors -> Factors
> ignore names = filter $ \(n, _) -> not (n `elem` names)
  
> eval :: Factors -> Score
> eval = foldr step 1.0
>   where
>     step (_, Nothing) s = 0.0
>     step (_, Just f)  s
>         | s < 1.0e-6    = 0.0
>         | otherwise     = s * f

> genScore   :: [Session] -> ScoreFunc
> genScore _ = score [
>     atmosphericOpacity
>   , atmosphericStabilityLimit
>   , hourAngleLimit
>   , observingEfficiencyLimit
>   , projectCompletion
>   , scienceGrade
>   , stringency
>   , surfaceObservingEfficiency
>   , thesisProject
>   , trackingEfficiency
>   , trackingErrorLimit
>   , zenithAngleLimit
>   ]

-- > receiver dt Session { receivers = rcvrs } = do
-- >     scheduled <- asks $ getReceivers dt . receiverSchedule
-- >     boolean "receiver" $ all (`elem` scheduled) rcvrs

> boolean name = factor name . fmap (\b -> if b then 1.0 else 0.0)
