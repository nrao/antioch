> module Antioch.Score where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Utilities
> import Antioch.Weather
> import Control.Monad.Identity
> import Control.Monad.Reader
> import Data.Array
> import Data.Array.IArray (amap)
> import Data.Array.ST
> import Data.List
> import Data.Maybe (fromJust)

> type Factor   = (String, Score)
> type Factors  = [Factor]

Ranking System from Memo 5.2, Section 3

3.1 Observing Efficiency

> efficiency, efficiencyHA :: DateTime -> Session -> Scoring Float
> efficiency   dt = fmap fst . calcEfficiency dt
> efficiencyHA dt = fmap snd . calcEfficiency dt

> calcEfficiency      :: DateTime -> Session -> Scoring (Float, Float)
> calcEfficiency dt s = do
>     trx <- receiverTemperature dt s
>     tk  <- kineticTemperature dt s
>     minTsys' <- minTsys
>     zod <- zenithOpticalDepth dt s
>     let [eff, effTransit] = map (calcEff trx tk minTsys' zod) [za, zat]
>     return $ (eff, eff / effTransit)
>   where
>     za  = zenithAngle dt s
>     zat = zenithAngleAtTransit s
>            
>     minTsys = return za
>            
>     calcEff trx tk minTsys' zod za = (minTsys' / tsys') ^2
>       where
>         -- Equation 4 & 6
>         opticalDepth = zod / (cos . min 1.5 $ za)
>         -- Equation 7
>         tsys  = trx + 5.7 + tk * (1 - exp (-opticalDepth))
>         tsys' = (exp opticalDepth) * tsys

> receiverTemperature      :: DateTime -> Session -> Scoring Float
> receiverTemperature dt s = return 1.0

> kineticTemperature      :: DateTime -> Session -> Scoring Float
> kineticTemperature dt s = return 1.0

> zenithOpticalDepth      :: DateTime -> Session -> Scoring Float
> zenithOpticalDepth dt s = do
>     w <- weather
>     return . fromJust $ opacity w dt (frequency s)

> zenithAngle      :: DateTime -> Session -> Radians
> zenithAngle dt s = zenithAngleHA s $ lst - ra s
>   where
>     lst = hrs2rad . utc2lstHours $ dt

> zenithAngleAtTransit   :: Session -> Radians
> zenithAngleAtTransit s = zenithAngleHA s 0.0

> zenithAngleHA                           :: Session -> Radians -> Radians
> zenithAngleHA Session { dec = dec' } ha =
>     -- Equation 5
>     acos $ sin gbtLat * sin dec' + cos gbtLat * cos dec' * cos ha

> atmosphericOpacity         :: ScoreFunc
> surfaceObservingEfficiency :: ScoreFunc
> trackingEfficiency         :: ScoreFunc

> atmosphericOpacity dt s = efficiency dt s >>= factor "atmosphericOpacity"

> surfaceObservingEfficiency dt s = factor "surfaceObservingEfficiency" $
>     if isDayTime dt
>     then
>         -- Equation 9
>         exp (-(k * (frequency s) ^ 2 * epsilonFactor))
>         -- Equation 10
>         -- exp (-((fromIntegral . round . frequency $ s)/69.2) ^ 2)
>     else
>         1.0
>     where
>         c = 299792485.0
>         epsilonDay   = 0.46
>         epsilonNight = 0.39
>         epsilonFactor = epsilonDay ^ 2 - epsilonNight ^ 2
>         k = 32.0 * pi ^ 2 * 1e12 / (c ^ 2)

> theta :: Float -> Float
> theta f = 740.0 / f

> rmsTE :: DateTime -> Float
> rmsTE dt = if isDayTime dt then sigmaDay else sigmaNight
>     where
>        sigmaDay = 3.3
>        sigmaNight = 2.8

> trackingEfficiency dt s = factor "trackingEfficiency" $
>    -- Equation 12
>    (1.0 + 4.0 * log 2.0 * (rmsTE' / theta') ^ 2) ^ (-2)
>    where
>        rmsTE' = rmsTE dt
>        theta' = theta . frequency $ s

3.2 Stringency

> -- stringency                    :: ScoreFunc

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
>     toFactor (n, d) = 1.0 + (asFactor n) - (asFactor d)
>     asFactor i      = if i > 0 then log (fromIntegral i / 60.0) else 0.0

> frequencyPressure          :: Array Int Float -> (Session -> Int) -> ScoreFunc
> rightAscensionPressure     :: Array Int Float -> (Session -> Int) -> ScoreFunc

> frequencyPressure fs f _ a =
>     factor "frequencyPressure" $ (fs ! f a) ** 0.5

> rightAscensionPressure fs f _ a =
>     factor "rightAscensionPressure" $ (fs ! f a) ** 0.3

3.4 Performance Limits

> minObservingEff :: Session -> Float
> minObservingEff Session { frequency = freq } =
>    -- Equation 23
>    avgEff - 0.02 - 0.1*(1 - avgEff)
>    where nu0 = 12.8
>          r = (max 50 freq) / nu0
>          -- Equation 22
>          avgEff = sum [x * cos (y*r) |
>                        (x, y) <- zip [0.74, 0.155, 0.12, -0.03, -0.01] [0..]]

> observingEfficiencyLimit  :: ScoreFunc
> hourAngleLimit            :: ScoreFunc
> zenithAngleLimit          :: ScoreFunc
> trackingErrorLimit        :: ScoreFunc
> atmosphericStabilityLimit :: ScoreFunc

> observingEfficiencyLimit dt s = factor "observingEfficiencyLimit" $
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
>     boolean "hourAngleLimit" $ effHA >= criterion
>   where
>     criterion = sqrt . (* 0.5) . minObservingEff $ s

> zenithAngleLimit dt s =
>    boolean "zenithAngleLimit" $ zenithAngle dt s < deg2rad 85.0

> trackingErrorLimit dt s = do
>     w <- weather
>     let wind' = fromJust $ wind w dt
>     -- Equation 26
>     let fv = rmsTrackingError wind' / (theta . frequency $ s)
>     boolean "trackingErrorLimit" $ fv <= maxErr
>   where
>     maxErr = 0.2 
>     -- Equation 11
>     rmsTrackingError w = sqrt ((rmsTE dt) ^ 2 + (abs w / 2.1) ^ 4)

> atmosphericStabilityLimit _ _ = factor "atmosphericStabilityLimit" 1.0

3.5 Other factors

> projectCompletion, thesisProject, scienceGrade :: ScoreFunc

> projectCompletion _ s = let weight = 1000.0
>                             total = fromIntegral (timeTotal . project $ s)
>                             left  = fromIntegral (timeLeft  . project $ s)
>                             percent = if total <= 0.0
>                                       then 0.0
>                                       else 100.0*(total - left)/total
>                         in factor "projectCompletion" $
>                            if percent <= 0.0
>                            then 1.0
>                            else 1.0 + percent/weight

> thesisProject _ s = factor "thesisProject" $
>                     if (thesis . project $ s) then 1.05 else 1.0

> scienceGrade _ s = factor "scienceGrade" $
>                    case (grade s) of
>                      GradeA -> 1.0
>                      GradeB -> 0.9
>                      GradeC -> 0.1

> -- receiver                      :: ScoreFunc

> data ScoringEnv = ScoringEnv {
>     envWeather :: Weather
>   }

> weather :: Scoring Weather
> weather = asks envWeather

> type Scoring = ReaderT ScoringEnv Identity

> runScoring     :: Weather -> Scoring t -> t
> runScoring w f = runIdentity . runReaderT f $ ScoringEnv w

> factor          :: String -> Score -> Scoring Factors
> factor name val = return [(name, val)]

> type ScoreFunc = DateTime -> Session -> Scoring Factors 

> instance Show (a -> b) where
>     show _ = "ScoreFunc"


> concatMapM      :: (Functor m, Monad m) => (a -> m [b]) -> [a] -> m [b]
> concatMapM f xs = fmap concat . mapM f $ xs

> score         :: [ScoreFunc] -> ScoreFunc
> score fs dt a = concatMapM (\f -> f dt a) fs

> ignore       :: [String] -> Factors -> Factors
> ignore names = filter $ \(n, _) -> not (n `elem` names)
  
> eval :: Factors -> Score
> eval = foldr step 1.0
>   where
>     step (_, f) s
>         | s < 1.0e-6 = 0.0
>         | otherwise  = s * f

-- > receiver dt Session { receivers = rcvrs } = do
-- >     scheduled <- asks $ getReceivers dt . receiverSchedule
-- >     boolean "receiver" $ all (`elem` scheduled) rcvrs

> boolean name True  = factor name 1.0
> boolean name False = factor name 0.0
