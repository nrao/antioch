> module Antioch.Score where

> import Antioch.Types
> import Control.Monad.Identity
> import Control.Monad.Reader
> import Data.List
> import Data.Time.Clock

> type Factor   = (String, Score)
> type Factors  = [Factor]

Ranking System from Memo 5.2, Section 3

3.1 Observing Efficiency

> zenithOpticalDepth :: DateTime -> Session -> Scoring Float
> zenithOpticalDepth dt s = do
>     w <- weather
>     return $ opacity w dt (frequency s)

3.2 Stringency

> -- stringency                    :: ScoreFunc

3.3 Pressure Feedback

> -- frequencyPressure             :: ScoreFunc
> -- rightAscensionPressure        :: ScoreFunc

3.4 Performance Limits

> -- observingEfficiencyLimit      :: ScoreFunc
> -- hourAngleLimit                :: ScoreFunc
> -- zenithAngleLimit              :: ScoreFunc
> -- trackingErrorLimit            :: ScoreFunc

> atmosphericStabilityLimit     :: ScoreFunc
> atmosphericStabilityLimit _ _ = factor "atmosphericStabilityLimit" 1.0

3.5 Other factors

> projectCompletion     :: ScoreFunc
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

> thesisProject     :: ScoreFunc
> thesisProject _ s = factor "thesisProject" $
>                     if (thesis . project $ s) then 1.05 else 1.0

> scienceGrade      :: ScoreFunc
> scienceGrade _ s = factor "scienceGrade" $
>                    case (grade s) of
>                      GradeA -> 1.0
>                      GradeB -> 0.9
>                      GradeC -> 0.1

> -- receiver                      :: ScoreFunc


> data Weather = Weather {
>     opacity :: DateTime -> Float -> Float
>   , tsys    :: DateTime -> Float -> Float
>   , wind    :: DateTime -> Float  -- m/s
>   }

> getWeather :: IO Weather
> getWeather = do
>     return Weather {
>         opacity = \_ _ -> 0.2
>       , tsys    = \_ _ -> 255.0
>       , wind    = const   2.0
>       }

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

-- > initBins        :: Int -> (Session -> Int) -> [Session] -> Array Int (Int, Int)
-- > initBins n f as = runSTArray (initBins' n f allocs)

-- > initBins' n f as = do
-- >     arr <- newArray (0, n-1) (0, 0)
-- >     for as $ \a -> do
-- >         let bin = f a
-- >         (t, c) <- readArray arr bin
-- >         -- total should be max of total and completed?
-- >         writeArray arr bin $! (t + totalTime a, c + timeCompleted a)
-- >     return arr
-- >   where
-- >     for []     f = return ()
-- >     for (x:xs) f = f x >> for xs f

-- > binsToFactors :: Array Int (Int, Int) -> Array Int [Float]
-- > binsToFactors = amap toFactor
-- >   where
-- >     toFactor (n, d) = 1.0 + (asFactor n) - (asFactor d)
-- >     asFactor i      = if i > 0 then log (fromIntegral i / 60.0) else 0.0

-- > frequencyPressure          :: Array Int [Float] -> (Session -> Int) -> ScoreFunc
-- > frequencyPressure fs f _ a =
-- >     factor "frequencyPressure" $ (fs ! f a) `pow` 0.5

-- > rightAscensionPressure          :: Array Int [Float] -> (Session -> Int) -> ScoreFunc
-- > rightAscensionPressure fs f _ a =
-- >     factor "rightAscensionPressure" $ (fs ! f a) `pow` 0.3
