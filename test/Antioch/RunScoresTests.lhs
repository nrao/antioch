
> module Antioch.RunScoresTests where

> import Antioch.DateTime
> import Antioch.Score
> import Antioch.Types
> import Antioch.Weather      (Weather, getWeatherTest, minTSysPrime)
> import Antioch.Utilities
> import Antioch.PProjects
> import Antioch.Receiver
> import Antioch.ReceiverTemperatures
> import Antioch.RunScores

> import Test.HUnit
> import Data.List
> import Control.Monad.Trans  (lift, liftIO)

> tests = TestList [ 
>     test_runScorePeriods
>   , test_runScoreSession
>     ]

> test_runScorePeriods = TestCase $ do
>   scores <- runScorePeriods pids pTestProjects True
>   assertEqual "test_runScorePeriods_1" exp scores
>   scores <- runScorePeriods [450] projs True
>   assertEqual "test_runScorePeriods_2" exp2 scores
>     where
>   -- try the periods in PProjects wich mostly score zero
>   pids = [100, 101, 200, 201] -- from PProjects.lhs
>   scores' = [0.0, 0.0, 2.126854, 0.0]::[Score]
>   exp = zip pids scores'
>   -- now create a period that should score non-zero
>   s = defaultSession { receivers = [[Rcvr1_2]] 
>                      , frequency = 1.1
>                      , dec = 1.5 -- always up
>                      }
>   p = defaultPeriod { session = s
>                     , startTime = fromGregorian 2006 2 1 0 30 0
>                     , duration = 60
>                     , peId = 450
>                     }
>   s' = makeSession s [] [p]
>   proj = defaultProject
>   projs = [makeProject proj 100 100 [s']]
>   exp2 = [(450, 0.37846184)]


> test_runScoreSession = TestCase $ do
>   score <- runScoreSession sessId startDt 60 projs True
>   assertEqual "test_runScoreSession_1" exp score
>   --score <- runScoreSession id dt dur pTestProjects True
>   --assertEqual "test_runScoreSession_1" exp score
>     where
>   --id = 13
>   --dt = fromGregorian 2006 9 23 12 0 0
>   --dur = 30
>   -- setup session like ScoreTests.test_scorePeriod
>   scores = [1.6321898,1.6325561,1.6327252,1.632888]
>   exp = ((sum $ tail scores) / 4.0)::Score 
>   startDt = fromGregorian 2006 2 1 0 30 0
>   s = head pSessions
>   sessId = sId s
>   p = defaultPeriod { session = s
>                       , startTime = startDt
>                       , duration = 60
>                       , pForecast = startDt
>                       }
>   s' = makeSession s [] [p]
>   proj = defaultProject
>   projs = [makeProject proj 0 0 pSessions]


> pSessions = zipWith6 genPSess tots useds ras bands grades ids
>   where tots   = [12*60, 18*60, 10*60, 20*60]
>         useds  = [ 2*60,  8*60,  5*60, 12*60]
>         ras    = [  5.4,  10.1,   4.9,  18.1]
>         bands  = [    L,     C,     X,     L]
>         grades = [4.0, 4.0, 4.0, 4.0]
>         ids    = [340, 341, 342, 343]
>         genPSess t u ra b g i = defaultSession {
>             sId = i
>           , sAllottedS = t
>           , sAllottedT = t
>           , periods = [defaultPeriod {duration = u
>                                     , pState = Scheduled
>                                     , pDuration = u}]
>           , ra = hrs2rad ra
>           , band = b
>           , grade = g
>           , receivers = [[Rcvr1_2]] 
>         }
