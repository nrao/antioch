> module Antioch.Generators where

> import Antioch.Types
> import Data.Char
> import System.Random (randomR)
> import Test.QuickCheck hiding (frequency)
> import qualified Test.QuickCheck as T

> instance Arbitrary Project where
>     arbitrary = genProject

> instance Arbitrary Session where
>     arbitrary = genSession

> instance Arbitrary Period where
>     arbitrary = genPeriod


choose LST range and declination
s - single sources or few sources in one area of the sky
    g - galactic plane (some near GC)
    e - extra galactic
a - all sky or a large region of the sky

> deg2rad :: Float -> Float
> deg2rad x = x * 3.14 / 180

> rad2deg :: Float -> Float
> rad2deg x = x * 180 / 3.14

> rad2hr :: Float -> Float
> rad2hr x = 12 * x / 3.14

> skyType = elements "geegeegeeaaa"  -- sssa, s <- gee

> genRaDec 'g' = T.frequency [(20, galacticCenter), (80, galactic)]
>   where
>     galacticCenter = do
>         dec <- choose (-27.0, -29.0)
>         return (18.0, dec)
>     galactic = do
>         longitude <- choose (0.0, 250.0)
>         let (rar, decr) = sla_galeq (deg2rad longitude) 0.0
>         return (rad2hr rar, rad2deg decr)
> genRaDec _   = do
>     ra  <- randomR (0.0, 23.999)
>     dec <- fmap (rad2deg . asin) . choose $ (sin . deg2rad $ -35.0, sin . deg2rad $ 90.0)
>     return (ra, dec)

-- > genAllocation :: Gen Allocation
-- > genAllocation = do
-- >     s <- semester
-- >     b <- band s
-- >     f <- freq b
-- >     (ra, dec) <- genRaDec
-- >     totalHours <- choose (2, 30)
-- >     minDuration <- choose (2, 4)
-- >     maxDuration <- choose (6, 8)
-- >     return $ defaultAllocation {
-- >                  semester = s
-- >                , band     = b
-- >                , freq     = f
-- >                , rightAscension = ra
-- >                , declination    = dec
-- >                }

> genProject :: Gen Project
> genProject = return $ defaultProject

> genSession :: Gen Session
> genSession = return $ defaultSession

> genPeriod :: Gen Period
> genPeriod = return $ defaultPeriod

-- > type Semester = Int
  
-- > semester :: Gen Semester
-- > semester = fmap (read . str) . elements $ "0111122223333"

-- > str :: a -> [a]
-- > str = (: [])

-- > data Band = L | S | C | X | U | K | A | Q
-- >           deriving (Read, Show)

choose observing band distribution
average up to trimester 7C

Band  Hours  Percent  Year  alloc
L     700    46.0%    27.6  26
S     130     8.6%     5.2   4
C     110     7.2%     4.3   5
X     120     7.9%     4.7   4
U      90     5.9%     3.5   3
K     230    15.1%     9.1   6
A      60     3.9%     2.3   6
Q      80     5.3%     3.2   6

-- > band     :: Int -> Gen Band
-- > band sem = fmap (read . str) . elements $ bands !! sem
-- >   where
-- >     bands = [ "KKQQAAXUCCSLLLLLLLLL"  -- 0 => backup
-- >             , "KKKQQQAXUCCSSLLLLLLL"  -- 1
-- >             , "KQQAXUCSLLLLLLLLLLLL"  -- 2
-- >             , "KKQQAAAXXUCCSLLLLLLL"  -- 3
-- >             ]

-- > freq   :: Band -> Gen Float
-- > -- Assume we are observing the water line 40% of the time.
-- > freq K = frequency [(40, return 22.2), (60, choose (18.0, 26.0))]
-- > freq L = return 2.0
-- > freq S = choose ( 2.0,  3.95)
-- > freq C = choose ( 3.95, 5.85)
-- > freq X = choose ( 8.0, 10.0)
-- > freq U = choose (12.0, 15.4)
-- > freq A = choose (26.0, 40.0)
-- > freq Q = choose (40.0, 50.0)
