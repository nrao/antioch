> module Antioch.Schedule.PackTests where

> import Antioch.Schedule.Pack
> import Test.HUnit

> tests = TestList [
>     test_NumSteps
>   , test_Unwind1
>   , test_Unwind2
>   , test_Candidates1
>   , test_Candidates2
>   , test_Best
>   , test_Madd1
>   , test_Madd2
>   , test_PackWorker'1
>   , test_PackWorker'3
>   , test_PackWorker1
>   , test_PackWorker2
>   , test_PackWorker3
>   , test_PackWorker4
>   ]

> test_NumSteps = TestCase . assertEqual "test_NumSteps" 192 . numSteps $ 48 * 60

> test_Unwind1 = TestCase . assertEqual "test_Unwind1" xs . unwind $ ys
>   where
>     xs = [Candidate 1 2 1.0, Candidate 2 2 1.0]
>     ys = [Just (Candidate 2 2 2.0), Nothing, Just (Candidate 1 2 1.0), Nothing]

> test_Unwind2 = TestCase . assertEqual "test_Unwind2" xs . unwind $ ys
>   where
>     xs = [Candidate 1 1 1.0, Candidate 2 2 1.0, Candidate 3 3 1.0]
>     ys = [Just (Candidate 3 3 3.0), Nothing, Nothing, Just (Candidate 2 2 2.0), Nothing, Just (Candidate 1 1 1.0)]

> test_Candidates1 = TestCase . assertEqual "test_Candidates1" xs . candidates $ ys
>   where
>     xs = [Nothing, Just (Candidate 1 2 2.0), Just (Candidate 1 3 3.0), Just (Candidate 1 4 4.0)]
>     ys = Item 1 2 4 [] (replicate 6 1.0)

> test_Candidates2 = TestCase . assertEqual "test_Candidates2" xs . candidates $ ys
>   where
>     xs = []
>     ys = Item 1 2 4 [] (0.0 : replicate 5 1.0)

> test_Best = TestCase . assertEqual "test_Best" xs . best $ ys
>   where
>     xs = Just (Candidate 1 4 4.0)
>     ys = [Nothing, Nothing, Just (Candidate 1 3 3.0), Just (Candidate 1 4 4.0)]

> test_Madd1 = TestCase . assertEqual "test_Madd1" xs . best . zipWith madd ys $ zs
>   where
>     xs = Just (Candidate 1 4 4.0)
>     ys = [Nothing, Nothing, Just (Candidate 1 3 3.0), Just (Candidate 1 4 4.0)]
>     zs = replicate 4 Nothing

> test_Madd2 = TestCase . assertEqual "test_Madd2" xs . best . zipWith madd ys $ zs
>   where
>     xs = Nothing
>     ys = [Nothing, Nothing, Just (Candidate 1 3 3.0), Just (Candidate 1 4 4.0)]
>     zs = replicate 2 Nothing

> test_PackWorker'1 = TestCase . assertEqual "test_PackWorker'1" xs . packWorker' ys zs $ ws
>   where
>     xs = [Just (Candidate 1 4 4.0), Just (Candidate 1 3 3.0), Just (Candidate 1 2 2.0), Nothing, Nothing]
>     ys = replicate 4 Nothing
>     zs = [Nothing]
>     ws = map step [Item 1 2 4 (replicate 6 1.0) []]

> test_PackWorker'3 = TestCase . assertEqual "test_PackWorker'3" xs . packWorker' ys zs $ ws
>   where
>     xs = [Just (Candidate 2 1 1.0), Just (Candidate 1 2 3.1), Nothing, Just (Candidate 3 1 1.1), Nothing]
>     ys = [Just (Candidate 3 1 1.1), Nothing, Nothing, Just (Candidate 2 1 1.0)]
>     zs = [Nothing]
>     ws = map step [Item 1 2 4 (replicate 6 1.0) []]

> test_PackWorker1 = TestCase . assertEqual "test_PackWorker1" xs . packWorker ys $ ws
>   where
>     xs = [Candidate 1 4 4.0]
>     ys = replicate 4 Nothing
>     ws = [Item 1 2 4 (replicate 6 1.0) []]

> test_PackWorker2 = TestCase . assertEqual "test_PackWorker2" xs . packWorker ys $ ws
>   where
>     xs = [Candidate 1 3 3.0, Candidate 2 1 1.0]
>     ys = replicate 3 Nothing ++ [Just (Candidate 2 1 4.0)]
>     ws = [Item 1 2 4 (replicate 6 1.0) []]

> test_PackWorker3 = TestCase . assertEqual "test_PackWorker3" xs . packWorker ys $ ws
>   where
>     xs = [Candidate 3 1 1.1, Candidate 1 2 2.0, Candidate 2 1 1.0]
>     ys = [Just (Candidate 3 1 1.1), Nothing, Nothing, Just (Candidate 2 1 4.1)]
>     ws = [Item 1 2 4 (replicate 6 1.0) []]

This next test `test_PackWorker4` highlights a few different
attributes of the packing algorithm:

  1. The pre-scheduled/fixed sessions do not get lost or overwritten
     in spite of all the open sessions performing better in those time
     slots.

  2. The gap between the two fixed sessions stays empty due to
     min-duration constraints.

  3. We see session D compete with and overtake session C at the end
     of the day.

  4. A scheduled session will not include any zero periods.

> test_PackWorker4 =
>     TestCase . assertEqual "test_PackWorker4" result . packWorker fixed $ open
>   where
>     result = [ Candidate "A"  2 2.0
>              , Candidate "F1" 2 0.0
>              , Candidate "F2" 2 0.0
>              , Candidate "C"  2 2.0
>              , Candidate "D"  2 2.0
>              ]
>     fixed  = [ Nothing                      --  0
>              , Nothing                      --  1
>              , Just (Candidate "F1" 1 1.0)  --  2
>              , Just (Candidate "F1" 2 2.0)  --  3
>              , Nothing                      --  4
>              , Just (Candidate "F2" 1 1.0)  --  5
>              , Just (Candidate "F2" 2 2.0)  --  6
>              , Nothing                      --  7
>              , Nothing                      --  8
>              , Nothing                      --  9
>              , Nothing                      -- 10
>              ]
>     --                       0    1    2    3    4    5    6    7    8    9    10
>     open   = [ Item "A" 2 8 [1.0, 1.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 0.0, 0.0, 0.0] []
>              , Item "B" 2 8 [0.0, 0.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 0.0, 0.0, 0.0] []
>              , Item "C" 2 8 [0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.5, 0.5] []
>              , Item "D" 2 8 [0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0] []
>              ]
