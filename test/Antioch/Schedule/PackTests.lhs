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
>   , test_PackWorker1
>   ]

> test_NumSteps = TestCase . assertEqual "test_NumSteps" 192 . numSteps $ 48 * 60

> test_Unwind1 = TestCase . assertEqual "test_Unwind1" xs . unwind $ ys
>   where
>     xs = [Candidate 1 2 1.0, Candidate 2 2 1.0]
>     ys = [Just (Candidate 2 2 1.0), Nothing, Just (Candidate 1 2 1.0), Nothing]

> test_Unwind2 = TestCase . assertEqual "test_Unwind2" xs . unwind $ ys
>   where
>     xs = [Candidate 1 1 1.0, Candidate 2 2 1.0, Candidate 3 3 1.0]
>     ys = [Just (Candidate 3 3 1.0), Nothing, Nothing, Just (Candidate 2 2 1.0), Nothing, Just (Candidate 1 1 1.0)]

> test_Candidates1 = TestCase . assertEqual "test_Candidates1" xs . candidates $ ys
>   where
>     xs = [Nothing, Nothing, Just (Candidate 1 3 3.0), Just (Candidate 1 4 4.0)]
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
>     xs = [Just (Candidate 1 4 4.0), Just (Candidate 1 3 3.0), Nothing, Nothing, Nothing]
>     ys = replicate 4 Nothing
>     zs = [Nothing]
>     ws = map step [Item 1 2 4 (replicate 6 1.0) []]

> test_PackWorker1 = TestCase . assertEqual "test_PackWorker1" xs . packWorker ys $ ws
>   where
>     xs = [Candidate 1 4 4.0]
>     ys = replicate 4 Nothing
>     ws = [Item 1 2 4 (replicate 6 1.0) []]
