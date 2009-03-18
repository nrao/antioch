> module Antioch.SLAlibTests where

> import Antioch.DateTime
> import Antioch.SLALib
> import Antioch.Utilities
> import Test.HUnit
> import Antioch.Types
> import Antioch.DateTime
> import Antioch.Score

> tests = TestList [test_gmst]

BETA: matches 3rd party python library slalib.sla_gmst
import slalib
slalib.sla_gmst(54907.0)

> test_gmst = TestCase $ do
>   let mjd = 54907.0
>   let result = gmst mjd
>   assertEqual "test_utc2lst" 3.0490882973440634 result

