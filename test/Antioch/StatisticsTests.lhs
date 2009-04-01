> module Antioch.StatisticsTests where

> import Antioch.DateTime
> import Antioch.Generators (genSessions, genPeriods)
> import Antioch.Statistics
> import Antioch.Types
> import Antioch.Weather
> import Antioch.Utilities
> import Antioch.Generators (generateTestData)
> import Antioch.PProjects
> import Data.List
> import Test.HUnit
> import System.Random

> tests = TestList [
>     test_breakdownSimulationTimes
>   , test_count
>   , test_findScheduleGaps
>   , test_getOriginalSchedule'
>   , test_sessionDecFreq
>   , test_periodDecFreq
>   , test_sessionDecRA
>   , test_periodDecRA
>   , test_sessionRA
>   , test_periodRA
>   , test_sessionDec
>   , test_periodDec
>   , test_sessionFreq
>   , test_periodFreq
>   , test_sessionMinDuration
>   , test_sessionTP
>   , test_sessionTP2
>   , test_sessionTPQtrs
>   , test_freqTime
>   , test_periodBand
>   , test_periodDuration
>   , test_periodEfficiencyByBand
>   , test_decVsElevation
>   , test_efficiencyVsFrequency
>   , test_historicalFreq
>   , test_historicalDec
>   , test_historicalRA
>   , test_historicalTime
>   , test_historicalTime'
>   , test_historicalLST
>   , test_satisfactionRatio
>     ]

> test_count = TestCase $ do
>     assertEqual "StatisticsTests_test_count1" exp1 cnt1
>     assertEqual "StatisticsTests_test_count2" exp2 cnt2
>  where
>    cnt1 = count minDuration [0..5] [s1]
>    s1 = defaultSession {minDuration = 3}
>    exp1 = [(0,0),(1,0),(2,0),(3,1),(4,0),(5,0)]
>    s2 = defaultSession {minDuration = 1}
>    cnt2 = count minDuration [0..5] [s1,s2,s2,s1,s1]
>    exp2 = [(0,0),(1,2),(2,0),(3,3),(4,0),(5,0)]
> 
> test_sessionDecFreq = TestCase $ do
>     assertEqual "test_sessionDecFreq" expected (sessionDecFreq sessions)
>   where
>     (sessions, _) = generateTestData 100
>     expected = [(18.03244,0.47623572),(2.0,4.252978e-2),(2.0,0.53812695),(13.329185,-0.50425416),(22.2,1.0874313),(5.8117433,-0.4260103),(34.831585,0.39609724),(2.0,9.471178e-5),(9.647265,1.0906446),(27.0602,0.49002618),(34.159927,0.15560223),(32.254818,0.2770956),(12.633172,-0.19889463),(2.0,-0.5555474),(27.28363,0.73994595),(13.313569,-0.15411195),(8.989425,7.8556634e-2),(12.533225,1.0969883),(2.0,-0.32472938),(2.0,1.0468142),(2.0,4.9841534e-2),(2.1804707,1.1699796),(13.148822,0.5070484),(2.0,1.0779898),(2.0,0.9367015),(46.876442,-5.2755535e-2),(45.272423,0.77219695),(2.0,-0.5833577),(2.0,0.7394532),(8.363153,-0.106884986),(3.9028223,-0.28538352),(38.153526,0.47596583),(19.351812,0.37868357),(2.0,4.339968e-2),(2.0,-0.43405452),(24.259632,0.56981087),(2.0,0.5513684),(5.643878,0.8354136),(33.07285,0.16430223),(2.0,-0.5656911),(8.369127,0.44070128),(2.0,-8.372329e-2),(22.2,0.15594037),(9.3495655,-0.47388044),(22.2,0.22856916),(2.0,0.118912205),(48.359962,1.1704198),(2.0,0.6355781),(4.557304,-0.4877464),(34.265385,-8.631324e-2),(5.726377,2.4290108e-3),(26.562767,6.967458e-2),(22.71207,0.19237168),(2.0,-0.41489246),(2.0,7.767485e-3),(13.173273,0.8703967),(2.0,1.2960639),(36.141354,1.0952467),(2.0,1.0914029),(8.283322,-0.2660734),(30.796986,1.0945269),(46.121742,0.8139722),(2.0,0.37650114),(2.0,-0.22396849),(2.0,0.91400814),(3.8919954,0.44876957),(2.0,0.84143883),(12.842893,0.7303613),(2.0,1.040249),(8.347389,0.977887),(43.295284,1.0809405),(2.0,1.5956035e-2),(14.645904,-0.49930158),(9.703919,-0.18309043),(22.2,-0.24313517),(2.0,0.4702638),(2.0,0.68266124),(46.49578,0.42338145),(22.2,0.21835586),(42.9339,-9.22699e-3),(9.160325,-0.32752123),(2.0,-0.414729),(31.575743,0.27493513),(41.64395,2.7394248e-2),(2.0,-0.506914),(2.0,0.40044433),(26.966955,-1.9969795e-2),(2.0,1.164524),(45.750862,0.109786525),(2.0,1.0115127e-2),(35.298923,-0.47125766),(27.078657,-0.4771885),(2.0,0.32714063),(35.33423,0.65024656),(2.0,0.29171142),(26.398733,0.59418243),(2.0,0.93078256),(4.442998,0.88392806),(8.728631,-0.41651148),(2.0,-0.2486362)]

> test_periodDecFreq = TestCase $ do
>     assertEqual "test_periodDecFreq" expected (periodDecFreq periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [(26.590275,0.88389367),(4.2308164,-0.27464512),(8.545437,-0.5072232),(29.43433,0.8051078),(2.0,-0.12282421),(22.2,0.5533878),(2.0,-0.49110794),(9.564482,1.2163651),(3.6233535,0.5001144),(42.254066,1.0535471),(3.366477,1.0495619),(2.0,0.47537318),(2.0,7.299652e-2),(2.0,0.53972495),(13.078968,0.83740026),(5.0934834,9.519441e-2),(45.92087,0.3817392),(13.252991,0.34255493),(2.0,-0.1744196),(2.0,0.8163508),(2.0,0.1501175),(22.2,0.69803804),(2.0,0.5510235),(38.70217,0.9723723),(22.2,0.37261942),(3.2661185,-0.24262673),(37.20534,0.883044),(2.0,0.57971),(4.90036,-0.5057993),(44.334812,-4.7268827e-2),(29.68108,1.0694193),(2.0,0.45509547),(2.0,0.13170244),(2.0,0.4473911),(5.34516,-0.5354764),(29.017683,-0.18087214),(2.0,-0.31181464),(32.833515,0.101155855),(2.0,-0.18615025),(2.0,0.42608637),(40.167473,0.615602),(37.422455,0.20544977),(3.8554935,0.18857574),(5.75789,-0.47331607),(36.23306,0.36253095),(2.0,0.6454446),(12.526739,1.0306063),(42.382786,-3.0572036e-2),(37.664043,0.79800963),(2.0,-0.48526937),(2.510288,-3.294235e-2),(2.0,0.31871307),(2.0,0.5450085),(2.0,1.0647041),(2.0,-4.9259704e-2),(22.2,-6.863196e-2),(9.309816,1.6191654e-2),(2.0,1.1519129),(4.288614,0.5075307),(12.81987,-0.32560778),(2.0,-0.50103384),(2.0,0.99980825),(39.43843,0.64607185),(2.0,-0.42784485),(3.8985143,0.36737704),(22.2,0.41769913),(2.0,1.2501698),(2.553279,0.60944664),(2.0,-0.43018067),(9.334478,0.46265897),(2.0,1.0972357),(2.0,-0.52266407),(2.0,0.6035951),(2.1527443,6.256886e-2),(2.0,0.27118987),(2.0,0.8735829),(9.359476,0.20241466),(5.271047,0.7365372),(2.0,0.15346724),(40.1292,0.9279352),(2.0,1.093586),(47.41227,0.21715273),(20.625532,-0.59806275),(2.0,0.25072753),(2.0,0.9953381),(40.872032,0.9134297),(3.8807325,-0.58189934),(40.950226,-0.3025735),(33.46852,7.7850685e-2),(2.0,1.0481862),(2.0,7.979844e-2),(2.3462698,0.32950327),(2.0,0.38801876),(2.0,0.1017484),(2.0,-2.6071698e-3),(22.2,1.0454168),(2.0,0.5680049),(18.224922,-0.26702267),(22.2,0.10543607),(2.0,-0.48442215)]

> test_sessionDecRA = TestCase $ do
>     assertEqual "test_sessionDecRA" expected (sessionDecRA sessions)
>   where
>     (sessions, _) = generateTestData 100
>     expected = [(1.0792952,0.47623572),(2.6042538,4.252978e-2),(4.0851264,0.53812695),(4.712389,-0.50425416),(0.4435295,1.0874313),(2.6694477,-0.4260103),(5.406888,0.39609724),(5.4558945,9.471178e-5),(0.40488172,1.0906446),(5.213649,0.49002618),(3.648202,0.15560223),(1.3925505,0.2770956),(5.007658,-0.19889463),(6.2680044,-0.5555474),(4.951024,0.73994595),(3.465972,-0.15411195),(0.16081977,7.8556634e-2),(0.18408342,1.0969883),(2.7106795,-0.32472938),(2.6413836,1.0468142),(1.0893681,4.9841534e-2),(0.972677,1.1699796),(1.3086164,0.5070484),(6.2034564,1.0779898),(4.651469,0.9367015),(0.8488927,-5.2755535e-2),(3.7047868,0.77219695),(0.41806483,-0.5833577),(4.857506,0.7394532),(0.32292676,-0.106884986),(1.9461274,-0.28538352),(1.3841348,0.47596583),(5.7449093,0.37868357),(2.2499232,4.339968e-2),(2.0350204,-0.43405452),(2.9462397,0.56981087),(5.2573795,0.5513684),(0.47654128,0.8354136),(3.7348216,0.16430223),(1.4268454,-0.5656911),(1.7751285,0.44070128),(1.8382459,-8.372329e-2),(1.4213305,0.15594037),(4.712389,-0.47388044),(3.028317,0.22856916),(1.7339851,0.118912205),(5.2986684,1.1704198),(1.3093021,0.6355781),(4.712389,-0.4877464),(4.8924823,-8.631324e-2),(0.8767221,2.4290108e-3),(3.9250374,6.967458e-2),(5.036791,0.19237168),(2.0228662,-0.41489246),(1.7912569,7.767485e-3),(2.6891441,0.8703967),(3.8266807,1.2960639),(0.32534027,1.0952467),(0.99251604,1.0914029),(1.1749816,-0.2660734),(2.8654163,1.0945269),(1.1807181,0.8139722),(0.53509593,0.37650114),(5.3834157,-0.22396849),(3.0285048,0.91400814),(0.7943857,0.44876957),(2.5061083,0.84143883),(0.6092942,0.7303613),(3.437586,1.040249),(4.2177706,0.977887),(0.5050463,1.0809405),(3.3635392,1.5956035e-2),(4.712389,-0.49930158),(3.9315567,-0.18309043),(3.188348,-0.24313517),(2.9540527,0.4702638),(5.724622,0.68266124),(0.15113473,0.42338145),(2.4142246,0.21835586),(1.6387138,-9.22699e-3),(4.7618637,-0.32752123),(4.709302,-0.414729),(4.0854535,0.27493513),(1.8577647,2.7394248e-2),(4.0247083,-0.506914),(5.1184354,0.40044433),(1.8054696,-1.9969795e-2),(5.461546,1.164524),(1.3000618,0.109786525),(1.790054,1.0115127e-2),(4.712389,-0.47125766),(5.0471725,-0.4771885),(5.2230425,0.32714063),(2.0568209,0.65024656),(1.6407765,0.29171142),(4.4810653,0.59418243),(3.009115,0.93078256),(5.611641,0.88392806),(6.045621,-0.41651148),(2.299907,-0.2486362)]

> test_periodDecRA = TestCase $ do
>     assertEqual "test_periodDecRA" expected (periodDecRA periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [(5.611585,0.88389367),(2.0540617,-0.27464512),(0.4683349,-0.5072232),(1.2331495,0.8051078),(1.858526,-0.12282421),(4.318395,0.5533878),(0.5082016,-0.49110794),(2.809732,1.2163651),(2.154395,0.5001144),(3.4659338,1.0535471),(5.0218077,1.0495619),(5.2037144,0.47537318),(1.0976746,7.299652e-2),(4.984044,0.53972495),(0.4616151,0.83740026),(4.5176225,9.519441e-2),(1.9307709,0.3817392),(3.0387373,0.34255493),(2.892424,-0.1744196),(1.218741,0.8163508),(2.8819237,0.1501175),(1.3509327,0.69803804),(3.919384,0.5510235),(0.500196,0.9723723),(1.7087058,0.37261942),(3.0048926,-0.24262673),(3.7335176,0.883044),(4.6364684,0.57971),(4.712389,-0.5057993),(2.5668106,-4.7268827e-2),(4.504141,1.0694193),(5.0527186,0.45509547),(3.3063054,0.13170244),(2.0755796,0.4473911),(0.13702846,-0.5354764),(5.797039,-0.18087214),(3.9745328,-0.31181464),(3.467835,0.101155855),(5.0960636,-0.18615025),(1.5605367,0.42608637),(3.3877187,0.615602),(5.0438013,0.20544977),(5.034764,0.18857574),(4.712389,-0.47331607),(5.132405,0.36253095),(5.332925,0.6454446),(0.5710633,1.0306063),(4.975602,-3.0572036e-2),(5.490058,0.79800963),(4.712389,-0.48526937),(0.86012745,-3.294235e-2),(4.5613203,0.31871307),(2.8023837,0.5450085),(0.61573344,1.0647041),(3.27224,-4.9259704e-2),(4.496201,-6.863196e-2),(1.2774023,1.6191654e-2),(4.757482,1.1519129),(5.198938,0.5075307),(3.4918878,-0.32560778),(4.712389,-0.50103384),(4.1375093,0.99980825),(4.910763,0.64607185),(3.9552033,-0.42784485),(6.1777143,0.36737704),(2.9853556,0.41769913),(2.9414415e-2,1.2501698),(2.199093,0.60944664),(3.8368502,-0.43018067),(3.0276396,0.46265897),(0.24470434,1.0972357),(5.9110146,-0.52266407),(5.8411922,0.6035951),(3.8176427,6.256886e-2),(1.8009102,0.27118987),(5.5950785,0.8735829),(3.3100474,0.20241466),(5.495553,0.7365372),(1.0918067,0.15346724),(5.3032255,0.9279352),(4.747118,1.093586),(1.9564886,0.21715273),(5.5531106,-0.59806275),(5.5862136,0.25072753),(2.5419014,0.9953381),(1.7594695,0.9134297),(1.5704693,-0.58189934),(2.49827,-0.3025735),(0.36141396,7.7850685e-2),(0.6991122,1.0481862),(1.6114947,7.979844e-2),(1.8695315,0.32950327),(5.984838,0.38801876),(6.2035794,0.1017484),(5.098386,-2.6071698e-3),(6.020699,1.0454168),(1.1436119,0.5680049),(1.9358552,-0.26702267),(1.740989,0.10543607),(3.5923,-0.48442215)]


> test_sessionRA = TestCase $ do
>     assertEqual "test_sessionRA" expected (sessionRA sessions)
>   where
>     (sessions, _) = generateTestData 100
>     expected = [(0.0,0.0),(1.0,3.0),(2.0,7.0),(3.0,2.0),(4.0,5.0),(5.0,6.0),(6.0,5.0),(7.0,7.0),(8.0,6.0),(9.0,2.0),(10.0,3.0),(11.0,5.0),(12.0,5.0),(13.0,2.0),(14.0,3.0),(15.0,4.0),(16.0,4.0),(17.0,1.0),(18.0,8.0),(19.0,4.0),(20.0,6.0),(21.0,6.0),(22.0,3.0),(23.0,0.0),(24.0,3.0)]

> test_periodRA = TestCase $ do
>     assertEqual "test_periodRA" expected (periodRA periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [(0.0,0.0),(1.0,3.0),(2.0,5.0),(3.0,3.0),(4.0,1.0),(5.0,6.0),(6.0,3.0),(7.0,5.0),(8.0,7.0),(9.0,2.0),(10.0,3.0),(11.0,2.0),(12.0,6.0),(13.0,4.0),(14.0,4.0),(15.0,4.0),(16.0,3.0),(17.0,1.0),(18.0,9.0),(19.0,3.0),(20.0,11.0),(21.0,4.0),(22.0,4.0),(23.0,5.0),(24.0,2.0)]

> test_sessionDec = TestCase $ do
>     assertEqual "test_sessionDec" expected (sessionDec sessions)
>   where
>     (sessions, _) = generateTestData 100
>     expected = [(-40.0,0.0),(-39.0,0.0),(-38.0,0.0),(-37.0,0.0),(-36.0,0.0),(-35.0,0.0),(-34.0,0.0),(-33.0,1.0),(-32.0,1.0),(-31.0,1.0),(-30.0,0.0),(-29.0,1.0),(-28.0,2.0),(-27.0,4.0),(-26.0,0.0),(-25.0,0.0),(-24.0,2.0),(-23.0,3.0),(-22.0,0.0),(-21.0,0.0),(-20.0,0.0),(-19.0,0.0),(-18.0,2.0),(-17.0,0.0),(-16.0,1.0),(-15.0,1.0),(-14.0,1.0),(-13.0,1.0),(-12.0,1.0),(-11.0,1.0),(-10.0,1.0),(-9.0,0.0),(-8.0,1.0),(-7.0,0.0),(-6.0,1.0),(-5.0,0.0),(-4.0,2.0),(-3.0,1.0),(-2.0,0.0),(-1.0,1.0),(0.0,1.0),(1.0,5.0),(2.0,1.0),(3.0,3.0),(4.0,1.0),(5.0,1.0),(6.0,0.0),(7.0,2.0),(8.0,0.0),(9.0,2.0),(10.0,1.0),(11.0,0.0),(12.0,1.0),(13.0,1.0),(14.0,1.0),(15.0,0.0),(16.0,2.0),(17.0,1.0),(18.0,0.0),(19.0,1.0),(20.0,0.0),(21.0,0.0),(22.0,2.0),(23.0,2.0),(24.0,0.0),(25.0,1.0),(26.0,2.0),(27.0,1.0),(28.0,2.0),(29.0,1.0),(30.0,1.0),(31.0,1.0),(32.0,1.0),(33.0,1.0),(34.0,0.0),(35.0,1.0),(36.0,0.0),(37.0,1.0),(38.0,1.0),(39.0,0.0),(40.0,1.0),(41.0,0.0),(42.0,1.0),(43.0,2.0),(44.0,0.0),(45.0,1.0),(46.0,0.0),(47.0,1.0),(48.0,1.0),(49.0,1.0),(50.0,1.0),(51.0,1.0),(52.0,0.0),(53.0,1.0),(54.0,2.0),(55.0,0.0),(56.0,0.0),(57.0,1.0),(58.0,0.0),(59.0,0.0),(60.0,2.0),(61.0,0.0),(62.0,2.0),(63.0,6.0),(64.0,0.0),(65.0,0.0),(66.0,0.0),(67.0,1.0),(68.0,2.0),(69.0,0.0),(70.0,0.0),(71.0,0.0),(72.0,0.0),(73.0,0.0),(74.0,0.0),(75.0,1.0),(76.0,0.0),(77.0,0.0),(78.0,0.0),(79.0,0.0),(80.0,0.0),(81.0,0.0),(82.0,0.0),(83.0,0.0),(84.0,0.0),(85.0,0.0),(86.0,0.0),(87.0,0.0),(88.0,0.0),(89.0,0.0),(90.0,0.0)]

> test_periodDec = TestCase $ do
>     assertEqual "test_periodDec" expected (periodDec periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [(-40.0,0.0),(-39.0,0.0),(-38.0,0.0),(-37.0,0.0),(-36.0,0.0),(-35.0,0.0),(-34.0,1.0),(-33.0,1.0),(-32.0,0.0),(-31.0,0.0),(-30.0,1.0),(-29.0,2.0),(-28.0,3.0),(-27.0,3.0),(-26.0,0.0),(-25.0,0.0),(-24.0,2.0),(-23.0,0.0),(-22.0,0.0),(-21.0,0.0),(-20.0,0.0),(-19.0,0.0),(-18.0,1.0),(-17.0,2.0),(-16.0,0.0),(-15.0,2.0),(-14.0,0.0),(-13.0,1.0),(-12.0,0.0),(-11.0,0.0),(-10.0,2.0),(-9.0,1.0),(-8.0,0.0),(-7.0,1.0),(-6.0,0.0),(-5.0,0.0),(-4.0,0.0),(-3.0,1.0),(-2.0,2.0),(-1.0,2.0),(0.0,1.0),(1.0,1.0),(2.0,0.0),(3.0,0.0),(4.0,1.0),(5.0,3.0),(6.0,3.0),(7.0,1.0),(8.0,1.0),(9.0,2.0),(10.0,0.0),(11.0,1.0),(12.0,2.0),(13.0,1.0),(14.0,0.0),(15.0,1.0),(16.0,1.0),(17.0,0.0),(18.0,0.0),(19.0,2.0),(20.0,1.0),(21.0,1.0),(22.0,3.0),(23.0,1.0),(24.0,1.0),(25.0,1.0),(26.0,1.0),(27.0,2.0),(28.0,1.0),(29.0,1.0),(30.0,1.0),(31.0,1.0),(32.0,3.0),(33.0,1.0),(34.0,1.0),(35.0,2.0),(36.0,1.0),(37.0,1.0),(38.0,1.0),(39.0,0.0),(40.0,1.0),(41.0,0.0),(42.0,0.0),(43.0,1.0),(44.0,0.0),(45.0,0.0),(46.0,1.0),(47.0,2.0),(48.0,1.0),(49.0,0.0),(50.0,0.0),(51.0,3.0),(52.0,0.0),(53.0,1.0),(54.0,1.0),(55.0,0.0),(56.0,1.0),(57.0,0.0),(58.0,2.0),(59.0,0.0),(60.0,2.0),(61.0,3.0),(62.0,2.0),(63.0,2.0),(64.0,0.0),(65.0,0.0),(66.0,1.0),(67.0,0.0),(68.0,0.0),(69.0,0.0),(70.0,1.0),(71.0,0.0),(72.0,1.0),(73.0,0.0),(74.0,0.0),(75.0,0.0),(76.0,0.0),(77.0,0.0),(78.0,0.0),(79.0,0.0),(80.0,0.0),(81.0,0.0),(82.0,0.0),(83.0,0.0),(84.0,0.0),(85.0,0.0),(86.0,0.0),(87.0,0.0),(88.0,0.0),(89.0,0.0),(90.0,0.0)]

> test_sessionFreq = TestCase $ do
>     assertEqual "test_sessionFreq" expected (sessionFreq sessions)
>   where
>     (sessions, _) = generateTestData 100
>     expected = [(1.0,0),(2.0,41025),(3.0,375),(4.0,1800),(5.0,1995),(6.0,1560),(7.0,0),(8.0,0),(9.0,7020),(10.0,4350),(11.0,0),(12.0,0),(13.0,3225),(14.0,5190),(15.0,1575),(16.0,0),(17.0,0),(18.0,0),(19.0,960),(20.0,675),(21.0,0),(22.0,0),(23.0,5475),(24.0,0),(25.0,1350),(26.0,0),(27.0,3705),(28.0,3270),(29.0,0),(30.0,0),(31.0,1560),(32.0,1350),(33.0,1095),(34.0,645),(35.0,1995),(36.0,2025),(37.0,1530),(38.0,0),(39.0,1065),(40.0,0),(41.0,0),(42.0,900),(43.0,720),(44.0,1215),(45.0,0),(46.0,1950),(47.0,3825),(48.0,0),(49.0,1185),(50.0,0)]

> test_sessionFreq2 = TestCase $ do
>     assertEqual "test_sessionFreq2_1" cnt4_5   (snd (freqHist!!4)) 
>     assertEqual "test_sessionFreq2_2" cnt5_6   (snd (freqHist!!5)) 
>     assertEqual "test_sessionFreq2_3" cnt22_23 (snd (freqHist!!22)) 
>     assertEqual "test_sessionFreq2_4" cnt27_28 (snd (freqHist!!27)) 
>  where
>    ss = getOpenPSessions
>    freqHist = sessionFreq ss
>    cnt4_5 = 3600
>    cnt5_6 = 2400
>    cnt22_23 = 1800
>    cnt27_28  = 4800 + 4800
>     

Test border affects in histograms - put a frequency right at 2.0 and see
what bin it shows up in.

> test_sessionFreqHrs = TestCase $ do
>     assertEqual "test_sessionFreqHrs" 1.0 (snd (freqHist!!1))
>     assertEqual "test_sessionFreqHrs" 1.0 (snd (freqHist!!2))
>   where
>     s1 = defaultSession { totalTime = 60, frequency = 2.0 }
>     s2 = defaultSession { totalTime = 60, frequency = 2.2 }
>     freqHist = sessionFreqHrs [s1, s2]

> test_periodFreq = TestCase $ do
>     assertEqual "test_periodFreq" expected (periodFreq periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [(1.0,0),(2.0,13605),(3.0,1815),(4.0,1755),(5.0,1140),(6.0,1260),(7.0,0),(8.0,0),(9.0,465),(10.0,645),(11.0,0),(12.0,0),(13.0,690),(14.0,960),(15.0,0),(16.0,0),(17.0,0),(18.0,0),(19.0,150),(20.0,0),(21.0,600),(22.0,0),(23.0,2565),(24.0,0),(25.0,0),(26.0,0),(27.0,225),(28.0,0),(29.0,0),(30.0,1140),(31.0,0),(32.0,0),(33.0,375),(34.0,150),(35.0,0),(36.0,0),(37.0,315),(38.0,660),(39.0,525),(40.0,600),(41.0,1335),(42.0,0),(43.0,375),(44.0,0),(45.0,150),(46.0,540),(47.0,0),(48.0,60),(49.0,0),(50.0,0)]

> -- This test is failing because auto-generated report range only needs to go to 11.
> test_sessionTP = TestCase $ do
>     assertEqual "test_sessionTP" expected (sessionTP periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [(1.0,3),(2.0,10),(3.0,12),(4.0,14),(5.0,8),(6.0,10),(7.0,11),(8.0,12),(9.0,13),(10.0,7),(11.0,0)]

> -- This test is failing because auto-generated report range only needs to go to 3.
> test_sessionTP2 = TestCase $ do
>     assertEqual "test_sessionTP2" exp cnt
>   where
>     cnt = sessionTP ps
>     ps = [p1, p2, p1, p2, p1]
>     p1 = defaultPeriod {duration = 60}
>     p2 = defaultPeriod {duration = 150}
>     exp = [(1.0,3),(2.0,0),(3.0,2)]

> test_sessionTPQtrs = TestCase $ do
>     assertEqual "test_sessionTPQtrs" exp cnt
>   where
>     cnt = take 8 $ sessionTPQtrs ps
>     ps = [p1, p2, p1, p2, p1]
>     p1 = defaultPeriod {duration = 30}
>     p2 = defaultPeriod {duration = 105}
>     q  = quarter
>     exp = [(0,0),(1*q,0),(2*q,3),(3*q,0),(4*q,0),(5*q,0),(6*q,0),(7*q,2)]

> test_periodDuration = TestCase $ do
>     assertEqual "test_periodDuration" exp cnt
>   where
>     cnt = take 8 $ periodDuration ps
>     ps = [p1, p2, p1, p2, p1]
>     p1 = defaultPeriod {duration = 30}
>     p2 = defaultPeriod {duration = 105}
>     q  = quarter
>     exp = [(0,0),(1*q,0),(2*q,(3*30)),(3*q,0),(4*q,0),(5*q,0),(6*q,0),(7*q,(2*105))]

> test_sessionMinDuration = TestCase $ do
>     assertEqual "test_sessionMinDuration" exp cnt
>   where
>     cnt = take 8 $ sessionMinDuration ss
>     ss = [s1, s2, s1, s2, s1]
>     s1 = defaultSession {minDuration = 30}
>     s2 = defaultSession {minDuration = 105}
>     q  = quarter
>     exp = [(0,0),(1*q,0),(2*q,(3*30)),(3*q,0),(4*q,0),(5*q,0),(6*q,0),(7*q,(2*105))]

> test_freqTime = TestCase $ do
>     assertEqual "test_freqTime" expected (freqTime periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [(1199145600,26.590275),(1199232000,4.2308164),(1199232000,8.545437),(1199145600,29.43433),(1199232000,2.0),(1199145600,22.2),(1199232000,2.0),(1199145600,9.564482),(1199145600,3.6233535),(1199145600,42.254066),(1199145600,3.366477),(1199232000,2.0),(1199232000,2.0),(1199232000,2.0),(1199145600,13.078968),(1199232000,5.0934834),(1199232000,45.92087),(1199232000,13.252991),(1199145600,2.0),(1199232000,2.0),(1199145600,2.0),(1199232000,22.2),(1199232000,2.0),(1199232000,38.70217),(1199232000,22.2),(1199232000,3.2661185),(1199145600,37.20534),(1199232000,2.0),(1199232000,4.90036),(1199232000,44.334812),(1199145600,29.68108),(1199232000,2.0),(1199232000,2.0),(1199145600,2.0),(1199232000,5.34516),(1199232000,29.017683),(1199145600,2.0),(1199232000,32.833515),(1199145600,2.0),(1199145600,2.0),(1199145600,40.167473),(1199232000,37.422455),(1199145600,3.8554935),(1199232000,5.75789),(1199232000,36.23306),(1199145600,2.0),(1199145600,12.526739),(1199145600,42.382786),(1199145600,37.664043),(1199145600,2.0),(1199232000,2.510288),(1199145600,2.0),(1199145600,2.0),(1199232000,2.0),(1199145600,2.0),(1199232000,22.2),(1199232000,9.309816),(1199145600,2.0),(1199145600,4.288614),(1199145600,12.81987),(1199145600,2.0),(1199232000,2.0),(1199145600,39.43843),(1199145600,2.0),(1199232000,3.8985143),(1199145600,22.2),(1199145600,2.0),(1199145600,2.553279),(1199232000,2.0),(1199232000,9.334478),(1199145600,2.0),(1199232000,2.0),(1199232000,2.0),(1199145600,2.1527443),(1199232000,2.0),(1199232000,2.0),(1199145600,9.359476),(1199145600,5.271047),(1199145600,2.0),(1199145600,40.1292),(1199232000,2.0),(1199145600,47.41227),(1199232000,20.625532),(1199232000,2.0),(1199232000,2.0),(1199232000,40.872032),(1199232000,3.8807325),(1199232000,40.950226),(1199145600,33.46852),(1199232000,2.0),(1199145600,2.0),(1199145600,2.3462698),(1199232000,2.0),(1199232000,2.0),(1199232000,2.0),(1199232000,22.2),(1199145600,2.0),(1199232000,18.224922),(1199145600,22.2),(1199145600,2.0)]

> test_periodBand = TestCase $ do
>     assertEqual "test_periodBand" expected (periodBand periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [(L,226.75),(S,59.5),(C,40.0),(X,18.5),(U,27.5),(K,55.25),(A,66.5),(Q,41.0)]

> test_periodEfficiencyByBand = TestCase $ do
>     assertEqual "test_periodEfficiencyByBand" expected (periodEfficiencyByBand periods $ getEfficiencies $ length periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [(L,58.466187),(S,17.29306),(C,9.189224),(X,4.37861),(U,7.2443223),(K,28.972088),(A,12.67433),(Q,14.974708)]

> test_decVsElevation = TestCase $ do
>     assertEqual "test_decVsElevation" expected (decVsElevation periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [(30.5135,0.88389367),(12.128235,-0.27464512),(6.7846527,-0.5072232),(81.143875,0.8051078),(37.83998,-0.12282421),(-17.655136,0.5533878),(9.491768,-0.49110794),(25.914787,1.2163651),(42.47911,0.5001144),(10.341194,1.0535471),(23.057686,1.0495619),(-12.1045685,0.47537318),(55.741035,7.299652e-2),(-3.9065247,0.53972495),(45.4461,0.83740026),(-44.242554,9.519441e-2),(65.05242,0.3817392),(4.0755005,0.34255493),(-36.1072,-0.1744196),(80.45042,0.8163508),(2.1008072,0.1501175),(76.68196,0.69803804),(-19.993439,0.5510235),(47.922543,0.9723723),(29.76619,0.37261942),(2.83284,-0.24262673),(6.115944,0.883044),(-18.332405,0.57971),(-65.03862,-0.5057993),(-15.393959,-4.7268827e-2),(9.734077,1.0694193),(-9.394196,0.45509547),(-40.32814,0.13170244),(31.37399,0.4473911),(12.037437,-0.5354764),(0.14824677,-0.18087214),(-48.466507,-0.31181464),(-26.637772,0.101155855),(-30.487808,-0.18615025),(61.19445,0.42608637),(-11.220093,0.615602),(-4.018776,0.20544977),(-22.974297,0.18857574),(-47.731323,-0.47331607),(-12.484047,0.36253095),(19.036514,0.6454446),(64.25675,1.0306063),(-24.599548,-3.0572036e-2),(32.854965,0.79800963),(-73.69939,-0.48526937),(49.679317,-3.294235e-2),(-27.786583,0.31871307),(24.140656,0.5450085),(52.945454,1.0647041),(-47.509735,-4.9259704e-2),(-55.11371,-6.863196e-2),(51.965252,1.6191654e-2),(14.496529,1.1519129),(-16.38816,0.5075307),(-44.015274,-0.32560778),(-80.25581,-0.50103384),(5.7622147,0.99980825),(-14.407089,0.64607185),(-56.85611,-0.42784485),(38.553074,0.36737704),(-0.5047302,0.41769913),(37.762398,1.2501698),(66.05587,0.60944664),(-74.861496,-0.43018067),(-16.766953,0.46265897),(50.32417,1.0972357),(6.732132,-0.52266407),(38.836723,0.6035951),(-28.203285,6.256886e-2),(52.345245,0.27118987),(35.818073,0.8735829),(-37.33732,0.20241466),(1.2473831,0.7365372),(60.335125,0.15346724),(14.621445,0.9279352),(22.65686,1.093586),(11.681702,0.21715273),(-57.106308,-0.59806275),(22.56971,0.25072753),(26.349697,0.9953381),(72.724754,0.9134297),(-1.8308029,-0.58189934),(-9.24913,-0.3025735),(52.360043,7.7850685e-2),(67.33705,1.0481862),(34.771507,7.979844e-2),(57.09979,0.32950327),(13.272362,0.38801876),(33.525425,0.1017484),(-14.718452,-2.6071698e-3),(30.674904,1.0454168),(84.03097,0.5680049),(4.4080124,-0.26702267),(53.927937,0.10543607),(-47.813904,-0.48442215)]

> test_efficiencyVsFrequency = TestCase $ do
>     assertEqual "test_efficiencyVsFreq" expected (efficiencyVsFrequency sessions $ getEfficiencies $ length sessions)
>   where
>     (sessions, _) = generateTestData 100
>     expected = [(18.03244,0.98727703),(2.0,0.35925463),(2.0,0.23123395),(13.329185,0.10321328),(22.2,0.4751926),(5.8117433,0.34717193),(34.831585,0.21915126),(2.0,9.1130584e-2),(9.647265,0.4631099),(27.0602,0.33508924),(34.159927,0.20706856),(32.254818,7.904789e-2),(12.633172,0.4510272),(2.0,0.32300654),(27.28363,0.19498587),(13.313569,6.696519e-2),(8.989425,0.43894452),(12.533225,0.31092384),(2.0,0.18290317),(2.0,5.4882497e-2),(2.0,0.42686182),(2.1804707,0.29884115),(13.148822,0.17082047),(2.0,4.279983e-2),(2.0,0.41477913),(46.876442,0.28675845),(45.272423,0.15873778),(2.0,3.0717134e-2),(2.0,0.40269643),(8.363153,0.2746758),(3.9028223,0.14665508),(38.153526,1.8634439e-2),(19.351812,0.39061373),(2.0,0.2625931),(2.0,0.13457239),(24.259632,6.5517426e-3),(2.0,0.37853104),(5.643878,0.2505104),(33.07285,0.12248972),(2.0,0.99446905),(8.369127,0.36644834),(2.0,0.23842767),(22.2,0.110407025),(9.3495655,0.98238635),(22.2,0.35436565),(2.0,0.22634497),(48.359962,9.832433e-2),(2.0,0.97030365),(4.557304,0.34228295),(34.265385,0.2142623),(5.726377,8.624163e-2),(26.562767,0.45822093),(22.71207,0.33020025),(2.0,0.20217961),(2.0,7.415894e-2),(13.173273,0.44613823),(2.0,0.31811756),(36.141354,0.19009691),(2.0,6.207624e-2),(8.283322,0.43405554),(30.796986,0.30603486),(46.121742,0.17801422),(2.0,4.9993545e-2),(2.0,0.42197287),(2.0,0.29395217),(3.8919954,0.16593152),(2.0,3.791085e-2),(12.842893,0.40989017),(2.0,0.28186947),(8.347389,0.15384883),(43.295284,2.5828153e-2),(2.0,0.39780748),(14.645904,0.26978678),(9.703919,0.14176613),(22.2,1.3745457e-2),(2.0,0.38572478),(2.0,0.25770408),(46.49578,0.12968343),(22.2,1.662761e-3),(42.9339,0.3736421),(9.160325,0.24562141),(2.0,0.11760074),(31.575743,0.98958004),(41.64395,0.3615594),(2.0,0.23353872),(2.0,0.10551804),(26.966955,0.97749734),(2.0,0.3494767),(45.750862,0.22145602),(2.0,9.343535e-2),(35.298923,0.96541464),(27.078657,0.337394),(2.0,0.20937333),(35.33423,8.135265e-2),(2.0,0.95333195),(26.398733,0.3253113),(2.0,0.19729063),(4.442998,6.9269955e-2),(8.728631,0.94124925),(2.0,0.3132286)]

> test_historicalFreq = TestCase $ do
>     assertEqual "test_historicalFreq" expected (historicalFreq periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [26.590275,4.2308164,8.545437,29.43433,2.0,22.2,2.0,9.564482,3.6233535,42.254066,3.366477,2.0,2.0,2.0,13.078968,5.0934834,45.92087,13.252991,2.0,2.0,2.0,22.2,2.0,38.70217,22.2,3.2661185,37.20534,2.0,4.90036,44.334812,29.68108,2.0,2.0,2.0,5.34516,29.017683,2.0,32.833515,2.0,2.0,40.167473,37.422455,3.8554935,5.75789,36.23306,2.0,12.526739,42.382786,37.664043,2.0,2.510288,2.0,2.0,2.0,2.0,22.2,9.309816,2.0,4.288614,12.81987,2.0,2.0,39.43843,2.0,3.8985143,22.2,2.0,2.553279,2.0,9.334478,2.0,2.0,2.0,2.1527443,2.0,2.0,9.359476,5.271047,2.0,40.1292,2.0,47.41227,20.625532,2.0,2.0,40.872032,3.8807325,40.950226,33.46852,2.0,2.0,2.3462698,2.0,2.0,2.0,22.2,2.0,18.224922,22.2,2.0]

> test_historicalDec = TestCase $ do
>     assertEqual "test_historicalDec" expected (historicalDec periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [0.88389367,-0.27464512,-0.5072232,0.8051078,-0.12282421,0.5533878,-0.49110794,1.2163651,0.5001144,1.0535471,1.0495619,0.47537318,7.299652e-2,0.53972495,0.83740026,9.519441e-2,0.3817392,0.34255493,-0.1744196,0.8163508,0.1501175,0.69803804,0.5510235,0.9723723,0.37261942,-0.24262673,0.883044,0.57971,-0.5057993,-4.7268827e-2,1.0694193,0.45509547,0.13170244,0.4473911,-0.5354764,-0.18087214,-0.31181464,0.101155855,-0.18615025,0.42608637,0.615602,0.20544977,0.18857574,-0.47331607,0.36253095,0.6454446,1.0306063,-3.0572036e-2,0.79800963,-0.48526937,-3.294235e-2,0.31871307,0.5450085,1.0647041,-4.9259704e-2,-6.863196e-2,1.6191654e-2,1.1519129,0.5075307,-0.32560778,-0.50103384,0.99980825,0.64607185,-0.42784485,0.36737704,0.41769913,1.2501698,0.60944664,-0.43018067,0.46265897,1.0972357,-0.52266407,0.6035951,6.256886e-2,0.27118987,0.8735829,0.20241466,0.7365372,0.15346724,0.9279352,1.093586,0.21715273,-0.59806275,0.25072753,0.9953381,0.9134297,-0.58189934,-0.3025735,7.7850685e-2,1.0481862,7.979844e-2,0.32950327,0.38801876,0.1017484,-2.6071698e-3,1.0454168,0.5680049,-0.26702267,0.10543607,-0.48442215]

> test_historicalRA = TestCase $ do
>     assertEqual "test_historicalRA" expected (historicalRA periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [5.611585,2.0540617,0.4683349,1.2331495,1.858526,4.318395,0.5082016,2.809732,2.154395,3.4659338,5.0218077,5.2037144,1.0976746,4.984044,0.4616151,4.5176225,1.9307709,3.0387373,2.892424,1.218741,2.8819237,1.3509327,3.919384,0.500196,1.7087058,3.0048926,3.7335176,4.6364684,4.712389,2.5668106,4.504141,5.0527186,3.3063054,2.0755796,0.13702846,5.797039,3.9745328,3.467835,5.0960636,1.5605367,3.3877187,5.0438013,5.034764,4.712389,5.132405,5.332925,0.5710633,4.975602,5.490058,4.712389,0.86012745,4.5613203,2.8023837,0.61573344,3.27224,4.496201,1.2774023,4.757482,5.198938,3.4918878,4.712389,4.1375093,4.910763,3.9552033,6.1777143,2.9853556,2.9414415e-2,2.199093,3.8368502,3.0276396,0.24470434,5.9110146,5.8411922,3.8176427,1.8009102,5.5950785,3.3100474,5.495553,1.0918067,5.3032255,4.747118,1.9564886,5.5531106,5.5862136,2.5419014,1.7594695,1.5704693,2.49827,0.36141396,0.6991122,1.6114947,1.8695315,5.984838,6.2035794,5.098386,6.020699,1.1436119,1.9358552,1.740989,3.5923]

> test_historicalTime = TestCase $ do
>     assertEqual "test_historicalTime" expected (historicalTime periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [1199145600,1199232000,1199232000,1199145600,1199232000,1199145600,1199232000,1199145600,1199145600,1199145600,1199145600,1199232000,1199232000,1199232000,1199145600,1199232000,1199232000,1199232000,1199145600,1199232000,1199145600,1199232000,1199232000,1199232000,1199232000,1199232000,1199145600,1199232000,1199232000,1199232000,1199145600,1199232000,1199232000,1199145600,1199232000,1199232000,1199145600,1199232000,1199145600,1199145600,1199145600,1199232000,1199145600,1199232000,1199232000,1199145600,1199145600,1199145600,1199145600,1199145600,1199232000,1199145600,1199145600,1199232000,1199145600,1199232000,1199232000,1199145600,1199145600,1199145600,1199145600,1199232000,1199145600,1199145600,1199232000,1199145600,1199145600,1199145600,1199232000,1199232000,1199145600,1199232000,1199232000,1199145600,1199232000,1199232000,1199145600,1199145600,1199145600,1199145600,1199232000,1199145600,1199232000,1199232000,1199232000,1199232000,1199232000,1199232000,1199145600,1199232000,1199145600,1199145600,1199232000,1199232000,1199232000,1199232000,1199145600,1199232000,1199145600,1199145600]

> test_historicalTime' = TestCase $ do
>     assertEqual "test_historicalTime'" expected (historicalTime' periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]

> test_historicalLST = TestCase $ do
>     assertEqual "test_historicalLST" expected (historicalLST periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [3.2261043,3.8558538,5.265954,5.1062374,5.265954,3.2261043,5.171948,2.5680578,4.4481907,2.6620646,2.6620646,4.889928,4.1378736,3.385821,5.858292,4.3258877,5.9240007,4.889928,2.4740503,5.0779405,4.7302117,6.300029,3.009794,5.829994,1.9757212,6.488042,5.012232,5.829994,4.1378736,2.6337676,5.012232,3.8558538,1.9757212,3.3201115,3.103801,3.5738337,5.9522986,4.513901,3.5081253,3.978157,2.9440842,2.2577407,3.978157,2.7277734,4.0438676,2.6620646,3.7901444,3.0380914,2.2860377,5.012232,3.2918146,3.6021307,5.2002444,5.547974,2.192031,4.7959213,4.4198937,5.858292,5.7642846,4.542198,5.9522986,3.5738337,6.4223313,5.4822636,3.385821,3.978157,5.7642846,6.4223313,2.163734,1.9757212,4.4481907,1.9757212,2.6337676,5.858292,4.607908,2.5397608,1.8160039,5.858292,4.260178,4.3541846,1.9757212,1.8160039,6.488042,2.163734,3.009794,5.7359867,1.8817143,3.6678412,2.5680578,3.385821,2.9440842,5.1062374,4.889928,3.103801,2.7277734,5.265954,4.4481907,2.6337676,5.4822636,5.012232]

> test_satisfactionRatio = TestCase $ do
>     assertEqual "test_satisfactionRatio" expected (satisfactionRatio sessions periods)
>   where
>     (sessions, periods) = generateTestData 100
>     expected = [(1.0,0.0,0.0),(2.0,1.0807323,5.132569e-3),(3.0,15.772972,0.20508842),(4.0,3.1774065,4.201459e-2),(5.0,1.8622164,3.055228e-2),(6.0,2.632171,4.1076623e-2),(7.0,0.0,0.0),(8.0,0.0,0.0),(9.0,0.2158659,5.54528e-3),(10.0,0.483213,1.0539614e-2),(11.0,0.0,0.0),(12.0,0.0,0.0),(13.0,0.6972484,1.4703777e-2),(14.0,0.6027983,1.077711e-2),(15.0,0.0,0.0),(16.0,0.0,0.0),(17.0,0.0,0.0),(18.0,0.0,0.0),(19.0,0.50919974,2.3030771e-2),(20.0,0.0,0.0),(21.0,1.0,1.0),(22.0,0.0,0.0),(23.0,1.5267622,1.6699122e-2),(24.0,0.0,0.0),(25.0,0.0,0.0),(26.0,0.0,0.0),(27.0,0.1979076,7.3086494e-3),(28.0,0.0,0.0),(29.0,0.0,0.0),(30.0,1.0,1.0),(31.0,0.0,0.0),(32.0,0.0,0.0),(33.0,1.1160543,3.1925347e-2),(34.0,0.7578787,3.427836e-2),(35.0,0.0,0.0),(36.0,0.0,0.0),(37.0,0.6709456,2.0941025e-2),(38.0,1.0,1.0),(39.0,1.6064893,3.883865e-2),(40.0,1.0,1.0),(41.0,1.0,1.0),(42.0,0.0,0.0),(43.0,1.6973324,4.8553128e-2),(44.0,0.0,0.0),(45.0,1.0,1.0),(46.0,0.90245867,2.1512771e-2),(47.0,0.0,0.0),(48.0,1.0,1.0),(49.0,0.0,0.0),(50.0,0.0,0.0)]

> test_findScheduleGaps = TestCase $ do
>   assertEqual "test_findScheduleGaps" exp gaps
>     where
>   start = fromGregorian 2006 2 1 0 0 0
>   dur   = 24 * 60
>   gaps = findScheduleGaps start dur ps 
>   dt1 = fromGregorian 2006 2 1 1 30 0 -- gap at start for 1.5 hrs
>   dt2 = fromGregorian 2006 2 1 5 30 0 -- gap p1-p2 of 1 hr
>   dur1 = 120
>   dur2 = 240
>   end1 = dur1 `addMinutes'` dt1
>   end2 = dur2 `addMinutes'` dt2
>   p1 = Period defaultSession dt1 dur1 0.0 undefined False
>   p2 = Period defaultSession dt2 dur2 0.0 undefined False
>   ps = [p1, p2]
>   exp = [(start, 90), (end1, 120), (end2, (14*60)+30)]

> test_getOriginalSchedule' = TestCase $ do
>   assertEqual "test_getOriginalSchedule'" exp original
>     where
>   (observed, canceled, failedBackups) = getTestPeriods
>   original = getOriginalSchedule' observed canceled
>   exp = sort $ observed ++ failedBackups
>   

> test_breakdownSimulationTimes = TestCase $ do
>   assertEqual "test_breakdownSimulationTimes" exp times
>     where
>   (observed, canceled, failedBackups) = getTestPeriods
>   start = fromGregorian 2006 2 1 0 0 0
>   dur = 12*60
>   times = breakdownSimulationTimes [defaultSession] start dur observed canceled
>   sessHrs = 0.0 :: Float
>   simHrs = 12.0 :: Float
>   shdHrs = 7.0 :: Float
>   obsHrs = 5.0 :: Float
>   cnlHrs = 5.0 :: Float
>   bckHrs = 3.0 :: Float
>   totalDead = 7.0 :: Float
>   scheduledDead = 5.0 :: Float
>   failedBackup = 2.0 :: Float
>   sessAvHrs = 0.0 :: Float
>   sessBackupHrs = 0.0 :: Float
>   sessAvBckp = 0.0 :: Float
>   exp = (simHrs, sessHrs, sessBackupHrs, sessAvHrs, sessAvBckp, shdHrs, obsHrs, cnlHrs, bckHrs, totalDead, scheduledDead, failedBackup)

Test utilities

> getTestPeriods :: ([Period], [Period], [Period])
> getTestPeriods = (observed, canceled, failedBackups)
>   where
>   start = fromGregorian 2006 2 1 0 0 0
>   dur = 60
>   dts = [(2*i*60) `addMinutes'` start | i <- [1..5]]
>   observed = zipWith mkPeriod dts [True, True, True, False, False] 
>   mkPeriod dt backup = Period defaultSession dt dur 0.0 undefined backup
>   canceled' = take 3 observed
>   canceledDts = [start, (5*60) `addMinutes'` start]
>   failedBackups = zipWith mkPeriod canceledDts [False, False]
>   canceled = sort $ failedBackups ++ canceled'
>   

> getEfficiencies    :: Int -> [Float]
> getEfficiencies n =
>     [fst $ randomR (0.0, 1.0) $ mkStdGen i | i <- [0 .. n]]

> assertAlmostEqual :: String -> Int -> Float -> Float -> IO ()
> assertAlmostEqual name places expected value =
>     assertBool name $ abs (value - expected) < epsilon
>   where
>     epsilon = 1.0 / 10.0 ** fromIntegral places

