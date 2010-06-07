if __name__ == "__main__":
    import sys
    sys.path[1:1] = [".."]

from WeatherData        import WeatherData
from datetime           import datetime
import unittest
import nose

class TestWeatherData(unittest.TestCase):

    def setUp(self):
        self.wd = WeatherData()

    def testGetWindVelocity(self):
        # setup
        # define start/stop times
        start = (2007,11,30,0,0,0)
        end = (2007,12,30,0,0,0)
        expMJD = [ 54434.00000041
                ,  54434.00001201
                ,  54434.00002358
                ,  54434.00003515]
        expWind = [ 2.57343006,  2.87351751,  2.85987568,  2.87351751]

        # test
        time, wind = self.wd.getWindVelocity((start,end))
    
        for i in range(4):
            self.assertAlmostEquals(expMJD[i], time[i], 2)
            self.assertAlmostEquals(expWind[i], wind[i], 2)

    def testGetLastHourMedianWindSpeeds(self):

        # for when?
        now = datetime(2007,11,30,0,0,0)

        # test a specific time
        wind = self.wd.getLastHourMedianWindSpeeds(now)

        self.assertAlmostEquals(2.38246560, wind, 4)

        # make sure it changes by looking at the *current* wind
        wind = self.wd.getLastHourMedianWindSpeeds()

        self.assertNotEqual(2.38246560, wind)

    def testHourDanaMedianSpeeds(self):

        dt = datetime(2010, 6, 7, 12) # UTC
        m = self.wd.getHourDanaMedianSpeeds(dt)
        self.assertAlmostEquals(3.74649739265, m, 4)

    def testDanaMedian(self):

        # simple tests first
        data = [1.0 for i in range(3600)]
        m = self.wd.danaMedian(data)
        self.assertEqual(1.0, m)

        data = [1.0 for i in range(110)]
        m = self.wd.danaMedian(data)
        self.assertEqual(1.0, m)

        data = [float(i) for i in range(3600)]
        m = self.wd.danaMedian(data)
        self.assertEqual(3249.5, m)
    
if __name__ == "__main__":
    unittest.main()
