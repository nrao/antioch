# Copyright (C) 2011 Associated Universities, Inc. Washington DC, USA.
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
# 
# Correspondence concerning GBT software should be addressed as follows:
#       GBT Operations
#       National Radio Astronomy Observatory
#       P. O. Box 2
#       Green Bank, WV 24944-0002 USA

if __name__ == "__main__":
    import sys
    sys.path[1:1] = [".."]

from WeatherData        import WeatherData
from datetime           import datetime, timedelta
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

        dt = datetime(2009, 6, 7, 12) # UTC
        m = self.wd.getHourDanaMedianSpeeds(dt)
        self.assertAlmostEquals(0.52738451957702637, m, 4)

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
