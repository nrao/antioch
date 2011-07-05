# Copyright (C) 2009 Associated Universities, Inc. Washington DC, USA.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 675 Mass Ave Cambridge, MA 02139, USA.
#
# Correspondence concerning GBT software should be addressed as follows:
#     GBT Operations
#     National Radio Astronomy Observatory
#     P. O. Box 2
#     Green Bank, WV 24944-0002 USA

if __name__ == "__main__":
    import sys
    sys.path[1:1] = [".."]

from datetime import datetime, timedelta
from SolarHeating import SolarHeating

import unittest
import pg

class TestSolarHeating(unittest.TestCase):

    def setUp(self):

        self.sh = SolarHeating()

    def testIsDayTime(self):

        # for more then one day, cycle through each quarter, and see when 
        # the daytime flag toggles
        dts = []
        for i in range(24 * 4):
            mins = i * 15
            hour = int(mins / 60)
            min = int(mins % 60)
            dt = datetime(2010, 1, 1, hour, min, 0)
            dts.append(dt)

        # get just the physical sun rise/set times
        self.sh.day_offset = timedelta(hours=0)
        self.sh.night_offset = timedelta(hours=0)

        # according to:
        # http://aa.usno.navy.mil/data/docs/RS_OneYear.php
        # 1/1/2010: rise = 07:37, set = 17:11 ET
        #       ->  rise = 12:37, set = 22:11 UTC
        # so the first 12.5 hours should be false
        exp = [False for i in range(12*4 + 3)]
        exp.extend([True for i in range(9*4 + 2)])
        exp.extend([False for i in range(1*4 + 3)])

        for i, dt in enumerate(dts):
            self.assertEquals(exp[i], self.sh.isDayTime(dt))

    def testIsDayTime2(self):

        # get just the physical sun rise/set times
        self.sh.day_offset = timedelta(hours=0)
        self.sh.night_offset = timedelta(hours=0)
        # according to:
        # http://aa.usno.navy.mil/data/docs/RS_OneYear.php
        # 1/1/2010: rise = 07:37, set = 17:11 ET
        #       ->  rise = 12:37, set = 22:11 UTC
        # PTCS  ->  rise = 14:38, set =  1:11 UTC next day
        beforeRiseDt = datetime(2010, 1, 1, 12, 30)
        afterRiseDt  = datetime(2010, 1, 1, 12, 45)
        self.assertEquals(False, self.sh.isDayTime(beforeRiseDt))
        self.assertEquals(True,  self.sh.isDayTime(afterRiseDt))

        beforeSetDt = datetime(2010, 1, 1, 22, 0)
        afterSetDt  = datetime(2010, 1, 1, 22, 15)
        self.assertEquals(True, self.sh.isDayTime(beforeSetDt))
        self.assertEquals(False,  self.sh.isDayTime(afterSetDt))

        # now make sure the PTCS offsets don't muck things up
        self.sh.day_offset = timedelta(hours=2)
        self.sh.night_offset = timedelta(hours=3)
        beforeRiseDt = datetime(2010, 1, 1, 14, 30)
        afterRiseDt  = datetime(2010, 1, 1, 14, 45)
        self.assertEquals(False, self.sh.isDayTime(beforeRiseDt))
        self.assertEquals(True,  self.sh.isDayTime(afterRiseDt))

        beforeSetDt = datetime(2010, 1, 2, 1, 0)
        afterSetDt  = datetime(2010, 1, 2, 1, 15)
        self.assertEquals(True, self.sh.isDayTime(beforeSetDt))
        self.assertEquals(False,  self.sh.isDayTime(afterSetDt))

    def testGetSunRiseSet(self):

        # according to:
        # http://aa.usno.navy.mil/data/docs/RS_OneYear.php
        # 1/1/2010: rise = 07:37, set = 17:11 ET
        #       ->  rise = 12:37, set = 22:11 UTC
        # PTCS  ->  rise = 14:38, set =  1:11 UTC next day
        dt = datetime(2010, 1, 1)
        expRise = 12.0 + (37.0/60.0) 
        expSet  = 22.0 + (11.0/60.0)
        rise, set = self.sh.getSunRiseSet(dt)
        self.assertAlmostEquals(expRise, rise, 1)
        self.assertAlmostEquals(expSet, set, 1)

        # 4/20/2010: rise = 06:38, set = 20:01 ET
        #        ->  rise = 10:38, set = 24:01 UTC
        # PTCS   ->  rise = 12:38, set =  3:01 UTC next day
        dt = datetime(2010, 4, 20)
        expRise = 10.0 + (38.0/60.0) 
        expSet  = 24.0 + (1.0/60.0)
        rise, set = self.sh.getSunRiseSet(dt)
        self.assertAlmostEquals(expRise, rise, 1)
        self.assertAlmostEquals(expSet, set, 1)

if __name__ == "__main__":
    unittest.main()
    # for more verbosity:
    #suite = unittest.TestLoader().loadTestsFromTestCase(TestCleoDBImport)
    #unittest.TextTestRunner(verbosity=2).run(suite)

