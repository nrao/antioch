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
from Weather2DBImport import Weather2DBImport
import unittest
import pg

class TestWeather2DBImport(unittest.TestCase):

    # Note: these aren't "unit tests" because they interact with a DB!

    def setUp(self):
        self.dbname = "weather_unit_tests"
        self.wdb = Weather2DBImport(self.dbname)

    def testGetNeededWeatherDates(self):
        needed = self.wdb.getNeededWeatherDates()
        self.assertEquals(868, len(needed))
        self.assertEquals((114, '2006-01-05 17:00:00'), needed[0])

    def testFindNullValues(self):    
        nulls =  self.wdb.findNullValues("wind_speed")
        self.assertEquals([], nulls)

    def testBackfillWind(self):

        # set the test flag so that we don't inadvertantely write
        # to our test DB
        results = self.wdb.backfillWind(test = True)
        self.assertEquals([], results)

       
if __name__ == "__main__":
    unittest.main()
    # for more verbosity:
    #suite = unittest.TestLoader().loadTestsFromTestCase(TestCleoDBImport)
    #unittest.TextTestRunner(verbosity=2).run(suite)
    
