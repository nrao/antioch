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

if __name__ == "__main__":
    unittest.main()
    # for more verbosity:
    #suite = unittest.TestLoader().loadTestsFromTestCase(TestCleoDBImport)
    #unittest.TextTestRunner(verbosity=2).run(suite)

