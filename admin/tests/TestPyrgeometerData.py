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

from PyrgeometerData        import PyrgeometerData
from datetime           import datetime, timedelta
import unittest
import nose

class TestWeatherData(unittest.TestCase):

    def setUp(self):
        self.pd = PyrgeometerData()

    def testGetHourMedianDownwardIrradiance(self):

        exp = [
            349.567321777
            , 369.547271729
            , 363.796203613
            , 367.676879883
            , 360.617858887
            , 366.510101318
            , 366.566253662
            , 369.266937256
            , 376.525360107
            , 373.901947021
            , 369.190643311
            , 371.558563232
            , 380.395202637
            , 380.33782959
            , 382.432250977
            , 387.385009766
            , 387.474578857
            , 391.059753418
            , 396.60168457
            , 392.65020752
            , 383.975860596
            , 371.716430664
            , 376.309783936
            , 352.016723633
        ]             
        start = datetime(2009, 5, 1, 0)
        for i in range(24):
            dt = start + timedelta(hours = i)
            d = self.pd.getHourMedianDownwardIrradiance(dt)
            self.assertAlmostEqual(exp[i], d)


    
if __name__ == "__main__":
    unittest.main()
