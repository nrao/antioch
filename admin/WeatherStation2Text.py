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

import sys
from datetime    import datetime, timedelta
from TimeAgent   import dt2mjd
from WeatherData import WeatherData

# Reads measured winds from the telescope logs
#
# 1. mjd
# 2. weather station 2 wind speed (should be our 90th percentile,
#    20 sec median values)
# 
# python WeatherStation2Text.py 2003 365 >results.txt


class WeatherStation2Text:
    """
    This class contains logic to print 
    weather station 2 wind speeds using the WeatherData class
    """
    def __init__(self, year):
        self.dtFormat = "%Y-%m-%d %H:%M:%S"
        self.weatherData = WeatherData()
        self.start = datetime(year, 1, 1, 0, 0, 0)
        self.end = self.start + timedelta(hours = 24*365)

    def getWind(self, dt):
        """
        Just a wraper method to avoid using long ass method names
        and format the data.
        """
        return dt, self.weatherData.getLastHourMedianWindSpeeds(dt)

    def getWinds(self, hours):
        retval = []
        for h in range(hours):
            dt = self.start + timedelta(hours = h)
            try:
                retval.append(self.getWind(dt))
            except:
                pass
        return retval

    def print_winds(self, data):
        print "MJD Measured"
        for dt, m in data:
            mjd = dt2mjd(dt)
            print mjd, m

if __name__ == "__main__":
    wst = WeatherStation2Text(int(sys.argv[1]))
    winds = wst.getWinds(24*int(sys.argv[2]))
    wst.print_winds(winds)
