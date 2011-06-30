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

import pg
import sys
import settings
from datetime import *
from TimeAgent import dt2mjd

# Reads both forecasted and measured winds from the specified
# weather database and prints:
#
# 1. mjd
# 2. forecast wind speed for the best forecast
# 3. weather station 2 wind speed (should be our 90th percentile,
#    20 sec median values)
# 
# python ExtractWind.py weather 2007 >results.txt

class ExtractWind:

    def __init__(self, dbname, year, filename = None):

        self.dtFormat = "%Y-%m-%d %H:%M:%S"
        self.dbname = dbname # e.g., "weather"
        self.start = datetime(year, 1, 1, 0, 0, 0)
        self.end = self.start + timedelta(hours = 24*365)
        self.cnn = pg.connect(user = "dss", dbname = dbname, port = settings.DATABASE_PORT)

    def read_gbt_weather(self):
        query = "SELECT wd.date, f.wind_speed_mph, gw.wind_speed FROM forecasts AS f, gbt_weather AS gw, weather_dates AS wd WHERE wd.id = gw.weather_date_id AND wd.id = f.weather_date_id AND forecast_type_id = 1 AND wd.date >= '%s' AND wd.date < '%s' order by wd.date" % (datetime.strftime(self.start, self.dtFormat), datetime.strftime(self.end, self.dtFormat))
        r = self.cnn.query(query)
        result = [(d['date'], d['wind_speed_mph'], d['wind_speed']) for d in r.dictresult()]
        return result

    def print_winds(self, data):
        print "MJD Forecast Measured"
        for d, f, m in data:
            dt = datetime.strptime(d, self.dtFormat)
            mjd = dt2mjd(dt)
            print mjd, f, m

if __name__ == "__main__":
    ew = ExtractWind(sys.argv[1], int(sys.argv[2]))
    winds = ew.read_gbt_weather()
    ew.print_winds(winds)
