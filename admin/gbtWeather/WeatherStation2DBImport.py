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

from datetime    import datetime, timedelta
from WeatherData import WeatherData
import pg

class WeatherStation2DBImport:
    """
    This class contains lodgic to populate the weather database with
    weather station 2 wind speeds using the WeatherData class
    """
    def __init__(self, dbname = "weather"):
        self.c           = pg.connect(user = "dss"
                                    , dbname = dbname)
        self.weatherData = WeatherData()

    def getWind(self, dt):
        """
        Just a wraper method to avoid using long ass method names
        and format the data.
        """
        dt = datetime.strptime(dt, "%Y-%m-%d %H:%M:%S")
        return dt, self.weatherData.getLastHourMedianWindSpeeds(dt)

    def getWindSpeeds(self):
        """
        Gets the wind speeds from weather station 2 data for every hour
        in the weather database that does not have a weather station 2
        record to now.
        """
        r = \
            self.c.query("""
                         SELECT id, date
                         FROM weather_dates
                         WHERE id NOT IN (SELECT weather_date_id
                                          FROM weather_station2)
                               AND date <= '%s'
                         """ % datetime.utcnow())
        return [self.getWind(row['date']) for row in r.dictresult()]

    def getWeatherDate(self, dt):
        r = \
            self.c.query("SELECT id FROM weather_dates WHERE date = '%s'" % dt)
        if len(r.dictresult()) == 0:
            self.c.query("INSERT INTO weather_dates (date) VALUES ('%s')" % dt)
            r = \
                self.c.query("SELECT id FROM weather_dates WHERE date = '%s'" % dt)
        return r.dictresult()[0]["id"]

    def insert(self):
        for dt, wind_speed in self.getWindSpeeds():
            wd_id = self.getWeatherDate(dt)
            # Handle if no weather station wind speed measure was taken
            if str(wind_speed) == 'nan':
                earlier = datetime.utcnow() - timedelta(hours = 4)
                if dt < earlier:
                    wind_speed = "NULL"
            self.c.query("""
                         INSERT INTO weather_station2 (wind_speed, weather_date_id)
                         VALUES (%s, %s)
                         """ % (wind_speed, wd_id))

if __name__ == "__main__":
    ws2 = WeatherStation2DBImport()
    ws2.insert()
