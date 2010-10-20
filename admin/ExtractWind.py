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
