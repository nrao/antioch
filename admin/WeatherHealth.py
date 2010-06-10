import pg
from datetime import *

class WeatherHealth:

    def __init__(self, dbname):

        self.dtFormat = "%Y-%m-%d %H:%M:%S"

        self.dbname = dbname # ex: "weather"

        self.cnn = pg.connect(user = "dss", dbname = dbname)

    def check(self):
        self.getAllForecastTimes()
        self.checkMissingForecastTimes()

    def getAllForecastTimes(self):
        query = "SELECT * FROM forecast_times ORDER BY date;"
        r = self.cnn.query(query)
        return r.dictresult()

    def checkMissingForecastTimes(self):
        rows = self.getAllForecastTimes()
        for i in range(len(rows)-1):
            r1 = rows[i]
            r2 = rows[i+1]
            dt1 = datetime.strptime(r1['date'], format)
            dt2 = datetime.strptime(r2['date'], format)
            hours = ((dt2 - dt1).seconds) / (60 * 60)
            
            if hours != 6:
                print dt1, dt2

    def checkForecastTimeHealth(self, forecastTime):
        "For a given forecast time, how does it's data look?"

        # there should be a certain number of forecasts ... (ex: 85)
        query = "SELECT f.id FROM forecasts as f, forecast_times as ft WHERE ft.id = f.forecast_time_id AND ft.date = '%s'" % forecastTime

        r = self.cnn.query(query)

        print len(r)

if __name__ == '__main__':
    dbname = sys.argv[1]
    print "checking health of db: ", dbname
    wh = WeatherHealth(dbname)
    wh.check()
