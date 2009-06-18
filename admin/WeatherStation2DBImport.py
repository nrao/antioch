from datetime    import datetime
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
        print type(dt), dt
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
            print "Inserting wind_speed for %s." % dt
            wd_id = self.getWeatherDate(dt)
            self.c.query("""
                         INSERT INTO weather_station2 (wind_speed, weather_date_id)
                         VALUES (%s, %s)
                         """ % (wind_speed, wd_id))

if __name__ == "__main__":
    ws2 = WeatherStation2DBImport()
    ws2.insert()
