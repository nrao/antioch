from datetime        import datetime, timedelta
from WeatherData     import WeatherData
from PyrgeometerData import PyrgeometerData
import pg
import settings

if __name__ == "__main__":
    import sys

class Weather2DBImport:
    """
    This class contains logic to populate the weather database with
    GBT weather data.
    """

    def __init__(self, dbname = ""):
        self.c           = pg.connect(user = "dss"
                                    , dbname = dbname
                                    , port   = settings.DATABASE_PORT
                                    )
        self.weatherData = WeatherData()
        self.pyrgeometerData = PyrgeometerData()

    def getNeededWeatherDates(self, dt = None):
        """
        Get's those dates that don't have any acomponying weather data.
        """
        if dt is None:
            dt = datetime.utcnow()        
        r = \
            self.c.query("""
                         SELECT id, date
                         FROM weather_dates
                         WHERE id NOT IN (SELECT weather_date_id
                                          FROM gbt_weather)
                               AND date <= '%s'
                         """ % dt)
        return [(row['id'], row['date']) for row in r.dictresult()]

    def insert(self, weatherDateId, wind, irradiance):
        """
        Inserts a row of data into the weather table.
        """
        windOK = wind and wind == wind
        irradianceOK = irradiance and irradiance == irradiance
        if windOK and irradianceOK:
            query = """
                    INSERT INTO gbt_weather (weather_date_id,wind_speed,irradiance)
                    VALUES (%s, %s, %s)
                    """ % (weatherDateId, wind, irradiance)
        elif windOK and not irradianceOK:
            query = """
                    INSERT INTO gbt_weather (weather_date_id,wind_speed)
                    VALUES (%s, %s)
                    """ % (weatherDateId, wind)
        elif not windOK and irradianceOK:
            query = """
                    INSERT INTO gbt_weather (weather_date_id,irradiance)
                    VALUES (%s, %s)
                    """ % (weatherDateId, irradiance)
        if windOK or irradianceOK:
            self.c.query(query)

    def update(self):
        """
        Looks to see what weather times need updating, then retrieves that
        data from the sampler logs, and finally writes results to DB.
        """
        
        results = []
        dts = self.getNeededWeatherDates()
        for dtId, dtStr in dts:
            dt = datetime.strptime(dtStr, "%Y-%m-%d %H:%M:%S")
            wind = self.weatherData.getHourDanaMedianSpeeds(dt)
            di   = self.pyrgeometerData.getHourMedianDownwardIrradiance(dt)
            results.append((dtId, wind, di))
            self.insert(dtId, wind, di)
        return results    

    def findNullValues(self, column):
        "Who is missing a value?"
        query = """
                SELECT gbt.id, wd.date 
                FROM gbt_weather AS gbt, weather_dates AS wd
                WHERE gbt.%s is NULL AND gbt.weather_date_id = wd.id
                """ % column
        r = self.c.query(query)
        return [(row['id'], row['date']) for row in r.dictresult()]
              
    def updateRow(self, rowId, column, value):
        """
        Updates a row in the weather table with a value.
        """
        query = """
                UPDATE gbt_weather SET %s = %s WHERE id = %d
                """ % (column, value, rowId)
        self.c.query(query)

    def backfill(self, column, callback):
        """
        Generic method for looking for null values in the weather table,
        and updating those rows with the appropriate value from the 
        sampler logs.
        """

        results = []
        missing = self.findNullValues(column)
        for id, dtStr in missing:
            dt = datetime.strptime(dtStr, "%Y-%m-%d %H:%M:%S")
            v = callback(dt)
            # watch for NaN values
            if v and v == v:
                results.append((id, dtStr, v))
                self.updateRow(id, column, v)
        return results 
   
    def backfillWind(self):
        return self.backfill("wind_speed"
                           , self.weatherData.getLastHourMedianWindSpeeds)
    
    def backfillIrradiance(self):
        return self.backfill("irradiance"
                   , self.pyrgeometerData.getLastHourMedianDownwardIrradiance)
    

    def backfillReport(self, filename):
        "Backfills the DB, and creates report on results."

        # NOTE: current results for this method: using the weather or
        # or weather_unit_test DB's, since we only have 2006 & 2009 - present
        # data in those, and there is no 2006 pygeometer data, this method
        # only backfills in 2009 - present

        f = open(filename, 'w')
        lines = []
        lines.append("Irradiance\n")
        lines.append("Start (ET): %s\n" % datetime.now())
        # TBF: just back fill irradiance right now
        results = self.backfillIrradiance()
        for r in results:
            lines.append("%s,%s,%s\n" % (r[0], r[1], r[2]))
        lines.append("End (ET): %s\n" % datetime.now())
        f.writelines(lines)    
        f.close()    
        print "printed report to: ", filename

if __name__ == "__main__":
    print "Reading gbt weather data for database", sys.argv[1]
    w = Weather2DBImport(sys.argv[1])
    w.update()
