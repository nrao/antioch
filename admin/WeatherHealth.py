import pg
import sys
from datetime import *

class WeatherHealth:

    def __init__(self, dbname, filename = None):

        self.dtFormat = "%Y-%m-%d %H:%M:%S"

        self.dbname = dbname # ex: "weather"

        self.cnn = pg.connect(user = "dss", dbname = dbname)
        
        self.sixHourForecastStart = datetime(2007, 10, 5)

        # reports
        self.quiet = False
        self.filename = filename
        self.reportLines = []
        self.missingForecastTimes = []
        self.missingWeatherDates = []
        self.forecastTimes = {}
        self.badValues = {}
        self.missingFreqs = []

    def add(self, lines):
        "For use with printing reports"
        if not self.quiet:
            print lines
        self.reportLines += lines

    def writeReport(self, filename = None):
        self.filename = filename
        if self.filename is not None:
            f = open(self.filename, 'w')
            f.writelines(self.reportLines)
            f.close()

    def check(self):
        "Top level method for checking DB health"

        self.checkForBadValues()    
        self.checkMissingForecastTimes()
        self.checkMissingWeatherDates()
        rows = self.getAllForecastTimes()
        fts = [datetime.strptime(r['date'], self.dtFormat) for r in rows]
        for ft in fts:
            #print ft
            self.checkForecastTimeHealth(ft)

    def getAllForecastTimes(self):
        query = "SELECT * FROM forecast_times ORDER BY date;"
        r = self.cnn.query(query)
        return r.dictresult()

    def getAllWeatherDates(self):
        query = "SELECT * FROM weather_dates ORDER BY date;"
        r = self.cnn.query(query)
        return [d for d in r.dictresult() if self.hasForecast(d['id'])]

    def hasForecast(self, id):
        query = "SELECT * FROM forecasts WHERE weather_date_id = %d" % id
        r = self.cnn.query(query)
        return r.dictresult() != []

    def checkMissingForecastTimes(self):
        rows = self.getAllForecastTimes()
        for i in range(len(rows)-1):
            r1 = rows[i]
            r2 = rows[i+1]
            dt1 = datetime.strptime(r1['date'], self.dtFormat)
            dt2 = datetime.strptime(r2['date'], self.dtFormat)
            hours = ((dt2 - dt1).seconds) / (60 * 60) + \
                    ((dt2 - dt1).days * 24)
           
            if hours != 6:
                self.missingForecastTimes.append((dt1, dt2))

    def checkMissingWeatherDates(self):
        rows = self.getAllWeatherDates()
        for i in range(len(rows)-1):
            r1 = rows[i]
            r2 = rows[i+1]
            id1 = r1['id']
            id2 = r2['id']
            dt1 = datetime.strptime(r1['date'], self.dtFormat)
            dt2 = datetime.strptime(r2['date'], self.dtFormat)
            hours = ((dt2 - dt1).seconds) / (3600) + \
                    ((dt2 - dt1).days * 24)
           
            if hours != 1:
                td = dt2 - dt1
                tdh = int(round(24*td.days + td.seconds/3600.)) - 1
                self.missingWeatherDates.append((dt1, dt2, tdh))

    def checkMissingForecasts(self, forecastTime):

        query = "SELECT wd.date FROM forecasts as f, forecast_times as ft, weather_dates as wd WHERE wd.id = f.weather_date_id AND ft.id = f.forecast_time_id AND ft.date = '%s' order by wd.date" % forecastTime

        self.forecastTimes.update([(forecastTime, {})])
        self.forecastTimes[forecastTime] = {}

        r = self.cnn.query(query)
        rows = r.dictresult()
        if len(rows) == 0:
            #print "MISSING any forecasts for %s" % forecastTime
            self.forecastTimes[forecastTime]["hour_span"] = 0
            return
        start =  datetime.strptime(rows[0]['date'], self.dtFormat)
        end   =  datetime.strptime(rows[-1]['date'], self.dtFormat)
        dhours = (end - start).days * 24
        shours = ((end - start).seconds) / (60 * 60)
        hours = dhours + shours
        #print "hours span: ", hours
        self.forecastTimes[forecastTime]["hour_span"] = hours 
        self.forecastTimes[forecastTime]["gaps"] = []

        firstDt = datetime.strptime(rows[0]['date'], self.dtFormat)
        if firstDt > forecastTime:
            hours = ((firstDt - forecastTime).seconds) / (60 * 60) + \
                    ((firstDt - forecastTime).days * 24)
            print "Missing %d forecasts between %s and %s" % (hours, forecastTime, firstDt)
            self.forecastTimes[forecastTime]["gaps"].append((hours, forecastTime, firstDt))

        for i in range(len(rows)-1):
            r1 = rows[i]
            r2 = rows[i+1]
            dt1 = datetime.strptime(r1['date'], self.dtFormat)
            dt2 = datetime.strptime(r2['date'], self.dtFormat)
            hours = ((dt2 - dt1).seconds) / (60 * 60) + \
                    ((dt2 - dt1).days * 24)

            if hours != 1:
                print "Missing %d forecasts between %s and %s" % (hours - 1, dt1, dt2)
                self.forecastTimes[forecastTime]["gaps"].append((hours-1, dt1, dt2))

    def checkForecastTimeHealth(self, forecastTime):
        "For a given forecast time, how does it's data look?"

        # there should be a certain number of forecasts ... (ex: 85)
        #query = "SELECT f.id FROM forecasts as f, forecast_times as ft WHERE ft.id = f.forecast_time_id AND ft.date = '%s'" % forecastTime

        #r = self.cnn.query(query)

        #print forecastTime, len(r.dictresult())

        self.checkMissingForecasts(forecastTime)

        self.checkMissingFrequencies(forecastTime)

    def checkMissingFrequencies(self, forecastTime):

        # get the forecasts for this forecast time
        q = """
        SELECT f.id 
        FROM forecasts as f, forecast_times as ft 
        WHERE f.forecast_time_id = ft.id 
           AND ft.date = '%s'
        """ % forecastTime

        r = self.cnn.query(q)

        # for each forecast, make sure they have the right number of freqs
        for row in r.dictresult():
           forecastId = int(row['id'])

           q = """
           SELECT count(ff.id) 
           FROM forecast_by_frequency as ff, forecasts as f 
           WHERE f.id = ff.forecast_id 
              AND f.id = %d
           """ % forecastId

           r = self.cnn.query(q)

           numFreqs = int(r.dictresult()[0]['count'])

           if numFreqs != 85:
               self.missingFreqs.append((forecastTime, forecastId, numFreqs))

    def checkForBadValues(self):
        "Looks for NULLs and default values from CLEO"

        # look in forecasts
        columns = ['wind_speed', 'wind_speed_mph', 'irradiance']
        for col in columns:
            query = """
            SELECT id FROM forecasts where %s = -9999 or %s = NULL
            """ % (col, col)
            r = self.cnn.query(query)
            self.badValues[col] = len(r.dictresult())

        # look in frequencies
        columns = ['opacity', 'tsys']
        for col in columns:
            query = """
            SELECT id FROM forecast_by_frequency where %s = -9999 or %s = NULL
            """ % (col, col)
            r = self.cnn.query(query)
            self.badValues[col] = len(r.dictresult())
     
        # TBF: we know that beforee 2005-02-02 there's a bunch of bad stuff
        columns = ['opacity', 'tsys']
        for col in columns:
            query = """
            SELECT ff.id 
            FROM forecast_by_frequency as ff, forecasts as f, 
                weather_dates as wd
            WHERE wd.id = f.weather_date_id 
                AND ff.forecast_id = f.id 
                AND (%s = -9999 or %s = NULL)
                AND wd.date > '2005-02-02 00:00:00'
            """ % (col, col)
            r = self.cnn.query(query)
            self.badValues[col+"_past_bad_date"] = len(r.dictresult())

    def report(self):
        "After the DB has been checked, anything interesting in the results?"

        # TBF: print this all to a file as well

        self.add("Report: \n")

        self.add("Possible missing Forecast Times: \n")
        #self.add("%s\n" % self.missingForecastTimes)
        for m in self.missingForecastTimes:
            self.add("Missing FTs between %s and %s\n" % (m[0], m[1]))
        
        dts = self.forecastTimes.keys()
        dts.sort()

        self.add("Forecast Time hour spans: \n")
        for dt in dts:
            span = self.forecastTimes[dt]["hour_span"] 
            if (span != 54 and span != 60 and dt < self.sixHourForecastStart) \
              or \
               (span not in [84] and dt >= self.sixHourForecastStart):
                # or 78?
                self.add("%s - %d\n" % (dt, span))
                
        self.add("Gaps in a forecast's weather dates: \n")
        for dt in dts:
            gaps = self.forecastTimes[dt].get("gaps", [])
            for g in gaps:
                self.add("Missing %d forecasts between %s and %s for FT %s\n" % (g[0], g[1], g[2], dt))

        self.add("Missing weather dates or weather dates with no forecasts: \n")
        for b, e, d in self.missingWeatherDates:
            self.add("%s to %s, %d hours\n" % (b, e, d))

        self.add("Bad Values: \n")
        for col, count in self.badValues.items():
            self.add("Column %s has %d bad values\n" % (col, count))

        self.add("Number of forecasts missing frequencies: %d\n" % \
            len(self.missingFreqs))

        self.writeReport(self.filename)

    def fillGaps(self):
        """
        Repeatedly copies the previous day's best forecasts into "gaps",
        i.e., weather_dates which have no forcasts, as a menas to 
        create reasonable forecasts across a dates and times.
        "Best prediction for tomorrow's weather is today's weather."
        Note: - modifies the database.
        """
        d1 = timedelta(hours = 1)
        d24 = timedelta(days = 1)
        import_time = datetime.now().replace(microsecond = 0)
        import_id = self.addImportTime(import_time)
        print "Using %s as import_time for filling gaps, id %d." % (import_time
                                                                  , import_id)
        self.checkMissingWeatherDates()
        for b, e, d in self.missingWeatherDates:
            print "Filling gap between %s to %s for %d hours\n" % (b, e, d)
            # get 24-hours' worth of previous best forecasts
            first_gap = b + d1
            forecasts = []
            hour = first_gap - d24
            while hour < first_gap:
                forecasts.append(self.getForecast(hour))
                hour += d1
            # fill in missing forecasts
            hour = b + d1
            for i in range(d):
                self.setForecast(import_id, hour, forecasts[i % 24])
                hour += d1

    def addImportTime(self, timestamp):
        return self.addTimeToDB(timestamp, "import_times")

    def addWeatherDate(self, timestamp):
        return self.addTimeToDB(timestamp, "weather_dates")

    def getForecast(self, hour):
        """
        Returns the best forecast for the given datetime. Returns:
         {'wind_speed': <float>
        , 'irradiance': <float>
        , 'wind_speed_mph': <float> 
        , 'forecast_time_id': <float>
        , 'id': <int>
        , 'weather_date_id': <int>
        , 'frequencies': [{'opacity': <float>
                         , 'frequency': <int>
                         , 'tsys': <float>
                          }, ... ]
         }
        """
        query = "SELECT id FROM weather_dates WHERE date = '%s'" % hour.strftime(self.dtFormat)
        r = self.cnn.query(query)
        w_id = r.dictresult()[0]["id"]
        query = "SELECT id, weather_date_id, forecast_time_id, wind_speed, wind_speed_mph, irradiance FROM forecasts WHERE weather_date_id = %d ORDER BY forecast_type_id" % w_id
        r = self.cnn.query(query)
        f_id = r.dictresult()[0]["id"]
        retval = r.dictresult()[0]
        query = """SELECT frequency, opacity, tsys
                   FROM forecast_by_frequency
                   WHERE forecast_id = %d
                """ % f_id
        r = self.cnn.query(query)
        retval["frequencies"] = r.dictresult()

        return retval

    def setForecast(self, import_time_id, weather_date, forecast):
        """
        Inserts the best forecast for the given import_time id and datetime
        using the provided forecast values.
        """
        w_id = self.addWeatherDate(weather_date)
        forecast_id = self.addForecast(import_time_id, w_id, forecast)
        self.addForecastByFrequency(forecast_id, forecast['frequencies'])

    def addForecast(self, import_time_id, weather_date_id, value):
        """
        Insert into the forecast table from the data dictionary.
        by checking forecast type and timestamp.
        """
        bogus_forecat_type_id = 16
        query = """INSERT
                   INTO forecasts (forecast_type_id, weather_date_id, forecast_time_id, import_time_id, wind_speed, wind_speed_mph, irradiance)
                   VALUES (%s, %s, %s, %s, %s, %s, %s)
                """ % (bogus_forecat_type_id
                     , weather_date_id
                     , value['forecast_time_id']
                     , import_time_id
                     , value['wind_speed']
                     , value['wind_speed_mph']
                     , value['irradiance']
                      )
        self.cnn.query(query)

        # Get the id of the forecast just inserted
        r  = self.cnn.query('SELECT id from forecasts ORDER BY id DESC LIMIT 1')
        return r.dictresult()[0]["id"]

    def addForecastByFrequency(self, id, values):
        """
        Insert into the forcast_by_frequency table from the
        list of data dictionaries values.
        """
        for v in values:
            q = """INSERT
                   INTO forecast_by_frequency (frequency, opacity, tsys, forecast_id)
                   VALUES(%s, %s, %s, %s)""" % (v['frequency']
                                              , v['opacity']
                                              , v['tsys']
                                              , id)
            self.cnn.query(q)

            # print the id of the forecast_by_frequency just inserted
            #print self.cnn.query('SELECT id from forecast_by_frequency ORDER BY id DESC LIMIT 1').dictresult()[0]["id"]

    def cleanUp(self, start, end):
        """
        Delete all data entries linked to forecast times between (inclusive)
        the two given dates
        Note: modifies the database
        """

        query = """
        SELECT wd.date, f.forecast_type_id 
        FROM weather_dates as wd, forecasts as f, forecast_times as ft
        WHERE ft.id = f.forecast_time_id AND  wd.id = f.weather_date_id
          AND ft.date >= '%s' AND ft.date <= '%s';
        """ % (start, end)

        r = self.cnn.query(query)

        dts = []

        for row in r.dictresult():
            dt = row['date']
            type = row['forecast_type_id']

            if dt not in dts:
                dts.append(dt)
                print "weather date: ", dt

            # cleanup frequencies
            subquery = """
            SELECT f.id
            FROM weather_dates as wd, forecasts as f
            WHERE wd.id = f.weather_date_id 
                AND wd.date = '%s' 
                AND f.forecast_type_id = %d
            """ % (dt, type)

            query = """
            DELETE FROM forecast_by_frequency
            USING forecasts
            WHERE forecast_id = forecasts.id AND forecasts.id = (%s);
            """ % subquery

            r = self.cnn.query(query)
            print "Deleted this many frequency forecasts: ", r, type

            # now delete the forecasts
            query = """
            DELETE FROM forecasts
            USING weather_dates
            WHERE weather_date_id = weather_dates.id 
               AND weather_dates.date = '%s'
               AND forecast_type_id = %d
            """ % (dt, type)

            r = self.cnn.query(query)
            print "Deleted this many forecasts: ", r, type


        # finally, delete this forecast time
        query = """
        DELETE FROM forecast_times WHERE date >= '%s' 
        AND date <= '%s';
        """ % (start, end)    

        r = self.cnn.query(query)
        print "Deleted this many forecast_times: ", r

    def addTimeToDB(self, timestamp, table):
        """
        Searches for given timestamp value in the given 
        table.  If it doesn't exist, create it.
        In any case returns it's ID.
        """

        select_query = "SELECT id FROM %s WHERE date = '%s'" % \
            (table, timestamp)
        insert_query = "INSERT INTO %s (date) VALUES ('%s')" % \
            (table, timestamp)

        # timestamp already exists?
        r = self.cnn.query(select_query)
        if len(r.dictresult()) == 0:
            # it doesn't, insert it
            self.cnn.query(insert_query)
            # now get the id of what we just created
            r = self.cnn.query(select_query) 
        else:
            print "%s time %s already exits!" % (table, timestamp)

        return r.dictresult()[0]["id"]
           

if __name__ == '__main__':
    dbname = sys.argv[1]
    print "checking health of db: ", dbname
    filename = 'weatherReport.txt'
    wh = WeatherHealth(dbname, filename = filename)
    ##wh.check()
    ##wh.report()
    wh.fillGaps()
    #start = datetime(2007, 1, 1, 0)
    #end   = datetime(2008, 2, 1, 12)
    #wh.cleanUp(start, end)
