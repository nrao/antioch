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

class WeatherHealth:

    """
    As the name implies, this class is responsible for checking/maintaining the health of a 
    Weather database.  It includes an entry point, from which, checks can be
    made and a report produced.  One may want to run these checks after
    updating a DB or producing a new one.  Checks include:
       * bad values
       * missing forecast times
       * missing forecast weather dates
       * missing gbt weather dates
       * and more!
    There are also methods available for improving the health of a DB: 
       * cleaning up unsused dates
       * filling gaps of various sizes
       * use w/ caution!
    """

    def __init__(self, dbname, filename = None):

        self.dtFormat = "%Y-%m-%d %H:%M:%S"

        self.dbname = dbname # ex: "weather"

        self.cnn = pg.connect(user = "dss", dbname = dbname, port = settings.DATABASE_PORT)
        
        self.sixHourForecastStart = datetime(2007, 10, 5)

        # reports
        self.quiet = False
        self.filename = filename
        self.reportLines = []
        self.missingForecastTimes = []
        self.missingWeatherDates = []
        self.missingGbtWeatherDates = []
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
        self.checkMissingGbtWeatherDates()
        rows = self.getAllForecastTimes()
        fts = [datetime.strptime(r['date'], self.dtFormat) for r in rows]
        for ft in fts:
            #print ft
            self.checkForecastTimeHealth(ft)

    def checkGbtWeather(self):
        "Top level for checking just the gbt_weather table"

        self.checkMissingGbtWeatherDates()
        self.checkGbtWeatherBadValues()

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

    def getAllGbtWeatherDates(self):
        query = "SELECT * FROM weather_dates ORDER BY date;"
        r = self.cnn.query(query)
        return [d for d in r.dictresult() if self.hasGbtWeather(d['id'])]

    def hasGbtWeather(self, weather_date_id):
        query = "SELECT * FROM gbt_weather WHERE weather_date_id = %d" % \
            weather_date_id
        r = self.cnn.query(query)
        return r.dictresult() != []

    def checkMissingGbtWeatherDates(self):
        rows = self.getAllGbtWeatherDates()
        for i in range(len(rows)-1):
            r1 = rows[i]
            r2 = rows[i+1]
            dt1 = datetime.strptime(r1['date'], self.dtFormat)
            dt2 = datetime.strptime(r2['date'], self.dtFormat)
            hours = ((dt2 - dt1).seconds) / (3600) + \
                    ((dt2 - dt1).days * 24)
            if hours != 1:
                td = dt2 - dt1
                tdh = int(round(24*td.days + td.seconds/3600.)) - 1
                self.missingGbtWeatherDates.append((dt1, dt2, tdh))
    
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
     
        # NOTE: we know that beforee 2005-02-02 there's a bunch of bad stuff
        # but most DBs don't start before 2006-01-01
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

    def checkGbtWeatherBadValues(self):

        #  Note: right now ALL irradicance values are NULL
        #  Once new Pyrgeometer is up, we can start checking it.
        q = """
        SELECT * FROM gbt_weather 
        WHERE
        wind_speed = NULL 
        OR
        wind_speed > 20.0
        OR 
        wind_speed < 0.0
        """
        r = self.cnn.query(q)
        self.badValues["wind_speed"] = len(r.dictresult())

    def report(self):
        "After the DB has been checked, anything interesting in the results?"

        self.add("Report: \n")

        self.add("Possible missing Forecast Times: \n")
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

        self.reportGbtWeather()

        self.add("Bad Values: \n")
        for col, count in self.badValues.items():
            self.add("Column %s has %d bad values\n" % (col, count))

        self.add("Number of forecasts missing frequencies: %d\n" % \
            len(self.missingFreqs))

        self.writeReport(self.filename)

    def reportGbtWeather(self):
        "Separate report function so we can call this alone if need be"

        self.add("Missing gbt_weather dates or weather dates with no gbt_weather entries: \n")
        for b, e, d in self.missingGbtWeatherDates:
            self.add("%s to %s, %d hours\n" % (b, e, d))

        self.add("GBT Weather Bad Values: \n")
        for col, count in self.badValues.items():
            if col in ["wind_speed"]: # Note: irradiance is still not available
                self.add("Column %s has %d bad values\n" % (col, count))

    def fillGaps(self):
        """
        Insures all weather dates have a forecast by copying forecasts
        from other parts of the database.
        Note: - modifies the database.
        """
        import_time = datetime.now().replace(microsecond = 0)
        import_id = self.addImportTime(import_time)
        print "Using %s as import_time for filling gaps, id %d." % (import_time
                                                                  , import_id)
        self.checkMissingWeatherDates()
        for b, e, d in self.missingWeatherDates:
            print "Filling gap between %s to %s for %d hours\n" % (b, e, d)
            if d > 3*24:
                # get best forecasts from last year
                self.fillLargeGap(import_id, b, e, d)
            else:
                # get best forecasts from yesterday
                self.fillSmallGap(import_id, b, e, d)

    def fillSmallGap(self, import_id, b, e, d):
        """
        Repeatedly copies the previous day's best forecasts into a "gap",
        i.e., weather_dates which have no forcasts, as a menas to 
        create reasonable forecasts across a dates and times.
        "Best prediction for tomorrow's weather is today's weather."
        Note: - modifies the database.
        """
        d24 = timedelta(days = 1)
        d1 = timedelta(hours = 1)
        # get 24-hours' worth of previous best forecasts
        first_gap = b + d1
        forecasts = []
        fhour = first_gap - d24
        while fhour < first_gap:
            forecasts.append(self.getForecast(fhour))
            fhour += d1
        # fill in missing forecasts
        whour = b + d1
        for i in range(d):
            self.setForecast(import_id, whour, forecasts[i % 24])
            whour += d1

    def fillLargeGap(self, import_id, b, e, d):
        """
        Copies the previous year's best forecasts into a "gap",
        i.e., weather_dates which have no forcasts, as a means to 
        create reasonable forecasts across a dates and times.
        Note: - modifies the database.
        """
        dyear = timedelta(hours = 365*24)
        d1 = timedelta(hours = 1)
        first_gap = b + d1
        fhour = first_gap - dyear
        whour = b + d1
        for i in range(d):
            self.setForecast(import_id, whour, self.getForecast(fhour))
            whour += d1
            fhour += d1

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
         
        numRows = len(r.dictresult())
        rNum = 0

        for row in r.dictresult():

            # print a counter
            print "Cleaning up %d of %d (%f done)." % (rNum, numRows, (100.0*(float(rNum)/float(numRows))))
            rNum += 1

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
        
        # really finally, get rid of the dates ...
        self.cleanupWeatherDates(start, end)

    def cleanupWeatherDates(self, start, end):
        query = """
        DELETE FROM weather_dates WHERE date >= '%s' 
        AND date <= '%s';
        """ % (start, end)    

        r = self.cnn.query(query)
        print "Deleted this many weather_dates: ", r


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
    wh.check()
    wh.report()
    #start = datetime(2004, 1 , 1, 0)
    #end   = datetime(2008, 12, 19, 0)
    #wh.cleanupWeatherDates(start, end)
