import pg
import sys
from datetime import *

class WeatherHealth:

    def __init__(self, dbname, filename = None):

        self.dtFormat = "%Y-%m-%d %H:%M:%S"

        self.dbname = dbname # ex: "weather"

        self.cnn = pg.connect(user = "dss", dbname = dbname)
        
        self.sixHourForecastStart = datetime(2007, 12, 1)

        # reports
        self.quiet = False
        self.filename = filename
        self.reportLines = []
        self.missingForecastTimes = []
        self.forecastTimes = {}
        self.badValues = {}

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

        rows = self.getAllForecastTimes()
        fts = [datetime.strptime(r['date'], self.dtFormat) for r in rows]
        for ft in fts:
            print ft
            self.checkForecastTimeHealth(ft)


    def getAllForecastTimes(self):
        query = "SELECT * FROM forecast_times ORDER BY date;"
        r = self.cnn.query(query)
        return r.dictresult()

    def checkMissingForecastTimes(self):
        rows = self.getAllForecastTimes()
        for i in range(len(rows)-1):
            r1 = rows[i]
            r2 = rows[i+1]
            dt1 = datetime.strptime(r1['date'], self.dtFormat)
            dt2 = datetime.strptime(r2['date'], self.dtFormat)
            hours = ((dt2 - dt1).seconds) / (60 * 60)
            
            if hours != 6:
                #print dt1, dt2
                self.missingForecastTimes.append((dt1, dt2))

    def checkMissingForecasts(self, forecastTime):

        query = "SELECT wd.date FROM forecasts as f, forecast_times as ft, weather_dates as wd WHERE wd.id = f.weather_date_id AND ft.id = f.forecast_time_id AND ft.date = '%s' order by wd.date" % forecastTime

        #self.forecastTimes.update([(forecastTime, {})])
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
            hours = ((firstDt - forecastTime).seconds) / (60 * 60)
            #print "Missing %d forecasts between %s and %s" % ((hours -1), forecastTime, firstDt)
            self.forecastTimes[forecastTime]["gaps"].append((hours-1, forecastTime, firstDt))



        for i in range(len(rows)-1):
            r1 = rows[i]
            r2 = rows[i+1]
            dt1 = datetime.strptime(r1['date'], self.dtFormat)
            dt2 = datetime.strptime(r2['date'], self.dtFormat)
            hours = ((dt2 - dt1).seconds) / (60 * 60)

            if hours != 1:
                #print "Missing %d forecasts between %s and %s" % ((hours -1), dt1, dt2)
                self.forecastTimes[forecastTime]["gaps"].append((hours-1, dt1, dt2))
                

    def checkForecastTimeHealth(self, forecastTime):
        "For a given forecast time, how does it's data look?"

        # there should be a certain number of forecasts ... (ex: 85)
        query = "SELECT f.id FROM forecasts as f, forecast_times as ft WHERE ft.id = f.forecast_time_id AND ft.date = '%s'" % forecastTime

        r = self.cnn.query(query)

        #print forecastTime, len(r.dictresult())

        self.checkMissingForecasts(forecastTime)

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
        self.add("%s\n" % self.missingForecastTimes)
        #for m in self.missingForecastTimes:
        #    self.add("%s\n" % m)
        
        dts = self.forecastTimes.keys()
        dts.sort()

        self.add("Forecast Time hour spans: \n")
        for dt in dts:
            span = self.forecastTimes[dt]["hour_span"] 
            if span != 54 and span != 60 and dt < self.sixHourForecastStart:
                self.add("%s - %d\n" % (dt, span))
                
        self.add("Gaps in weather dates: \n")
        for dt in dts:
            gaps = self.forecastTimes[dt].get("gaps", [])
            for g in gaps:
                self.add("Missing %d forecasts between %s and %s for FT %s\n" % (g[0], g[1], g[2], dt))

        self.add("Bad Values: \n")
        for col, count in self.badValues.items():
            self.add("Column %s has %d bad values\n" % (col, count))

        self.writeReport(self.filename)

    def cleanUp(self, start, end):
        """
        Delete all data entries linked to forecast times between (inclusive)
        the two given dates
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
           

if __name__ == '__main__':
    dbname = sys.argv[1]
    print "checking health of db: ", dbname
    filename = 'weatherReport.txt'
    wh = WeatherHealth(dbname, filename = filename)
    wh.check()
    wh.report()
    #start = datetime(2007, 1, 1, 0)
    #end   = datetime(2008, 2, 1, 12)
    #wh.cleanUp(start, end)
