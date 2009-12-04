from DBImport        import DBImport
from os              import listdir, system
from datetime        import datetime, timedelta
import sys
import TimeAgent
import pg

MAXFORECASTHOURS = 96
FORECASTDELTA = 6
TWELVEDELTASTART = 1
SIXDELTASTART = 9
MAXFORECASTTYPE = SIXDELTASTART + MAXFORECASTHOURS/FORECASTDELTA

# headers to be found on first line of each file
# freqFileHeader is at the bottom of this file - too long!
windFileHeader = "timeListMJD pwatTimeList_avrg smphTimeList_avrg smph75mTimeList_avrg drctTimeList_avrg presTimeList_avrg tmpcTimeList_avrg humidTimeList_avrg dwpcTimeList_avrg"

class CleoDBImport:

    def __init__(self, forecast_time, dbname):
        self.dbname   = dbname
        self.dbimport = DBImport()
        now = datetime.utcnow()
        self.startstamp = now.replace(hour=forecast_time, minute=0, second=0, microsecond=0)

    def mph2mps(self, speed):
        return speed / 2.237
    
    def getForecastTypeId(self, delta):
        """
        Translates a time difference in hours into a database-ready,
        backward-compatible integer identifier, e.g., 1-8 or 9-24 or ...
        """
        retval = int(delta) / FORECASTDELTA + SIXDELTASTART
        # TBF perhaps retval < SIXDELTASTART always implies SIXDELTASTART?
        return retval if SIXDELTASTART <= retval < MAXFORECASTTYPE else None

    def getForecastTypeIdFromTimestamp(self, timestamp):
        """
        Translates a datetime using the forecast time into a database-ready,
        backward-compatible integer identifier, e.g., 1-8 or 9-24 or ...
        """
        if timestamp >= self.startstamp:
            td = timestamp - self.startstamp
            return self.getForecastTypeId(td.days * 24. + td.seconds / 3600.)
        else:
            return SIXDELTASTART
        
    def getWeather(self):
        "Make actual calls to cleo to populate weather data files."
        atmo = "/home/dss/bin/forecastsCmdLine -readCaches -sites HotSprings -calculate OpacityTime TsysTime TatmTime -freqList 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 -elevTsys 90"
        print atmo
        system(atmo)

        wind = "/home/dss/bin/forecastsCmdLine -readCaches -sites Elkins Lewisburg -average -calculate GroundTime"
        print wind
        system(wind)

    def read(self, files):
        """
        Given the existence of cleo data files, parse the files into a
        data dictionary.
        """
        forecast_file, wind_file = files
        self.data = {}
        
        # read cleo forecast (ground)
        print 'Process cleo forecast data (ground) ...'
        f          = open(wind_file, 'r')
        lines      = f.readlines()
        header     = lines[0]
        assert header.strip() == windFileHeader.strip()
        windcol    = lines[0].split(' ').index('smphTimeList_avrg')

        for line in lines[1:]:
            row = line.split(' ')
            timestamp = TimeAgent.hour(TimeAgent.mjd2dt(float(row[0]))) #mjd
            self.data[timestamp] = {}
            # what forecast type will this be?
            self.data[timestamp]['forecast_type_id'] = \
                self.getForecastTypeIdFromTimestamp(timestamp)
            # get forecasted wind speed
            # convert from mph to m/s
            speed = float(row[windcol])
            self.data[timestamp]['speed_mph'] = speed
            self.data[timestamp]['speed_ms'] = self.dbimport.correctWindSpeed(timestamp, self.mph2mps(speed))
        f.close()

        # read cleo forecast (atmosphere)
        print 'Process cleo forecast data (atmosphere)'
        f     = open(forecast_file, 'r')
        lines = f.readlines()
        header = lines[0]
        assert header.strip() == freqFileHeader.strip()
        first = lines[0].split(' ')
        lines = lines[1:]
        for line in lines:
            row = line.split(' ')
            timestamp  = TimeAgent.hour(TimeAgent.mjd2dt(float(row[0]))) #mjdt1
            if not self.data.has_key(timestamp):
                print "No wind data for %s" % timestamp
                continue
            # OpacityTime<freq>List_HotSprings
            self.data[timestamp]['tauCleo']  = []
            # TsysTime<freq>List_HotSprings
            self.data[timestamp]['tSysCleo'] = []
            # TatmTime<freq>List_HotSprings
            self.data[timestamp]['tAtmCleo'] = []
            for ifreq in range(50):
                #print first[ifreq+1], first[ifreq+51], first[ifreq+101]
                self.data[timestamp]['tauCleo'].append(float(row[ifreq+1]))
                self.data[timestamp]['tSysCleo'].append(float(row[ifreq+51]))
                self.data[timestamp]['tAtmCleo'].append(float(row[ifreq+101]))
        f.close()

        # TBF: we are changing our dictionary to an ordered list
        # why not just create the new ordered list?
        # i.e., self.data[time index][0] = timestamp
        # or self.data[time index][1][keyword][maybe freq] = value
        self.data = [(timestamp, values) for timestamp, values in self.data.items()]
        self.data.sort(key = lambda x: x[0])

    def addWeatherDate(self, timestamp):
        """
        Searches for given timestamp value in the weather_dates
        table.  If it doesn't exist, create it.
        In any case returns it's ID.
        """

        # look to see if this timestamp already exists
        r = \
            self.c.query("SELECT id FROM weather_dates WHERE date = '%s'" % timestamp)

        if len(r.dictresult()) == 0:
            # it doesn't, insert it
            self.c.query("INSERT INTO weather_dates (date) VALUES ('%s')" % timestamp)
            # now get the id of what we just created
            r = \
                self.c.query("SELECT id FROM weather_dates WHERE date = '%s'" % timestamp)

        return r.dictresult()[0]["id"]

    def addForecast(self
                  , forecast_type_id
                  , weather_date_id
                  , value):
        """
        Intelligent insert into the forcast table from the data dictionary
        by checking forecast type and timestamp.
        """
        speed_mph = value['speed_mph']
        speed_ms = value['speed_ms']
        r = self.c.query("""SELECT wind_speed
                            FROM forecasts
                            WHERE forecast_type_id = %s and weather_date_id = %s
                         """ % (forecast_type_id, weather_date_id))

        if len(r.dictresult()) == 0:
            q = """INSERT
                   INTO forecasts (forecast_type_id, weather_date_id, wind_speed, wind_speed_mph)
                   VALUES (%s, %s, %s, %s)""" % (forecast_type_id
                                           , weather_date_id
                                           , speed_ms
                                           , speed_mph
                                             )
            self.c.query(q)

        # Get the id of the forecast just inserted
        r  = self.c.query('SELECT id from forecasts ORDER BY id DESC LIMIT 1')
        return r.dictresult()[0]["id"]

    def addForecastByFrequency(self, id, value):
        """
        Intelligent insert into the forcast_by_frequency table from the
        data dictionary
        by checking frequency and forecast id.
        """
        for i, (tau, tAtm) in enumerate(zip(value['tauCleo']
                                          , value['tAtmCleo']
                                        )):
            freq = i + 1
            r = self.c.query("""SELECT opacity, tsys
                                FROM forecast_by_frequency
                                WHERE forecast_id = %s AND frequency = %s
                             """ % (id, freq))
            if len(r.dictresult()) == 0:
                # tsys = tAtm from Cleo
                q = """INSERT
                       INTO forecast_by_frequency (frequency, opacity, tsys, forecast_id)
                       VALUES(%s, %s, %s, %s)""" % (freq, tau, tAtm, id)
                self.c.query(q)

    def insert(self):
        "From data dictionary into database."
        print "Inserting data for forecast", self.startstamp
        self.c = pg.connect(user = "dss", dbname = self.dbname)
        for timestamp, value in self.data:
            forecast_type_id = value['forecast_type_id']
            if value.has_key('tauCleo') and \
               value.has_key('tSysCleo') and \
               value.has_key('tAtmCleo'):
                if forecast_type_id is None:
                    continue

                print "%s UT: Inserting weather for %s" % \
                      (datetime.utcnow().strftime("%H:%M:%S"), timestamp)

                weather_dates_id = self.addWeatherDate(timestamp)
                forecast_id = self.addForecast(forecast_type_id
                                             , weather_dates_id
                                             , value)
                self.addForecastByFrequency(forecast_id, value)
            else:
                print "Got wind but not atmosphere forecasts for %s" % timestamp

        self.c.close()

    def importDB(self, files):
        self.read(files)
        self.insert()

if __name__ == "__main__":
    try:
        path = sys.argv[1]
    except:
        raise "Please specify path to weather files."
    try:
        forecast_time = sys.argv[2]
    except:
        raise "Please specify forecast time (0, 6, 12, or 18)."

    cleo = CleoDBImport(forecast_time)
    cleo.getWeather()
        
    f1, f2 = sorted([d for d in listdir(path) if "Forecasts" in d])[-2:]
    files  = ( 
               path + f1 + '/time_HotSprings' + f1[9:] + '.txt'
             , path + f2 + '/time_avrg'       + f2[9:] + '.txt'
             )

    print "Importing weather from the following files..."
    print "\t", files[0]
    print "\t", files[1]
    cleo.importDB(files)


    
freqFileHeader = "timeListMJD OpacityTime1List_HotSprings OpacityTime2List_HotSprings OpacityTime3List_HotSprings OpacityTime4List_HotSprings OpacityTime5List_HotSprings OpacityTime6List_HotSprings OpacityTime7List_HotSprings OpacityTime8List_HotSprings OpacityTime9List_HotSprings OpacityTime10List_HotSprings OpacityTime11List_HotSprings OpacityTime12List_HotSprings OpacityTime13List_HotSprings OpacityTime14List_HotSprings OpacityTime15List_HotSprings OpacityTime16List_HotSprings OpacityTime17List_HotSprings OpacityTime18List_HotSprings OpacityTime19List_HotSprings OpacityTime20List_HotSprings OpacityTime21List_HotSprings OpacityTime22List_HotSprings OpacityTime23List_HotSprings OpacityTime24List_HotSprings OpacityTime25List_HotSprings OpacityTime26List_HotSprings OpacityTime27List_HotSprings OpacityTime28List_HotSprings OpacityTime29List_HotSprings OpacityTime30List_HotSprings OpacityTime31List_HotSprings OpacityTime32List_HotSprings OpacityTime33List_HotSprings OpacityTime34List_HotSprings OpacityTime35List_HotSprings OpacityTime36List_HotSprings OpacityTime37List_HotSprings OpacityTime38List_HotSprings OpacityTime39List_HotSprings OpacityTime40List_HotSprings OpacityTime41List_HotSprings OpacityTime42List_HotSprings OpacityTime43List_HotSprings OpacityTime44List_HotSprings OpacityTime45List_HotSprings OpacityTime46List_HotSprings OpacityTime47List_HotSprings OpacityTime48List_HotSprings OpacityTime49List_HotSprings OpacityTime50List_HotSprings TsysTime1List_HotSprings TsysTime2List_HotSprings TsysTime3List_HotSprings TsysTime4List_HotSprings TsysTime5List_HotSprings TsysTime6List_HotSprings TsysTime7List_HotSprings TsysTime8List_HotSprings TsysTime9List_HotSprings TsysTime10List_HotSprings TsysTime11List_HotSprings TsysTime12List_HotSprings TsysTime13List_HotSprings TsysTime14List_HotSprings TsysTime15List_HotSprings TsysTime16List_HotSprings TsysTime17List_HotSprings TsysTime18List_HotSprings TsysTime19List_HotSprings TsysTime20List_HotSprings TsysTime21List_HotSprings TsysTime22List_HotSprings TsysTime23List_HotSprings TsysTime24List_HotSprings TsysTime25List_HotSprings TsysTime26List_HotSprings TsysTime27List_HotSprings TsysTime28List_HotSprings TsysTime29List_HotSprings TsysTime30List_HotSprings TsysTime31List_HotSprings TsysTime32List_HotSprings TsysTime33List_HotSprings TsysTime34List_HotSprings TsysTime35List_HotSprings TsysTime36List_HotSprings TsysTime37List_HotSprings TsysTime38List_HotSprings TsysTime39List_HotSprings TsysTime40List_HotSprings TsysTime41List_HotSprings TsysTime42List_HotSprings TsysTime43List_HotSprings TsysTime44List_HotSprings TsysTime45List_HotSprings TsysTime46List_HotSprings TsysTime47List_HotSprings TsysTime48List_HotSprings TsysTime49List_HotSprings TsysTime50List_HotSprings TatmTime1List_HotSprings TatmTime2List_HotSprings TatmTime3List_HotSprings TatmTime4List_HotSprings TatmTime5List_HotSprings TatmTime6List_HotSprings TatmTime7List_HotSprings TatmTime8List_HotSprings TatmTime9List_HotSprings TatmTime10List_HotSprings TatmTime11List_HotSprings TatmTime12List_HotSprings TatmTime13List_HotSprings TatmTime14List_HotSprings TatmTime15List_HotSprings TatmTime16List_HotSprings TatmTime17List_HotSprings TatmTime18List_HotSprings TatmTime19List_HotSprings TatmTime20List_HotSprings TatmTime21List_HotSprings TatmTime22List_HotSprings TatmTime23List_HotSprings TatmTime24List_HotSprings TatmTime25List_HotSprings TatmTime26List_HotSprings TatmTime27List_HotSprings TatmTime28List_HotSprings TatmTime29List_HotSprings TatmTime30List_HotSprings TatmTime31List_HotSprings TatmTime32List_HotSprings TatmTime33List_HotSprings TatmTime34List_HotSprings TatmTime35List_HotSprings TatmTime36List_HotSprings TatmTime37List_HotSprings TatmTime38List_HotSprings TatmTime39List_HotSprings TatmTime40List_HotSprings TatmTime41List_HotSprings TatmTime42List_HotSprings TatmTime43List_HotSprings TatmTime44List_HotSprings TatmTime45List_HotSprings TatmTime46List_HotSprings TatmTime47List_HotSprings TatmTime48List_HotSprings TatmTime49List_HotSprings TatmTime50List_HotSprings"
