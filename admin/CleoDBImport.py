from DBImport        import DBImport
from os              import listdir, system
from datetime        import datetime, timedelta
import sys
import TimeAgent
import pg

def getForecastType(hour):
    retval = int(hour) / 12 + 1
    return retval if retval < 6 else 5

class CleoDBImport:

    def __init__(self, dbname = "weather"):
        self.dbname   = dbname
        self.dbimport = DBImport()

    def mph2mps(self, speed):
        return speed / 2.237
    
    def getWeather(self):
        format = "%m/%d/%Y %H:00:00"
        start = TimeAgent.hour(datetime.utcnow())
        end   = start + timedelta(hours = 60)
        start = start.strftime(format)
        end   = end.strftime(format)
        atmo = "/home/dss/bin/forecastsCmdLine -readCaches -sites HotSprings -calculate OpacityTime TsysTime TatmTime -freqList 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 -elevTsys 90"
        print atmo
        wind = "/home/dss/bin/forecastsCmdLine -readCaches -sites Elkins Lewisburg -average -calculate GroundTime"
        system(atmo)
        print wind
        system(wind)

    def read(self, files):
        forecast_file, wind_file = files
        self.data = {}
        
        # read cleo forecast (ground)
        print 'Process cleo forecast data (ground) ...'
        f          = open(wind_file, 'r')
        lines      = f.readlines()[1:]
        windcol    = 6 if len(lines[0].split(' ')) > 10 else 2

        # Save the first timestamp for later to determine which forecast_type
        self.startstamp = TimeAgent.hour(TimeAgent.mjd2dt(float(lines[0].split(' ')[0])))
        
        for line in lines:
            row = line.split(' ')
            timestamp = TimeAgent.hour(TimeAgent.mjd2dt(float(row[0]))) #mjd
            self.data[timestamp] = {}
            # get forecasted wind speed
            # convert from mph to m/s
            self.data[timestamp]['speed'] = self.dbimport.correctWindSpeed(timestamp, self.mph2mps(float(row[windcol]))) #vCleo
        f.close()

        # read cleo forecast (atmosphere)
        print 'Process cleo forecast data (atmosphere)'
        f     = open(forecast_file, 'r')
        lines = f.readlines()
        first = lines[0].split(' ')
        lines = lines[1:]
        for line in lines:
            row = line.split(' ')
            timestamp  = TimeAgent.hour(TimeAgent.mjd2dt(float(row[0]))) #mjdt1
            if not self.data.has_key(timestamp):
                print "No wind data for %s" % timestamp
                continue
            self.data[timestamp]['tauCleo']  = []
            self.data[timestamp]['tSysCleo'] = []
            self.data[timestamp]['tAtmCleo'] = []
            for ifreq in range(50):
                #print first[ifreq+1], first[ifreq+51], first[ifreq+101]
                self.data[timestamp]['tauCleo'].append(float(row[ifreq+1]))
                self.data[timestamp]['tSysCleo'].append(float(row[ifreq+51]))
                self.data[timestamp]['tAtmCleo'].append(float(row[ifreq+101]))
        f.close()
        self.data = [(timestamp, values) for timestamp, values in self.data.items()]
        self.data.sort(key = lambda x: x[0])

    def insert(self):
        c = pg.connect(user = "dss", dbname = self.dbname)
        print "Inserting data"
        for timestamp, value in self.data:
            freq = 1
            speed = value['speed']
            if value.has_key('tauCleo') and value.has_key('tSysCleo') and value.has_key('tAtmCleo'):
                print "%s UT: Inserting weather for %s" % (datetime.utcnow().strftime("%H:%M:%S"), timestamp)
            else:
                print "Got wind but not atmosphere forecasts for %s" % timestamp
                continue
            
            td               = timestamp - self.startstamp
            forecast_type_id = getForecastType(td.days * 24. + td.seconds / 3600.)

            r = \
                c.query("SELECT id FROM weather_dates WHERE date = '%s'" % timestamp)
            if len(r.dictresult()) == 0:
                c.query("INSERT INTO weather_dates (date) VALUES ('%s')" % timestamp)
                r = \
                    c.query("SELECT id FROM weather_dates WHERE date = '%s'" % timestamp)
            weather_date_id = r.dictresult()[0]["id"]

            q = """INSERT
                   INTO forecasts (forecast_type_id, weather_date_id, wind_speed)
                   VALUES (%s, %s, %s)""" % (forecast_type_id
                                           , weather_date_id
                                           , speed
                                             )
            c.query(q)

            # Get the id of the forecast just inserted
            r  = c.query('SELECT id from forecasts ORDER BY id DESC LIMIT 1')
            id = r.dictresult()[0]["id"]
          
            for i, (tau, tAtm) in enumerate(zip(value['tauCleo']
                                              , value['tAtmCleo']
                                            )):
                freq = i + 1
                # tsys = tAtm from Cleo
                q = """INSERT
                       INTO forecast_by_frequency (frequency, opacity, tsys, forecast_id)
                       VALUES(%s, %s, %s, %s)""" % (freq, tau, tAtm, id)
                c.query(q)

        c.close()

    def importDB(self, files):
        self.read(files)
        self.insert()

if __name__ == "__main__":
    cleo = CleoDBImport()
    cleo.getWeather()
    try:
        path = sys.argv[1]
    except:
        assert False, "Please specify path to weather files."
        
    directories = [d for d in listdir(path) if "Forecasts" in d]
    directories.sort()
    f1, f2  = directories[-2:]
    files = ( 
              path + f1 + '/time_HotSprings' + f1[9:] + '.txt'
            , path + f2 + '/time_avrg'       + f2[9:] + '.txt'
            )
    print "Importing weather from the following files..."
    print "\t", files[0]
    print "\t", files[1]
    cleo.importDB(files)
       
