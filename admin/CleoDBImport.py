from DBImport        import DBImport
from os              import listdir, system
from datetime        import datetime, timedelta
import sys
import TimeAgent
import pg

MAXFORECASTHOURS = 96
FORECASTDELTA = 6
SIXDELTASTART = 1
MAXFORECASTTYPE = SIXDELTASTART + MAXFORECASTHOURS/FORECASTDELTA

# headers to be found on first line of each file
# freqFileHeader is at the bottom of this file - line too long for editor!
windFileHeader = "timeListMJD pwatTimeList_avrg smphTimeList_avrg smph75mTimeList_avrg drctTimeList_avrg presTimeList_avrg tmpcTimeList_avrg humidTimeList_avrg dwpcTimeList_avrg LCLDTimeList_avrg MCLDTimeList_avrg HCLDTimeList_avrg P0*MTimeList_avrg C0*MTimeList_avrg cfrlMaxTimeList_avrg LWDTimeList_avrg"

class CleoDBImport:

    def __init__(self, forecast_time, dbname, path = ".", history = False):
        
        # cleo forecast files get written to our current directory,
        # but we might want to override that for testing
        self.path     = path

        self.dbname        = dbname
        self.dbimport      = DBImport()
        self.forecast_time = forecast_time
        self.history       = history

        # take note of when this import is happening
        self.import_time = datetime.utcnow().replace(second = 0
                                                   , microsecond = 0)
        # for reporting and diagnostics
        self.files = {}
        self.report = []
        self.quiet = False

        self.initCleoCommandLines()

        # This is a mapping of column names in the wind file header, to the
        # name of where we store it in our internal data dictionary
        # This makes adding new quantities easy.
        self.windFileCols = [("smphTimeList_avrg", "speed_mph")
                           , ("LWDTimeList_avrg", "irradiance")
                            ]

    def initCleoCommandLines(self):
        """
        The system calls we make to cleo are rather complicated, and will
        change as what we need changes.
        """

        # here we init settings for the kind of info we're getting from the CLEO
        self.cleoCmdLine = "/home/dss/bin/forecastsCmdLine"

        # First the Atmosphere by frequency:
        # Frequencies: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 
        #freqs = range(1, 51)

        # Frequencies: 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 60 66 68 70 72 74 76 78 80 82 84 86 88 90 92 94 96 98 100 102 104 106 108 110 112 114 116 118 120
        freqs = range(2, 53)
        freqs.extend(range(54, 122, 2))          
        self.atmoFreqs = freqs
        self.numAtmoFreqs = len(freqs)
        freqStr = " ".join(["%d" % f for f in freqs])

        measurements = ["OpacityTime", "TsysTime", "TatmTime"]
        measurementsStr = " ".join(measurements)
        sites = ["HotSprings"]
        sitesStr = " ".join(sites)

        if self.history:
            historyOption = '-startTime "%s" -incrTime 1 -mimicHistorical' % \
                datetime.strftime(self.forecast_time, "%m/%d/%Y %H:%M:%S")
        else:
            historyOption = ''

        self.atmoCmdLine = "%s -readCaches -sites %s -calculate %s -freqList %s -elevTsys 90 %s" % \
            (self.cleoCmdLine, sitesStr, measurementsStr, freqStr, historyOption)

        # Then the winds, etc.
        measurements = ["GroundTime", "CloudsPrecipTime"]
        measurementsStr = " ".join(measurements)
        sites = ["Elkins", "Lewisburg"]
        sitesStr = " ".join(sites)

        self.windCmdLine = "%s -readCaches -sites %s -average -calculate %s %s" % \
            (self.cleoCmdLine, sitesStr, measurementsStr, historyOption)

    def reportLine(self, line):
        if not self.quiet:
            print line
        self.report.append(line)

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
        if timestamp >= self.forecast_time:
            td = timestamp - self.forecast_time
            return self.getForecastTypeId(td.days * 24. + td.seconds / 3600.)
        else:
            return SIXDELTASTART
        
    def getWeather(self):
        "Make actual calls to cleo to populate weather data files."

        self.reportLine("%s\n" % self.atmoCmdLine)
        system(self.atmoCmdLine)

        self.reportLine("%s\n" % self.windCmdLine)
        system(self.windCmdLine)

        # where are the files?  See the cleo help:
        # "The results of this program are a set of files that are 
        # placed in a newly-created subdirectory of the current dir."

    def read(self, forecast_file, wind_file):
        """
        Given the existence of cleo data files, parse the files into a
        data dictionary.
        """

        # init the data dictionary we will be writting to
        self.data = {}
        
        # we use a similar method for parsing each file, but there are enough
        # exceptions to warrant separate functions
        self.readWindFile(wind_file)
        self.readAtmoFile(forecast_file)

        # TBF: we are changing our dictionary to an ordered list
        # why not just create the new ordered list?
        # i.e., self.data[time index][0] = timestamp
        # or self.data[time index][1][keyword][maybe freq] = value
        self.data = [(timestamp, values) \
            for timestamp, values in self.data.items()]
        self.data.sort(key = lambda x: x[0])

    def readWindFile(self, file):
        """
        Parsing this file is straight forward: we'll need the timestamp
        and *scalar* quantities from each row.
        """

        # read cleo forecast (ground)
        print 'Process cleo forecast data (ground) ...', file
        f          = open(file, 'r')
        lines      = f.readlines()
        header     = lines[0]
        assert header.strip() == windFileHeader.strip()  
        #windcol    = lines[0].split(' ').index('smphTimeList_avrg')

        for line in lines[1:]:
            row = line.split(' ')
            timestamp = TimeAgent.hour(TimeAgent.mjd2dt(float(row[0]))) #mjd
            self.data[timestamp] = {}

            # what forecast type will this be?
            self.data[timestamp]['forecast_type_id'] = \
                self.getForecastTypeIdFromTimestamp(timestamp)

            self.readWindFileValues(header, timestamp, row)

            # TBF: we'll stop doing this eventually, but for now:
            # need to insert a corrected wind speed into the DB.
            speed_mph = self.data[timestamp]['speed_mph']
            self.data[timestamp]['speed_ms'] = \
                self.dbimport.correctWindSpeed(timestamp
                                             , self.mph2mps(speed_mph))

        f.close()

    def readWindFileValues(self, header, timestamp, row):
        "Uses the mapping of file headers to data dict to grab values"

        #Ex: self.windFileCols = [("smphTimeList_avrg", "speed_mph")]

        for colName, dataName in self.windFileCols:
            col = header.split(' ').index(colName)
            self.data[timestamp][dataName] = float(row[col])
            
    def readAtmoFile(self, file):
        """
        Parsing this file is more complicated, because each row contains
        *vector* quantities.
        """

        # read cleo forecast (atmosphere)
        print 'Process cleo forecast data (atmosphere) ... ', file
        f     = open(file, 'r')
        lines = f.readlines()
        header = lines[0]
        assert header.strip() == freqFileHeader.strip()
        first = lines[0].split(' ')
        lines = lines[1:]
        for line in lines:
            row = line.split(' ')
            timestamp  = TimeAgent.hour(TimeAgent.mjd2dt(float(row[0]))) #mjdt1
            if not self.data.has_key(timestamp):
                self.reportLine("ERROR: No wind data for %s\n" % timestamp)
                continue
            # frequencies
            self.data[timestamp]['freqs'] = []
            # OpacityTime<freq>List_HotSprings
            self.data[timestamp]['tauCleo']  = []
            # TsysTime<freq>List_HotSprings
            self.data[timestamp]['tSysCleo'] = []
            # TatmTime<freq>List_HotSprings
            self.data[timestamp]['tAtmCleo'] = []
            num = self.numAtmoFreqs
            for i in range(num):
                #print first[ifreq+1], first[ifreq+51], first[ifreq+101]
                self.data[timestamp]['freqs'].append(self.atmoFreqs[i])
                self.data[timestamp]['tauCleo'].append(float(row[i+1]))
                self.data[timestamp]['tSysCleo'].append(float(row[i+num+1]))
                self.data[timestamp]['tAtmCleo'].append(float(row[i+(num*2)+1]))
        f.close()

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
        r = self.c.query(select_query)
        if len(r.dictresult()) == 0:
            # it doesn't, insert it
            self.c.query(insert_query)
            # now get the id of what we just created
            r = self.c.query(select_query) 

        return r.dictresult()[0]["id"]
        
    def addForecastTime(self, timestamp):
        return self.addTimeToDB(timestamp, "forecast_times")

    def addImportTime(self, timestamp):
        return self.addTimeToDB(timestamp, "import_times")

    def addWeatherDate(self, timestamp):
        return self.addTimeToDB(timestamp, "weather_dates")

    def addForecast(self
                  , forecast_type_id
                  , weather_date_id
                  , forecast_time_id
                  , import_time_id
                  , value):
        """
        Intelligent insert into the forcast table from the data dictionary
        by checking forecast type and timestamp.
        """
        speed_mph  = value['speed_mph']
        speed_ms   = value['speed_ms']
        irradiance = value['irradiance']
        r = self.c.query("""SELECT wind_speed
                            FROM forecasts
                            WHERE forecast_type_id = %s and weather_date_id = %s
                         """ % (forecast_type_id, weather_date_id))

        if len(r.dictresult()) == 0:
            q = """INSERT
                   INTO forecasts (forecast_type_id, weather_date_id, forecast_time_id, import_time_id, wind_speed, wind_speed_mph, irradiance)
                   VALUES (%s, %s, %s, %s, %s, %s, %s)""" % (forecast_type_id
                                           , weather_date_id
                                           , forecast_time_id
                                           , import_time_id
                                           , speed_ms
                                           , speed_mph
                                           , irradiance
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
        for i, (freq, tau, tAtm) in enumerate(zip(value['freqs']
                                          , value['tauCleo']
                                          , value['tAtmCleo']
                                        )):
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

        self.reportLine("Inserting data for forecast %s\n" % self.forecast_time)

        #assert self.dbname != "weather"          # TBF temporary!
        self.c = pg.connect(user = "dss", dbname = self.dbname)

        # for the data we are inserting, record what forecast_time
        # this is for, and when the import was run.
        forecast_time_id = self.addForecastTime(self.forecast_time)
        import_time_id   = self.addImportTime(self.import_time)

        for timestamp, value in self.data:
            forecast_type_id = value['forecast_type_id']
            if value.has_key('tauCleo') and \
               value.has_key('tSysCleo') and \
               value.has_key('tAtmCleo'):
                if forecast_type_id is None:
                    continue

                self.reportLine("Inserting weather for %s: %5.2f, %5.2f\n" % \
                    (timestamp, value['speed_mph'], value['irradiance']))

                weather_dates_id = self.addWeatherDate(timestamp)
                forecast_id = self.addForecast(forecast_type_id
                                             , weather_dates_id
                                             , forecast_time_id
                                             , import_time_id
                                             , value)
                self.addForecastByFrequency(forecast_id, value)
            else:
                self.reportLine("ERROR: Got wind but not atmosphere forecasts for %s\n" % timestamp)

        self.c.close()

    def findForecastFiles(self):
        """
        Finds the files that we would like to import, based off the 
        assumption that they are the most recent ones in our path.
        """

        # where are the files?  See the cleo help:
        # "The results of this program are a set of files that are 
        # placed in a newly-created subdirectory of the current dir."
        # but we can override it with the path member
        print "self.path: ", self.path

        # the last two most recent dirs are the results from our two
        # commands we fired off in getWeather()
        f1, f2 = sorted([d for d in listdir(self.path) \
            if "Forecasts" in d])[-2:]

        # the frequency dependent realted stuff was written first    
        atmFile =  self.path + "/" + f1 + '/time_HotSprings' + \
            f1[9:] + '.txt'

        # then came the 'ground' or wind speed stuff     
        windFile = self.path + "/" + f2 + '/time_avrg' + f2[9:] + '.txt'
        
        # TBF: check the FileList* files in each dir for FT's.

        return atmFile, windFile

    def reportToFile(self, filename = None):

        # make the filename unique by adding the timestamp (import time)
        timeStr = datetime.strftime(self.import_time, "%Y_%m_%d_%H_%M_%S")
        if filename is None:
            filename = "CleoDBImport"
        filepath = "%s_%s.txt" % (filename, timeStr)

        # write all the report lines to a file
        f = open(filepath, 'w')
        lines = [line + "\n" for line in self.report]
        f.writelines(lines)
        # add what files were used
        fileLines = ["FILES:\n"]
        for fileType in ['atmFile', 'windFile']:
            file = self.files[fileType]
            l = "File %s: %s\n" % (fileType, file)
            fileLines.append(l)
        f.writelines(fileLines)
        f.close()

    def performImport(self):
        """
        Higher level function that performs all the steps for importing
        new forecast values into the DB:
            * call CLEO forecast commands to produce forecast files
            * reads in and parses these files into a data dict
            * inserts data dict contents into DB
        """

        self.reportLine("Performing import at %s UTC" % datetime.utcnow())

        # call cleo, and find the resulting files
        self.getWeather()
        atmFile, windFile = self.findForecastFiles()

        self.files["atmFile"] = atmFile
        self.files["windFile"] = windFile
        self.reportLine("Reading File atmFile: %s \n" % atmFile)
        self.reportLine("Reading File windFile: %s \n" % windFile)        

        # parse the files
        self.read(atmFile, windFile)

        # insert the parsed data into the DB
        self.insert()

        self.reportToFile()

# freqFileHeader on next line
freqFileHeader = "timeListMJD OpacityTime2List_HotSprings OpacityTime3List_HotSprings OpacityTime4List_HotSprings OpacityTime5List_HotSprings OpacityTime6List_HotSprings OpacityTime7List_HotSprings OpacityTime8List_HotSprings OpacityTime9List_HotSprings OpacityTime10List_HotSprings OpacityTime11List_HotSprings OpacityTime12List_HotSprings OpacityTime13List_HotSprings OpacityTime14List_HotSprings OpacityTime15List_HotSprings OpacityTime16List_HotSprings OpacityTime17List_HotSprings OpacityTime18List_HotSprings OpacityTime19List_HotSprings OpacityTime20List_HotSprings OpacityTime21List_HotSprings OpacityTime22List_HotSprings OpacityTime23List_HotSprings OpacityTime24List_HotSprings OpacityTime25List_HotSprings OpacityTime26List_HotSprings OpacityTime27List_HotSprings OpacityTime28List_HotSprings OpacityTime29List_HotSprings OpacityTime30List_HotSprings OpacityTime31List_HotSprings OpacityTime32List_HotSprings OpacityTime33List_HotSprings OpacityTime34List_HotSprings OpacityTime35List_HotSprings OpacityTime36List_HotSprings OpacityTime37List_HotSprings OpacityTime38List_HotSprings OpacityTime39List_HotSprings OpacityTime40List_HotSprings OpacityTime41List_HotSprings OpacityTime42List_HotSprings OpacityTime43List_HotSprings OpacityTime44List_HotSprings OpacityTime45List_HotSprings OpacityTime46List_HotSprings OpacityTime47List_HotSprings OpacityTime48List_HotSprings OpacityTime49List_HotSprings OpacityTime50List_HotSprings OpacityTime51List_HotSprings OpacityTime52List_HotSprings OpacityTime54List_HotSprings OpacityTime56List_HotSprings OpacityTime58List_HotSprings OpacityTime60List_HotSprings OpacityTime62List_HotSprings OpacityTime64List_HotSprings OpacityTime66List_HotSprings OpacityTime68List_HotSprings OpacityTime70List_HotSprings OpacityTime72List_HotSprings OpacityTime74List_HotSprings OpacityTime76List_HotSprings OpacityTime78List_HotSprings OpacityTime80List_HotSprings OpacityTime82List_HotSprings OpacityTime84List_HotSprings OpacityTime86List_HotSprings OpacityTime88List_HotSprings OpacityTime90List_HotSprings OpacityTime92List_HotSprings OpacityTime94List_HotSprings OpacityTime96List_HotSprings OpacityTime98List_HotSprings OpacityTime100List_HotSprings OpacityTime102List_HotSprings OpacityTime104List_HotSprings OpacityTime106List_HotSprings OpacityTime108List_HotSprings OpacityTime110List_HotSprings OpacityTime112List_HotSprings OpacityTime114List_HotSprings OpacityTime116List_HotSprings OpacityTime118List_HotSprings OpacityTime120List_HotSprings TsysTime2List_HotSprings TsysTime3List_HotSprings TsysTime4List_HotSprings TsysTime5List_HotSprings TsysTime6List_HotSprings TsysTime7List_HotSprings TsysTime8List_HotSprings TsysTime9List_HotSprings TsysTime10List_HotSprings TsysTime11List_HotSprings TsysTime12List_HotSprings TsysTime13List_HotSprings TsysTime14List_HotSprings TsysTime15List_HotSprings TsysTime16List_HotSprings TsysTime17List_HotSprings TsysTime18List_HotSprings TsysTime19List_HotSprings TsysTime20List_HotSprings TsysTime21List_HotSprings TsysTime22List_HotSprings TsysTime23List_HotSprings TsysTime24List_HotSprings TsysTime25List_HotSprings TsysTime26List_HotSprings TsysTime27List_HotSprings TsysTime28List_HotSprings TsysTime29List_HotSprings TsysTime30List_HotSprings TsysTime31List_HotSprings TsysTime32List_HotSprings TsysTime33List_HotSprings TsysTime34List_HotSprings TsysTime35List_HotSprings TsysTime36List_HotSprings TsysTime37List_HotSprings TsysTime38List_HotSprings TsysTime39List_HotSprings TsysTime40List_HotSprings TsysTime41List_HotSprings TsysTime42List_HotSprings TsysTime43List_HotSprings TsysTime44List_HotSprings TsysTime45List_HotSprings TsysTime46List_HotSprings TsysTime47List_HotSprings TsysTime48List_HotSprings TsysTime49List_HotSprings TsysTime50List_HotSprings TsysTime51List_HotSprings TsysTime52List_HotSprings TsysTime54List_HotSprings TsysTime56List_HotSprings TsysTime58List_HotSprings TsysTime60List_HotSprings TsysTime62List_HotSprings TsysTime64List_HotSprings TsysTime66List_HotSprings TsysTime68List_HotSprings TsysTime70List_HotSprings TsysTime72List_HotSprings TsysTime74List_HotSprings TsysTime76List_HotSprings TsysTime78List_HotSprings TsysTime80List_HotSprings TsysTime82List_HotSprings TsysTime84List_HotSprings TsysTime86List_HotSprings TsysTime88List_HotSprings TsysTime90List_HotSprings TsysTime92List_HotSprings TsysTime94List_HotSprings TsysTime96List_HotSprings TsysTime98List_HotSprings TsysTime100List_HotSprings TsysTime102List_HotSprings TsysTime104List_HotSprings TsysTime106List_HotSprings TsysTime108List_HotSprings TsysTime110List_HotSprings TsysTime112List_HotSprings TsysTime114List_HotSprings TsysTime116List_HotSprings TsysTime118List_HotSprings TsysTime120List_HotSprings TatmTime2List_HotSprings TatmTime3List_HotSprings TatmTime4List_HotSprings TatmTime5List_HotSprings TatmTime6List_HotSprings TatmTime7List_HotSprings TatmTime8List_HotSprings TatmTime9List_HotSprings TatmTime10List_HotSprings TatmTime11List_HotSprings TatmTime12List_HotSprings TatmTime13List_HotSprings TatmTime14List_HotSprings TatmTime15List_HotSprings TatmTime16List_HotSprings TatmTime17List_HotSprings TatmTime18List_HotSprings TatmTime19List_HotSprings TatmTime20List_HotSprings TatmTime21List_HotSprings TatmTime22List_HotSprings TatmTime23List_HotSprings TatmTime24List_HotSprings TatmTime25List_HotSprings TatmTime26List_HotSprings TatmTime27List_HotSprings TatmTime28List_HotSprings TatmTime29List_HotSprings TatmTime30List_HotSprings TatmTime31List_HotSprings TatmTime32List_HotSprings TatmTime33List_HotSprings TatmTime34List_HotSprings TatmTime35List_HotSprings TatmTime36List_HotSprings TatmTime37List_HotSprings TatmTime38List_HotSprings TatmTime39List_HotSprings TatmTime40List_HotSprings TatmTime41List_HotSprings TatmTime42List_HotSprings TatmTime43List_HotSprings TatmTime44List_HotSprings TatmTime45List_HotSprings TatmTime46List_HotSprings TatmTime47List_HotSprings TatmTime48List_HotSprings TatmTime49List_HotSprings TatmTime50List_HotSprings TatmTime51List_HotSprings TatmTime52List_HotSprings TatmTime54List_HotSprings TatmTime56List_HotSprings TatmTime58List_HotSprings TatmTime60List_HotSprings TatmTime62List_HotSprings TatmTime64List_HotSprings TatmTime66List_HotSprings TatmTime68List_HotSprings TatmTime70List_HotSprings TatmTime72List_HotSprings TatmTime74List_HotSprings TatmTime76List_HotSprings TatmTime78List_HotSprings TatmTime80List_HotSprings TatmTime82List_HotSprings TatmTime84List_HotSprings TatmTime86List_HotSprings TatmTime88List_HotSprings TatmTime90List_HotSprings TatmTime92List_HotSprings TatmTime94List_HotSprings TatmTime96List_HotSprings TatmTime98List_HotSprings TatmTime100List_HotSprings TatmTime102List_HotSprings TatmTime104List_HotSprings TatmTime106List_HotSprings TatmTime108List_HotSprings TatmTime110List_HotSprings TatmTime112List_HotSprings TatmTime114List_HotSprings TatmTime116List_HotSprings TatmTime118List_HotSprings TatmTime120List_HotSprings"

#freqFileHeader = "timeListMJD OpacityTime1List_HotSprings OpacityTime2List_HotSprings OpacityTime3List_HotSprings OpacityTime4List_HotSprings OpacityTime5List_HotSprings OpacityTime6List_HotSprings OpacityTime7List_HotSprings OpacityTime8List_HotSprings OpacityTime9List_HotSprings OpacityTime10List_HotSprings OpacityTime11List_HotSprings OpacityTime12List_HotSprings OpacityTime13List_HotSprings OpacityTime14List_HotSprings OpacityTime15List_HotSprings OpacityTime16List_HotSprings OpacityTime17List_HotSprings OpacityTime18List_HotSprings OpacityTime19List_HotSprings OpacityTime20List_HotSprings OpacityTime21List_HotSprings OpacityTime22List_HotSprings OpacityTime23List_HotSprings OpacityTime24List_HotSprings OpacityTime25List_HotSprings OpacityTime26List_HotSprings OpacityTime27List_HotSprings OpacityTime28List_HotSprings OpacityTime29List_HotSprings OpacityTime30List_HotSprings OpacityTime31List_HotSprings OpacityTime32List_HotSprings OpacityTime33List_HotSprings OpacityTime34List_HotSprings OpacityTime35List_HotSprings OpacityTime36List_HotSprings OpacityTime37List_HotSprings OpacityTime38List_HotSprings OpacityTime39List_HotSprings OpacityTime40List_HotSprings OpacityTime41List_HotSprings OpacityTime42List_HotSprings OpacityTime43List_HotSprings OpacityTime44List_HotSprings OpacityTime45List_HotSprings OpacityTime46List_HotSprings OpacityTime47List_HotSprings OpacityTime48List_HotSprings OpacityTime49List_HotSprings OpacityTime50List_HotSprings TsysTime1List_HotSprings TsysTime2List_HotSprings TsysTime3List_HotSprings TsysTime4List_HotSprings TsysTime5List_HotSprings TsysTime6List_HotSprings TsysTime7List_HotSprings TsysTime8List_HotSprings TsysTime9List_HotSprings TsysTime10List_HotSprings TsysTime11List_HotSprings TsysTime12List_HotSprings TsysTime13List_HotSprings TsysTime14List_HotSprings TsysTime15List_HotSprings TsysTime16List_HotSprings TsysTime17List_HotSprings TsysTime18List_HotSprings TsysTime19List_HotSprings TsysTime20List_HotSprings TsysTime21List_HotSprings TsysTime22List_HotSprings TsysTime23List_HotSprings TsysTime24List_HotSprings TsysTime25List_HotSprings TsysTime26List_HotSprings TsysTime27List_HotSprings TsysTime28List_HotSprings TsysTime29List_HotSprings TsysTime30List_HotSprings TsysTime31List_HotSprings TsysTime32List_HotSprings TsysTime33List_HotSprings TsysTime34List_HotSprings TsysTime35List_HotSprings TsysTime36List_HotSprings TsysTime37List_HotSprings TsysTime38List_HotSprings TsysTime39List_HotSprings TsysTime40List_HotSprings TsysTime41List_HotSprings TsysTime42List_HotSprings TsysTime43List_HotSprings TsysTime44List_HotSprings TsysTime45List_HotSprings TsysTime46List_HotSprings TsysTime47List_HotSprings TsysTime48List_HotSprings TsysTime49List_HotSprings TsysTime50List_HotSprings TatmTime1List_HotSprings TatmTime2List_HotSprings TatmTime3List_HotSprings TatmTime4List_HotSprings TatmTime5List_HotSprings TatmTime6List_HotSprings TatmTime7List_HotSprings TatmTime8List_HotSprings TatmTime9List_HotSprings TatmTime10List_HotSprings TatmTime11List_HotSprings TatmTime12List_HotSprings TatmTime13List_HotSprings TatmTime14List_HotSprings TatmTime15List_HotSprings TatmTime16List_HotSprings TatmTime17List_HotSprings TatmTime18List_HotSprings TatmTime19List_HotSprings TatmTime20List_HotSprings TatmTime21List_HotSprings TatmTime22List_HotSprings TatmTime23List_HotSprings TatmTime24List_HotSprings TatmTime25List_HotSprings TatmTime26List_HotSprings TatmTime27List_HotSprings TatmTime28List_HotSprings TatmTime29List_HotSprings TatmTime30List_HotSprings TatmTime31List_HotSprings TatmTime32List_HotSprings TatmTime33List_HotSprings TatmTime34List_HotSprings TatmTime35List_HotSprings TatmTime36List_HotSprings TatmTime37List_HotSprings TatmTime38List_HotSprings TatmTime39List_HotSprings TatmTime40List_HotSprings TatmTime41List_HotSprings TatmTime42List_HotSprings TatmTime43List_HotSprings TatmTime44List_HotSprings TatmTime45List_HotSprings TatmTime46List_HotSprings TatmTime47List_HotSprings TatmTime48List_HotSprings TatmTime49List_HotSprings TatmTime50List_HotSprings"
