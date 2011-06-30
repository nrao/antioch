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

from DBImport        import DBImport
from os              import listdir, system
from datetime        import datetime, timedelta
from utilities.emailNotifier   import emailNotifier
import sys
from utilities import TimeAgent
import pg
import settings

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

        freqs = range(2, 53)
        freqs.extend(range(54, 122, 2))          
        self.atmoFreqs = freqs
        self.numAtmoFreqs = len(freqs)
        freqStr = " ".join(["%d" % f for f in freqs])

        measurements = ["OpacityTime", "TsysTime", "TatmTime"]
        measurementsStr = " ".join(measurements)
        sites = ["Elkins", "HotSprings", "Lewisburg"]
        sitesStr = " ".join(sites)

        if self.history:
            historyOption = '-startTime "%s" -incrTime 1 -mimicHistorical' % \
                datetime.strftime(self.forecast_time, "%m/%d/%Y %H:%M:%S")
        else:
            historyOption = ''

        self.atmoCmdLine = "%s -sites %s -average -calculate %s -freqList %s -elevTsys 90 %s" % \
            (self.cleoCmdLine, sitesStr, measurementsStr, freqStr, historyOption)

        # Then the winds, etc.
        measurements = ["GroundTime", "CloudsPrecipTime"]
        measurementsStr = " ".join(measurements)

        self.windCmdLine = "%s -sites %s -average -calculate %s %s" % \
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
        # Note: perhaps retval < SIXDELTASTART always implies SIXDELTASTART?
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

        print self.atmoCmdLine
        system(self.atmoCmdLine)

        print self.windCmdLine
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

        # Note: we are changing our dictionary to an ordered list
        # why not just create the new ordered list?
        # i.e., self.data[time index][0] = timestamp
        # or self.data[time index][1][keyword][maybe freq] = value
        # This works, and performance isn't an issue.
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

        for line in lines[1:]:
            row = line.split(' ')
            timestamp = TimeAgent.hour(TimeAgent.mjd2dt(float(row[0]))) #mjd
            self.data[timestamp] = {}

            # what forecast type will this be?
            self.data[timestamp]['forecast_type_id'] = \
                self.getForecastTypeIdFromTimestamp(timestamp)

            self.readWindFileValues(header, timestamp, row)

            # Note: we'll stop doing this eventually, but for now:
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
            # OpacityTime<freq>List_avrg
            self.data[timestamp]['tauCleo']  = []
            # TsysTime<freq>List_avrg
            self.data[timestamp]['tSysCleo'] = []
            # TatmTime<freq>List_avrg
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
        """
        Searches for given timestamp value in the weather_dates
        table.  If it doesn't exist, create it.
        In any case returns it's ID.
        """

        # look to see if this timestamp already exists
        r = self.c.query("SELECT id FROM weather_dates WHERE date = '%s'" % timestamp)

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

        # uncomment this line if you're developing and feeling paranoid
        #assert self.dbname != "weather"          
        self.c = pg.connect(user = "dss", dbname = self.dbname, port = settings.DATABASE_PORT)

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
        atmFile =  self.path + "/" + f1 + '/time_avrg' + \
            f1[9:] + '.txt'

        # then came the 'ground' or wind speed stuff     
        windFile = self.path + "/" + f2 + '/time_avrg' + f2[9:] + '.txt'
        
        # Check the FileList* files in each dir for FT's.
        # Story: https://www.pivotaltracker.com/story/show/14224103

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

    def notify(self):
        "Sends email notification about import"

        to_list = ['pmargani@nrao.edu'
                 , 'mclark@nrao.edu'
                 , 'rcreager@nrao.edu'
                 , 'mmccarty@nrao.edu'
                 , 'koneil@nrao.edu'
                 , 'tminter@nrao.edu'
                 , 'dbalser@nrao.edu'
                 ]
        timeStr = datetime.strftime(self.import_time, "%Y_%m_%d_%H_%M_%S")
        subject = "Weather Forecasts have been imported."
        body = """
        Weather Forecasts have been imported at %s UTC.\n
        For details see most recent report at:\n
        /home/dss/release/antioch/admin/CleoDBImport_%s.txt\n
        """ % (self.import_time, timeStr)
    
        emailer = emailNotifier(smtp = "smtp.gb.nrao.edu"
                              , frm  = "dss@gb.nrao.edu")
        emailer.SetTo(to_list)
        emailer.SetSubject(subject)
        emailer.SetMessage(body)
        emailer.Notify()
        
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

        # send an email notification
        if not self.quiet:
            self.notify()

# freqFileHeader on next line
freqFileHeader = "timeListMJD OpacityTime2List_avrg OpacityTime3List_avrg OpacityTime4List_avrg OpacityTime5List_avrg OpacityTime6List_avrg OpacityTime7List_avrg OpacityTime8List_avrg OpacityTime9List_avrg OpacityTime10List_avrg OpacityTime11List_avrg OpacityTime12List_avrg OpacityTime13List_avrg OpacityTime14List_avrg OpacityTime15List_avrg OpacityTime16List_avrg OpacityTime17List_avrg OpacityTime18List_avrg OpacityTime19List_avrg OpacityTime20List_avrg OpacityTime21List_avrg OpacityTime22List_avrg OpacityTime23List_avrg OpacityTime24List_avrg OpacityTime25List_avrg OpacityTime26List_avrg OpacityTime27List_avrg OpacityTime28List_avrg OpacityTime29List_avrg OpacityTime30List_avrg OpacityTime31List_avrg OpacityTime32List_avrg OpacityTime33List_avrg OpacityTime34List_avrg OpacityTime35List_avrg OpacityTime36List_avrg OpacityTime37List_avrg OpacityTime38List_avrg OpacityTime39List_avrg OpacityTime40List_avrg OpacityTime41List_avrg OpacityTime42List_avrg OpacityTime43List_avrg OpacityTime44List_avrg OpacityTime45List_avrg OpacityTime46List_avrg OpacityTime47List_avrg OpacityTime48List_avrg OpacityTime49List_avrg OpacityTime50List_avrg OpacityTime51List_avrg OpacityTime52List_avrg OpacityTime54List_avrg OpacityTime56List_avrg OpacityTime58List_avrg OpacityTime60List_avrg OpacityTime62List_avrg OpacityTime64List_avrg OpacityTime66List_avrg OpacityTime68List_avrg OpacityTime70List_avrg OpacityTime72List_avrg OpacityTime74List_avrg OpacityTime76List_avrg OpacityTime78List_avrg OpacityTime80List_avrg OpacityTime82List_avrg OpacityTime84List_avrg OpacityTime86List_avrg OpacityTime88List_avrg OpacityTime90List_avrg OpacityTime92List_avrg OpacityTime94List_avrg OpacityTime96List_avrg OpacityTime98List_avrg OpacityTime100List_avrg OpacityTime102List_avrg OpacityTime104List_avrg OpacityTime106List_avrg OpacityTime108List_avrg OpacityTime110List_avrg OpacityTime112List_avrg OpacityTime114List_avrg OpacityTime116List_avrg OpacityTime118List_avrg OpacityTime120List_avrg TsysTime2List_avrg TsysTime3List_avrg TsysTime4List_avrg TsysTime5List_avrg TsysTime6List_avrg TsysTime7List_avrg TsysTime8List_avrg TsysTime9List_avrg TsysTime10List_avrg TsysTime11List_avrg TsysTime12List_avrg TsysTime13List_avrg TsysTime14List_avrg TsysTime15List_avrg TsysTime16List_avrg TsysTime17List_avrg TsysTime18List_avrg TsysTime19List_avrg TsysTime20List_avrg TsysTime21List_avrg TsysTime22List_avrg TsysTime23List_avrg TsysTime24List_avrg TsysTime25List_avrg TsysTime26List_avrg TsysTime27List_avrg TsysTime28List_avrg TsysTime29List_avrg TsysTime30List_avrg TsysTime31List_avrg TsysTime32List_avrg TsysTime33List_avrg TsysTime34List_avrg TsysTime35List_avrg TsysTime36List_avrg TsysTime37List_avrg TsysTime38List_avrg TsysTime39List_avrg TsysTime40List_avrg TsysTime41List_avrg TsysTime42List_avrg TsysTime43List_avrg TsysTime44List_avrg TsysTime45List_avrg TsysTime46List_avrg TsysTime47List_avrg TsysTime48List_avrg TsysTime49List_avrg TsysTime50List_avrg TsysTime51List_avrg TsysTime52List_avrg TsysTime54List_avrg TsysTime56List_avrg TsysTime58List_avrg TsysTime60List_avrg TsysTime62List_avrg TsysTime64List_avrg TsysTime66List_avrg TsysTime68List_avrg TsysTime70List_avrg TsysTime72List_avrg TsysTime74List_avrg TsysTime76List_avrg TsysTime78List_avrg TsysTime80List_avrg TsysTime82List_avrg TsysTime84List_avrg TsysTime86List_avrg TsysTime88List_avrg TsysTime90List_avrg TsysTime92List_avrg TsysTime94List_avrg TsysTime96List_avrg TsysTime98List_avrg TsysTime100List_avrg TsysTime102List_avrg TsysTime104List_avrg TsysTime106List_avrg TsysTime108List_avrg TsysTime110List_avrg TsysTime112List_avrg TsysTime114List_avrg TsysTime116List_avrg TsysTime118List_avrg TsysTime120List_avrg TatmTime2List_avrg TatmTime3List_avrg TatmTime4List_avrg TatmTime5List_avrg TatmTime6List_avrg TatmTime7List_avrg TatmTime8List_avrg TatmTime9List_avrg TatmTime10List_avrg TatmTime11List_avrg TatmTime12List_avrg TatmTime13List_avrg TatmTime14List_avrg TatmTime15List_avrg TatmTime16List_avrg TatmTime17List_avrg TatmTime18List_avrg TatmTime19List_avrg TatmTime20List_avrg TatmTime21List_avrg TatmTime22List_avrg TatmTime23List_avrg TatmTime24List_avrg TatmTime25List_avrg TatmTime26List_avrg TatmTime27List_avrg TatmTime28List_avrg TatmTime29List_avrg TatmTime30List_avrg TatmTime31List_avrg TatmTime32List_avrg TatmTime33List_avrg TatmTime34List_avrg TatmTime35List_avrg TatmTime36List_avrg TatmTime37List_avrg TatmTime38List_avrg TatmTime39List_avrg TatmTime40List_avrg TatmTime41List_avrg TatmTime42List_avrg TatmTime43List_avrg TatmTime44List_avrg TatmTime45List_avrg TatmTime46List_avrg TatmTime47List_avrg TatmTime48List_avrg TatmTime49List_avrg TatmTime50List_avrg TatmTime51List_avrg TatmTime52List_avrg TatmTime54List_avrg TatmTime56List_avrg TatmTime58List_avrg TatmTime60List_avrg TatmTime62List_avrg TatmTime64List_avrg TatmTime66List_avrg TatmTime68List_avrg TatmTime70List_avrg TatmTime72List_avrg TatmTime74List_avrg TatmTime76List_avrg TatmTime78List_avrg TatmTime80List_avrg TatmTime82List_avrg TatmTime84List_avrg TatmTime86List_avrg TatmTime88List_avrg TatmTime90List_avrg TatmTime92List_avrg TatmTime94List_avrg TatmTime96List_avrg TatmTime98List_avrg TatmTime100List_avrg TatmTime102List_avrg TatmTime104List_avrg TatmTime106List_avrg TatmTime108List_avrg TatmTime110List_avrg TatmTime112List_avrg TatmTime114List_avrg TatmTime116List_avrg TatmTime118List_avrg TatmTime120List_avrg"
