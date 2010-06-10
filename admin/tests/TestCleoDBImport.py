# Copyright (C) 2009 Associated Universities, Inc. Washington DC, USA.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 675 Mass Ave Cambridge, MA 02139, USA.
#
# Correspondence concerning GBT software should be addressed as follows:
#     GBT Operations
#     National Radio Astronomy Observatory
#     P. O. Box 2
#     Green Bank, WV 24944-0002 USA

if __name__ == "__main__":
    import sys
    sys.path[1:1] = [".."]

from datetime import datetime, timedelta
from CleoDBImport import CleoDBImport
import unittest
import pg
import shutil

class TestCleoDBImport(unittest.TestCase):

    def setUp(self):
        # use a special DB because we'll be cleaning this one out everytime.
        self.dbname = "weather_import_unit_tests"
        self.forecast = datetime.utcnow().replace(hour=6, minute=0, second=0, microsecond=0)
        self.import_time = datetime.utcnow().replace(second = 0
                                                   , microsecond = 0)

    def testInit(self):
        self.cleo = CleoDBImport(self.forecast, self.dbname, "tests")

        # make sure the command lines are properly formatted
        #atmo = "/home/dss/bin/forecastsCmdLine -readCaches -sites HotSprings -calculate OpacityTime TsysTime TatmTime -freqList 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 -elevTsys 90"
        atmo = '/home/dss/bin/forecastsCmdLine -readCaches -sites HotSprings -calculate OpacityTime TsysTime TatmTime -freqList 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88 90 92 94 96 98 100 102 104 106 108 110 112 114 116 118 120 -elevTsys 90 '
        self.assertEquals(atmo, self.cleo.atmoCmdLine)

        wind = '/home/dss/bin/forecastsCmdLine -readCaches -sites Elkins Lewisburg -average -calculate GroundTime CloudsPrecipTime '
        self.assertEquals(wind, self.cleo.windCmdLine)

    def testGetForecatTypeId(self):
        self.cleo = CleoDBImport(self.forecast, self.dbname, "tests")
        self.assertEquals(1, self.cleo.getForecastTypeId(5))
        self.assertEquals(2, self.cleo.getForecastTypeId(6))
        self.assertEquals(None, self.cleo.getForecastTypeId(-1))
        self.assertEquals(None, self.cleo.getForecastTypeId(99))

    def testGetForecatTypeFromTimestamp(self):
        self.cleo = CleoDBImport(self.forecast, self.dbname, "tests")
        now = datetime.utcnow()

        dt = now.replace(hour=5, minute=0, second=0, microsecond=0)
        self.assertEquals(1, self.cleo.getForecastTypeIdFromTimestamp(dt))
        dt = now.replace(hour=6, minute=0, second=0, microsecond=0)
        self.assertEquals(1, self.cleo.getForecastTypeIdFromTimestamp(dt))
        dt = now.replace(hour=7, minute=0, second=0, microsecond=0)
        self.assertEquals(1, self.cleo.getForecastTypeIdFromTimestamp(dt))
        dt = now.replace(hour=11, minute=0, second=0, microsecond=0)
        self.assertEquals(1, self.cleo.getForecastTypeIdFromTimestamp(dt))
        dt = now.replace(hour=12, minute=0, second=0, microsecond=0)
        self.assertEquals(2, self.cleo.getForecastTypeIdFromTimestamp(dt))


        dt = self.forecast +  timedelta(days = 3, seconds = (60*60*6))
        self.assertEquals(14, self.cleo.getForecastTypeIdFromTimestamp(dt))

    def testFindForecastFiles(self):
        self.cleo = CleoDBImport(self.forecast, self.dbname, "tests")

        files = self.cleo.findForecastFiles()        
        exp = ('tests/Forecasts_09_12_07_11h40m52s/time_HotSprings_09_12_07_11h40m52s.txt'
             , 'tests/Forecasts_09_12_07_11h40m57s/time_avrg_09_12_07_11h40m57s.txt')
        self.assertEquals(exp, files)        

    def testRead(self):
        cleo = CleoDBImport(6, "")
        #cleo.forecast_time = datetime(2009, 12, 1, 6, 0, 0)
        cleo.forecast_time = datetime(2010, 6, 4, 18, 0, 0)
        cleo.read("tests/test_freq_vals.txt", "tests/test_winds.txt")

        # Ground File - wind speeds

        # First row
        #timestamp = cleo.data[0][0]    # 2009-11-30 23:00:00 UTC
        timestamp = cleo.data[0][0]    # 2010-06-04 09:00:00 UTC
        # We expect this to be the first timestamp since cleo gives
        # you a 12 hour buffer from *before* you *asked* for the forecasts
        ## And we asked for these at 2009-12-1 11:40:00 (rounded to hour)
        #expTimestamp = datetime(2009, 11, 30, 23, 0, 0)
        # And we asked for these at 2006-06-04 21:00:00 (rounded to hour)
        expTimestamp = datetime(2010, 6, 4, 9, 0, 0)
        self.assertEquals(expTimestamp, timestamp)

        # The mph wind is something you can see for yourself in the file
        wind_mph = cleo.data[0][1]['speed_mph']
        self.assertEquals(6.0145, wind_mph)
        # The rest of these we derive from the file
        wind_ms = cleo.data[0][1]['speed_ms']
        self.assertAlmostEquals(1.3041921, wind_ms, 4)  
        # Should be a really old forecast
        ftype_id = cleo.data[0][1]['forecast_type_id']
        self.assertEquals(1, ftype_id)

        # Middle row
        timestamp = cleo.data[52][0]
        expTimestamp = datetime(2010, 6, 6, 13, 0, 0)
        self.assertEquals(expTimestamp, timestamp)
        wind_mph = cleo.data[52][1]['speed_mph']
        self.assertEquals(17.181, wind_mph)
        wind_ms = cleo.data[52][1]['speed_ms']
        self.assertAlmostEquals(6.20667, wind_ms, 4)     
        ftype_id = cleo.data[52][1]['forecast_type_id']
        self.assertEquals(8, ftype_id)
        
        # Last row - for some reason this test file doesn't have 3.5 days
        # into the future of data.
        last_row = 87
        timestamp = cleo.data[last_row][0]
        expTimestamp = datetime(2010, 6, 8)
        self.assertEquals(expTimestamp, timestamp)
        wind_mph = cleo.data[last_row][1]['speed_mph']
        self.assertEquals(7.30825, wind_mph)
        wind_ms = cleo.data[last_row][1]['speed_ms']
        self.assertAlmostEquals(3.865914, wind_ms, 4)     
        ftype_id = cleo.data[last_row][1]['forecast_type_id']
        self.assertEquals(14, ftype_id)

        # Atmosphere File

        # First row
        self.assertEquals(85, len(cleo.data[0][1]['tauCleo']))
        self.assertEquals(85, len(cleo.data[0][1]['tSysCleo']))
        self.assertEquals(85, len(cleo.data[0][1]['tAtmCleo']))
        tau = cleo.data[0][1]['tauCleo'][0]  # tau @ freq[0] GHz @ 2009-11-30 23:00
        self.assertEquals(0.00782361636839, tau)
        tau = cleo.data[0][1]['tauCleo'][21]  # tau @ freq[21] GHz @ 2009-11-30 23:00
        self.assertEquals(0.316691921831, tau)
        tAtm = cleo.data[0][1]['tAtmCleo'][49] # tatm @ freq[49] GHz @ 2009-11-30 23
        self.assertEquals(267.676350343, tAtm)

        # Middle row
        row = 52
        self.assertEquals(85, len(cleo.data[row][1]['tauCleo']))
        self.assertEquals(85, len(cleo.data[row][1]['tSysCleo']))
        self.assertEquals(85, len(cleo.data[row][1]['tAtmCleo']))
        tau = cleo.data[row][1]['tauCleo'][0]  # tau @ 1 GHz @ ?
        self.assertEquals(0.00700377123625, tau)
        tau = cleo.data[row][1]['tauCleo'][21]  # tau @22 GHz @ ?
        self.assertEquals(0.261487543705, tau)
        tAtm = cleo.data[row][1]['tAtmCleo'][49] # tatm @50 GHz @ ? 
        self.assertEquals(277.080140861, tAtm)

    def truncateTables(self, cnn):

        # truncat tables of interest
        #cnn = pg.connect(user = "dss", dbname = self.dbname) #"weather_unit_tests")
        tables = ['gbt_weather'
                , 'forecast_by_frequency'
                , 'forecasts'
                , 'forecast_times'
                , 'import_times'
                , 'weather_dates']
        q = "TRUNCATE TABLE"
        for t in tables:
            q += " %s," % t
        q = q[:-1] + " CASCADE;"
        cnn.query(q)
       
        # check that these tables are empty 
        for t in tables:
            q = "SELECT * FROM %s" % t
            r = cnn.query(q)
            self.assertEquals(0, len(r.dictresult()))

    def testInsert(self):
        self.cleo = CleoDBImport(self.forecast, self.dbname, "tests")

        cnn = pg.connect(user = "dss", dbname = self.dbname) #"weather_unit_tests")
        self.truncateTables(cnn)

        # create test data
        dt = datetime(2009, 1, 22, 6, 0, 0)
        forecast_type_id = 13
        freqs     = [2, 4, 6]
        tauCleo   = [1.0, 2.0, 3.0]
        tSysCleo  = [4.0, 5.0, 6.0]
        tAtmCleo  = [7.0, 8.0, 9.0]
        speed_ms  = 10.0
        speed_mph = 11.0
        irradiance = 300.0
        dataDct = dict(forecast_type_id = forecast_type_id
                     , speed_ms         = speed_ms
                     , speed_mph        = speed_mph
                     , irradiance       = irradiance
                     , tauCleo          = tauCleo
                     , tSysCleo         = tSysCleo
                     , tAtmCleo         = tAtmCleo
                     , freqs            = freqs
                     )
        self.cleo.data = [(dt, dataDct)]             
        
        # insert the data!
        self.cleo.insert()

        # test what's in the DB!
        # first check that only one forecast time is in there
        q = "SELECT * FROM forecast_times"
        r = cnn.query(q)
        self.assertEquals(1, len(r.dictresult()))
        expDt = r.dictresult()[0]['date']
        self.assertEquals(expDt, str(self.forecast))

        # first check that only one forecast time is in there
        q = "SELECT * FROM import_times"
        r = cnn.query(q)
        self.assertEquals(1, len(r.dictresult()))
        expDt = r.dictresult()[0]['date']
        self.assertEquals(expDt, str(self.import_time))

        # first check that only one forecast time is in there
        q = "SELECT * FROM weather_dates"
        r = cnn.query(q)
        self.assertEquals(1, len(r.dictresult()))
        expDt = r.dictresult()[0]['date']
        self.assertEquals(expDt, str(dt))

        # only one entry in forecasts
        q = "SELECT * from forecasts"
        r = cnn.query(q)
        self.assertEquals(1, len(r.dictresult()))
        self.assertEquals(speed_ms, r.dictresult()[0]['wind_speed'])
        self.assertEquals(speed_mph, r.dictresult()[0]['wind_speed_mph'])

        # only three entries in forecast by frequency
        q = "SELECT * from forecast_by_frequency"
        r = cnn.query(q)
        self.assertEquals(3, len(r.dictresult()))
        for i in range(3):
            self.assertEquals(freqs[i],    r.dictresult()[i]['frequency'])
            self.assertEquals(tauCleo[i],  r.dictresult()[i]['opacity'])
            self.assertEquals(tAtmCleo[i], r.dictresult()[i]['tsys'])

        # insert the data agains
        self.cleo.insert()

        # no changes in database
        q = "SELECT * FROM weather_dates"
        r = cnn.query(q)
        self.assertEquals(1, len(r.dictresult()))
        q = "SELECT * from forecasts"
        r = cnn.query(q)
        self.assertEquals(1, len(r.dictresult()))
        q = "SELECT * from forecast_by_frequency"
        r = cnn.query(q)
        self.assertEquals(3, len(r.dictresult()))

    def testImport(self):
        self.cleo = CleoDBImport(self.forecast, self.dbname, "tests")

        # setup DB
        cnn = pg.connect(user = "dss", dbname = self.dbname) 
        self.truncateTables(cnn)

        # setup object
        self.cleo.path = "."
        self.cleo.quiet = True

        self.cleo.performImport()

        # if the import got so far as to insert data, it must have done ok.
        report = " ".join(self.cleo.report)
        self.assertTrue("Inserting data for forecast" in report)

        # clean up the files
        for type, file in self.cleo.files.items():
            dir = file.split("/")[1]
            #shutil.rmtree(dir)

    def testHistoryImport(self):
        self.cleo = CleoDBImport(self.forecast, self.dbname, "tests", True)

        # setup DB
        cnn = pg.connect(user = "dss", dbname = self.dbname) 
        self.truncateTables(cnn)

        # setup object
        self.cleo.path = "."
        self.cleo.quiet = True

        self.cleo.performImport()

        # if the import got so far as to insert data, it must have done ok.
        report = " ".join(self.cleo.report)
        self.assertTrue("Inserting data for forecast" in report)

        # clean up the files
        for type, file in self.cleo.files.items():
            dir = file.split("/")[1]
            #shutil.rmtree(dir)

if __name__ == "__main__":
    #unittest.main()
    # for more verbosity:
    suite = unittest.TestLoader().loadTestsFromTestCase(TestCleoDBImport)
    unittest.TextTestRunner(verbosity=2).run(suite)
    
