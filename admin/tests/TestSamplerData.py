if __name__ == "__main__":
    import sys
    sys.path[1:1] = [".."]

from SamplerData        import SamplerData
from datetime           import datetime, timedelta
import unittest
import nose
from   mx                   import DateTime
import os

class TestSamplerData(unittest.TestCase):

    def testSetup(self):
        sd = SamplerData("Weather-Weather2-weather2")
        self.assertTrue(len(sd.logFiles) > 0)

    def testGetData(self):

        # get an hour's worth of 1 Hz data, and make sure it's all there
        sd = SamplerData("Weather-Weather2-weather2")
        now = datetime(2006, 2, 10, 12)
        start = now - timedelta(hours = 1)
        dates = (start.utctimetuple()[:6], now.utctimetuple()[:6])
        x, y = sd.GetPlotData(dates, (0,1), ('X','Y1'))
        self.assertEquals(3600, len(x))
        self.assertEquals(3600, len(y))

    def testGetLatestData(self):
        sd = SamplerData("Weather-Weather2-weather2")
        now = datetime.utcnow()
        start = now - timedelta(hours = 1)
        s, e = (start.utctimetuple()[:6], now.utctimetuple()[:6])
        startDateTime = DateTime.DateTime(s[0],s[1],s[2],s[3],s[4],s[5])
        endDateTime   = DateTime.DateTime(e[0],e[1],e[2],e[3],e[4],e[5])
        latestKeys = sd.GetLogFilesInRange(startDateTime, endDateTime)
        self.assertTrue(len(latestKeys) > 0)

        # now just look there and see what the latest are
        dir = "/home/gbtlogs/Weather-Weather2-weather2"
        files = os.listdir(dir)
        files.sort()
        latestFile = files[-1]

        # we better have picked up the latest file
        self.assertEquals(latestFile, latestKeys[-1])

if __name__ == "__main__":
    unittest.main()
