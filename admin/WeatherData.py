from   SamplerData     import SamplerData
from   mx              import DateTime
import numpy
from datetime import datetime, timedelta


# set some variables
X  = 0
Y1 = 1
Y2 = 2
MJD2DATETIME = (2400000.5 - 1721424.5)

class WeatherData(SamplerData):

    """
    This class uses SamplerData to retrieve wind speeds from the sampler
    2 log files and insert them into the DSS weather database.
    """

    def __init__(self):

        SamplerData.__init__(self, "Weather-Weather2-weather2")

    def getWindData(self, dates, cols, exprs):
        """
        Function to get sampler data
           dates: start/stop times year,month,day,hour,minute,second
               For example, dates = 
               ((2006, 1, 31, 0, 0, 0), (2006, 2, 5, 0, 0, 0))
           cols:  Columns in fits file. For example, cols = (0,1)
           exprs: Plot expressions.  For example, exprs = ('X','Y1')
        This will return a list with 2 or 3 arrays (e.g., mjd and windvel)
        """
        return self.GetPlotData(dates,cols,exprs)

    def getWindVelocity(self, dates):
        """
        Returns the wind velocities (m/s), with timestamps, recorded between the
        given dates.
        """
        return self.getWindData(dates, (0,1), ('X','Y1'))

    def getLastHourMedianWindSpeeds(self, now = None):

        if now is None:
            now = datetime.utcnow()

        # dates for the last hour
        start = now - timedelta(hours=1)
        dates = (start.utctimetuple()[:6], now.utctimetuple()[:6])

        # get mjd, windvel (m/s)
        mjd, wind = self.getWindVelocity(dates)

        return numpy.median(wind)
