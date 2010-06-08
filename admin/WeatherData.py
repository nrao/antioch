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

    def getHourDanaMedianSpeeds(self, dt):
        """
        Compute Dana Balser's (@Registered Trademark) 
        special median of an hours worth of data, centered at the given time.
        """

        # dates centered around the given time
        start = dt - timedelta(minutes = 30)
        end   = dt + timedelta(minutes = 30)
        dates = (start.utctimetuple()[:6], end.utctimetuple()[:6])

        # get mjd, windvel (m/s)
        mjd, wind = self.getWindVelocity(dates)

        return self.danaMedian(wind)

    def danaMedian(self, data):
        """
        Compute Dana Balser's (@Registered Trademark) special median.
        See DSPN6.1.  Data is assumed to be at 1 Hz.
        """

        # TBF: a real pythonista would use list-comprehension
        # and could spell it too.
        stepSize = 20
        fraction = 0.1 # get the 90% highest median
        length = len(data)
        steps = length / stepSize
        targetIndex = int(steps - (fraction * steps))
        stepMedians = []
        for i in range(steps):
            start = i*stepSize
            stop = start + stepSize
            stepData = data[start:stop]
            stepMedians.append(numpy.median(stepData))
        stepMedians.sort()
        return stepMedians[targetIndex]

