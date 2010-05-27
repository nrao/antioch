# M&C code to get sampler data
# source /home/sparrow/integration/sparrow.tcsh
from   mx                   import DateTime
import numpy
import pyfits
import dircache
import glob
import string
import sys
import pickle

# set some variables
X  = 0
Y1 = 1
Y2 = 2
MJD2DATETIME = (2400000.5 - 1721424.5)

class SamplerData:

    def __init__(self, samplerName, debug=0):

        self.debug = debug
        
        self.exprTxt = ['X','Y1', 'Y2']
        self.columnNames = []

        self.roots = ['/home/gbtlogs', '/home/archive/gbtlogs/*/*']
        dir = "/home/gbtlogs/" + samplerName
        self.SetLogDirectory(dir)
        self.ReadColumnInfo()


    def GetPlotData(self, dates, columns, expressions):
        "returns the x, y data, where x is the MJD date, and y is the data from the sampler2log file specified by the column names and their conversions"

        if self.debug:
            print "getting data for: ", dates, columns, expressions
       

        # set the FITS file columns whose data we will retrieve
        for col in columns:
            if col >= len(self.columnNames):
                raise "column indicies must be for: ", self.columnNames
        if self.debug:        
            print "data for column names: "
            for i in range(len(columns)):
                print self.columnNames[columns[i]]

        # set the conversions used
        for i in range(len(columns)):    
            self.exprTxt[i] = expressions[i]
            
        data     = [[], [], []]
        if len(self.columnNames) == 0:
            return data
        s = dates[0]
        e = dates[1]
        startDateTime = DateTime.DateTime(s[0],s[1],s[2],s[3],s[4],s[5]) #self.GetStartDateTime()
        endDateTime   = DateTime.DateTime(e[0],e[1],e[2],e[3],e[4],e[5]) #self.GetEndDateTime()
        startMJD      = startDateTime.mjd
        endMJD        = endDateTime.mjd

        # save these off
        self.startDateTime = startDateTime
        self.endDateTime = endDateTime
        
         
        logKeys = self.GetLogFilesInRange(startDateTime, endDateTime)

        i = 0

        numKeys = len(logKeys)
        
        for k in logKeys:
            f = self.logFiles[k]
            i += 1

            #msg = "\rReading Files: %d of %d" % (i, numKeys)
            #print msg,
            #sys.stdout.flush()

            # TBF: what to do about compressed files?
            hdulist = pyfits.open(f)
            hdudata = hdulist[1].data
            numRows = hdudata.size
            dmjds = hdudata.field(0)
            columnData = [ [], [], [] ]
            for i, colNo in enumerate(columns):
                columnData[i].extend( hdudata.field(columns[i]) )
            # TBF: we could do this better by using a query in the collection
            # of the data (see Pyfits manual)
            for rowNo in range(numRows):
                dmjd = dmjds[rowNo]
                if dmjd < startMJD or endMJD < dmjd: continue
                for id in range(len(columns)):
                    value = columnData[id][rowNo] 
                    data[id].append(value)
            hdulist.close()

        d2 = [numpy.array(d) for d in data]
        for d in d2:
            d.shape = (len(d),)

        return self.EvaluateExpr(d2, columns)

    def EvaluateExpr(self, data, columns):
        exprDict = globals()
        exprDict.update(numpy.__dict__)
        exprDict.update(locals())
        return [eval(self.BuildExpr(id), exprDict) for id in range(len(columns))]

    def BuildExpr(self, id):
        expr = self.GetExpr(id)
        for id in ("X", "Y1", "Y2"):
            expr = string.replace(expr, id, "data["+id+"]")
        return expr

    def GetExpr(self, id):
        return self.exprTxt[id]

    def GetLogFilesInRange(self, startDateTime, endDateTime):
        "Returns a list of paths to all the log files between the two times for the currently selected sampler."
        startText = startDateTime.Format("%Y_%m_%d_%H:%M:%S")
        endText   = endDateTime.Format("%Y_%m_%d_%H:%M:%S")
        
        length = len(self.logKeys)
        startIdx = 0
        while startIdx < length and \
              self.logKeys[startIdx] < startText:
            startIdx = startIdx + 1
        # TBF: this fails (rets 0) when called the second time in the unit tests
        # might be that numpy.max is getting called?
        #startIdx = max(0, startIdx - 1)
        if startIdx - 1 < 0:
            startIdx = 0
        else:
            startIdx -= 1

        endIdx   = startIdx
        while endIdx   < length and \
              self.logKeys[endIdx] < endText:
            endIdx   = endIdx   + 1
        # TBF: these fail second time called, like above
        #endIdxM1 = min(length - 1, endIdx)
        if length - 1 < endIdx:
            endIdxM1 = length - 1
        else:
            endIdxM1 = endIdx
        #endIdx   = min(length - 1, endIdx + 1)
        if length - 1 < endIdx + 1:
            endIdx = length - 1
        else:
            endIdxM1 = endIdx + 1

        self.noLogsMessage = ''
        if length == 0:
            self.noLogsMessage = 'No logs'
        elif startIdx == endIdxM1 == 0:
            self.noLogsMessage = 'No logs before ' + self.logKeys[startIdx]
        elif startIdx == endIdx == (length - 1):
            self.noLogsMessage = 'No logs after ' + self.logKeys[startIdx]
        else:
            self.noLogsMessage = 'No logs between ' + self.logKeys[startIdx] + " and " + self.logKeys[endIdxM1]

        return self.logKeys[startIdx:endIdx + 1]
        
    def GetStartDateTime(self):
        return self.GetDateTime().GetStart()

    def GetEndDateTime(self):
        return self.GetDateTime().GetEnd()

    def GetDateTime(self):
        return self.dateTime
    
    def ReadColumnInfo(self):
        """
        Selects the last file in the list of all files to read the column
        headers and returns the number of columns.
        """
        
        logFile = self.GetLastLogFile()
        if logFile is None:
            return 0
        hdulist = pyfits.open(logFile)
        columns = hdulist[1].columns
        self.columnNames = columns.names
        hdulist.close()
        return len(self.columnNames) 

    def GetLastLogFile(self):
        "Returns the path to the most recent file for the currently selected sampler."
        self.logKeys, self.logFiles = self.GetAllLogFiles()
        if len(self.logKeys) > 1:
            return self.logFiles[self.logKeys[-1]]
        elif len(self.logKeys) == 1:
            return self.logFiles[self.logKeys[0]]
        else:
            self.noLogsMessage = 'No logs'
            return None

    def GetAllLogFiles(self):
        "Returns a list of paths to all the files for the currently selected sampler."
        paths = { }
        
        for dir in self.GetLogDirectories():
            files = dircache.listdir(dir)
            for f in files:
                paths[f] = dir + '/' + f
        keys = paths.keys()
        keys = [k for k in keys if k.find(".fits") >= 0]
        keys.sort()
        return keys, paths

    def GetLogDirectories(self):
        "Returns a list of paths to all the directories holding the currently selected sampler."
        return self.directories
    
    def FindDirectories(self, sampler):
        if self.debug:
            print "Sampler: ", sampler
        self.logName = sampler
        targetDirectories = [ directory + '/' + sampler
                              for directory in self.roots ]
        targetDirectories.reverse()
        self.directories = []
        for directory in targetDirectories:
            self.directories += glob.glob(directory)

    def SetLogDirectory(self, newDirectory):
        "Sets the list of directories to a single entry for the currently selected sampler."
        #self.directories = [ newDirectory ]
        self.logName = newDirectory[string.rfind(newDirectory, "/")+1:]
        self.FindDirectories(self.logName)

    def GetLogFileName(self, logFilePath):
        "Given a full path to a log file, returns the name to the log file."
        return logFilePath[string.rfind(logFilePath, "/")+1:]

def main():

    sd = SamplerData("Weather-Weather2-weather2", True)

    start = (2001,11,1,0,0,0)
    end = (2001,11,8,1,0,0)
    dates = (start,end)
    cols = (0,1)
    exprs = ('X','Y1')

    data = sd.GetPlotData(dates,cols,exprs)
    print data
    print "x vs y: ", len(data[0]), len(data[1])

if __name__ == '__main__':
    main()

# Function to get sampler data
#    dates: start/stop times year,month,day,hour,minute,second
#           For example, dates = ((2006, 1, 31, 0, 0, 0), (2006, 2, 5, 0, 0, 0))
#    cols:  Columns in fits file. For example, cols = (0,1)
#    exprs: Plot expressions.  For example, exprs = ('X','Y1')
def getWindData(dates,cols,exprs):
    # get sampler data from weather station 2
    sd = SamplerData("Weather-Weather2-weather2")
    # this will return a list with two arrays (e.g., mjd and windvel)
    data = sd.GetPlotData(dates,cols,exprs)
    return data


# Function to get wind data and export
def writeWindData():
    # define start/stop times
    #start = (2004,5,1,0,0,0)
    #end = (2004,8,1,0,0,0)
    #start = (2004,7,30,0,0,0)
    #end = (2004,11,1,0,0,0)
    #start = (2004,10,30,0,0,0)
    #end = (2005,2,1,0,0,0)
    #start = (2005,1,30,0,0,0)
    #end = (2005,5,1,0,0,0)
    #start = (2005,4,30,0,0,0)
    #end = (2005,8,1,0,0,0)
    #start = (2005,7,30,0,0,0)
    #end = (2005,11,1,0,0,0)
    #start = (2005,10,25,0,0,0)
    #end = (2006,2,1,0,0,0)
    #start = (2006,1,30,0,0,0)
    #end = (2006,5,1,0,0,0)
    #start = (2006,4,30,0,0,0)
    #end = (2006,8,1,0,0,0)
    #start = (2006,7,30,0,0,0)
    #end = (2006,11,1,0,0,0)
    #start = (2006,10,30,0,0,0)
    #end = (2007,1,1,0,0,0)
    #start = (2006,12,30,0,0,0)
    #end = (2007,3,1,0,0,0)
    #start = (2007,2,30,0,0,0)
    #end = (2007,6,1,0,0,0)
    #start = (2007,5,30,0,0,0)
    #end = (2007,9,1,0,0,0)
    #start = (2007,8,30,0,0,0)
    #end = (2007,12,1,0,0,0)
    start = (2007,11,30,0,0,0)
    end = (2008,3,1,0,0,0)
    #
    #start = (2007,1,31,0,0,0)
    #end = (2007,3,1,0,0,0)
    dates = (start,end)
    # get mjd, windvel
    #cols = (0,1)
    #exprs = ('X','Y1')
    #x = getWindData(dates,cols,exprs)
    # get mjd, windvel, winddir
    cols = (0,1,2)
    exprs = ('X','Y1','Y2')
    x = getWindData(dates,cols,exprs)

    # process wind data for every hour
    m=3600
    mjd = x[0]
    windSpeed = x[1]
    windDir = x[2]
    mjd_h = mjd*24.0
    mjd_h_template = range(int(mjd_h[0])+1,int(mjd_h[len(mjd_h)-1])-1)
    mjd_template = numpy.array(mjd_h_template)/24.0
    windSpeed_median = numpy.zeros((len(mjd_template)), 'Float64')
    windSpeed_median20 = numpy.zeros((len(mjd_template)), 'Float64')
    windSpeed_median2010 = numpy.zeros((len(mjd_template)), 'Float64')
    windSpeed_avg = numpy.zeros((len(mjd_template)), 'Float64')
    windDir_median = numpy.zeros((len(mjd_template)), 'Float64')
    windDir_std = numpy.zeros((len(mjd_template)), 'Float64')
    # find the index that matches the template
    timeIndex = range(len(mjd_template))
    p = range(len(mjd_template))
    j = 0
    for i in range(len(mjd)):
        if mjd[i] >= mjd_template[j]:
            timeIndex[j] = i
            p[j] = mjd[i] - mjd_template[j]
            j = j + 1
        if j > (len(mjd_template)-1):
            break

    # calculate median wind speeds
    for i in range(len(mjd_template)):
        # check for gaps in time greater than 0.1 hours
        deltaT = (mjd[timeIndex[i]+m/2] - mjd[timeIndex[i]-m/2])*24.0
        # check for negative wind speeds
        windMin = min(windSpeed[timeIndex[i]-m/2:timeIndex[i]+m/2])
        if deltaT > 1.1 or windMin < 0.0:
            print i, deltaT, windMin
            windSpeed_median[i] = -1.0
            windSpeed_median20[i] = -1.0
            windSpeed_median2010[i] = -1.0
            windSpeed_avg[i] = -1.0
            windDir_median[i] = -1.0
            windDir_std[i] = -1.0
        else:
            # get the median value every hour (sample rate = 1 Hz)
            windSpeed_median[i] = numpy.median(windSpeed[timeIndex[i]-m/2:timeIndex[i]+m/2])
            # calculate median value every 20 seconds (sample rate = 1 Hz)
            indexRange = range(timeIndex[i]-m/2,timeIndex[i]+m/2,20)
            valueMedian = numpy.zeros((len(indexRange)), 'Float64')
            for j in range(len(indexRange)-1):
                valueMedian[j] = numpy.median(windSpeed[indexRange[j]:indexRange[j+1]])
            # get the maximum of the 20 sec median values
            windSpeed_median20[i] = max(valueMedian)
            # get the top 10 percent of the 20 sec median values
            valueMedian.sort()
            windSpeed_median2010[i] = valueMedian[int(len(valueMedian)-0.1*len(valueMedian))]
            # calculate the average wind speed (1 min at the top of the hour)
            windSpeed_avg[i] = numpy.array(windSpeed[timeIndex[i]-m/120:timeIndex[i]+m/120]).mean()
            # calculate the median wind direction
            windDir_median[i] = numpy.median(windDir[timeIndex[i]-m/2:timeIndex[i]+m/2])
            # calculate the standard deviation of the wind direction
            windDir_std[i] = numpy.array(windDir[timeIndex[i]-m/2:timeIndex[i]+m/2]).stddev()

    # data to export
    data = [mjd_template, windSpeed_median, windSpeed_median20, windSpeed_median2010, windSpeed_avg, windDir_median, windDir_std]

    # output file
    #f = open('weather2_wind.txt', 'w')
    #for i in range(len(mjd_template)):
    #    f.write("%f %f %f %f\n" % (data[0][i], data[1][i], data[2][i], data[3][i]))
    #f.close()
    # output file
    f = open('weather2_wind.txt', 'w')
    for i in range(len(mjd_template)):
        f.write("%f %f %f %f %f %f %f\n" % (data[0][i], data[1][i], data[2][i], data[3][i], data[4][i], data[5][i], data[6][i]))
    f.close()

    # code to pickle data to a file
    #print " "
    #print "Write out pickle file "
    #print " "
    #f = open('weather2_wind.pickle', 'w')
    #pickle.dump(x, f)
    #f.close()

    # Code to unpickle data from a file
    # import pickle
    # f = open('weather2_wind.pickle', 'r')
    # x = pickle.load(f)
    # f.close()
    
    return [x, data, p, timeIndex]

#
# source /home/sparrow/integration/sparrow.tcsh
# python
# from SamplerData import *
#
# x = writeWindData()
#

