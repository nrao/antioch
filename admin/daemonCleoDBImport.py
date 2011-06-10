from CleoDBImport  import CleoDBImport
import os
import time
from datetime import datetime

RONPATH = "/users/rmaddale/Weather/"
#RONPATH = "./"                           # for development
# Note: there is a file for each site; c27, kekn, klwb.  
# It's not clear to me why c27 is being used.
RONFILE = "TS_c27_NAM"
WEATHERDATABASE = "weather"
#WEATHERDATABASE = "weather_unit_tests"   # for development

def process(filePath):
    "Let CleoDBImport do the thing that it does."

    # get the forecast time from the contents of the filePath
    ft = parseForecastTime(open(filePath, 'r').readline())

    # use this forecast time with the import class
    cleo = CleoDBImport(ft, WEATHERDATABASE).performImport()

def parseForecastTime(line):
    """
    Converts strings like '1259928000 12:00:00 04Dec09' 
    to datetime objects
    """
    return datetime.strptime(line.split(' ', 1)[1].rstrip('\n'),
                             "%H:%M:%S %d%b%y")

filePath = RONPATH + RONFILE
previous_change = 0

print "Checking %s for inserting weather into %s" % (filePath, WEATHERDATABASE)

while True:
    # poll the file - when was it last modified?
    info = os.stat(filePath)
    if previous_change != info.st_mtime:
        process(filePath)
        # reset so we can wait for the next change
        previous_change = info.st_mtime
    time.sleep(30*60)



