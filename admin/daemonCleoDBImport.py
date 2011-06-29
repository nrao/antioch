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



