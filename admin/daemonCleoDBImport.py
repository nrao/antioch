from CleoDBImport  import CleoDBImport
import os
import time

ronPath = "/users/rmaddale/Weather/"
# TBF: there is a file for each site; c27, kekn, klwb.  which one to use?
file = "TS_c27_NAM"
filePath = ronPath + file
info = os.stat(filePath)
previous_change = info.st_mtime

while True:
    # poll the file - when was it last modified?
    info = os.stat(filePath)
    #print info.st_ctime, info.st_mtime, info.at_time
    if previous_change != info.st_mtime:
        print "do your thing"
    else:
        print "same old, same old"
    time.sleep(10)

    # if no change, wait a while, otherwise start import

def process():
    pass

    # get the forecast time from the contents of the file

    # use this forecast time with the import class
  
