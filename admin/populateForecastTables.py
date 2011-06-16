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

#! /usr/bin/env python

# usage:
# populateForecastTables.py <database name> <read_directory>


from DBImport  import DBImport
import TimeAgent
from datetime import datetime, timedelta
import sys
import pg
import settings

# ================================ functions ==============================

def fill(xs, forecast, date):
    wind_speed = dbimport.correctWindSpeed(date, float(xs[3]))
    w2_wind_speed = float(xs[2])
    r = \
        c.query("SELECT type_id FROM forecast_types WHERE type = '%s'" % forecast)
    forecast_id = r.dictresult()[0]["type_id"]

    r = \
        c.query("SELECT id FROM weather_dates WHERE date = '%s'" % date)
    if len(r.dictresult()) == 0:
        c.query("INSERT INTO weather_dates (date) VALUES ('%s')" % date)
        r = \
            c.query("SELECT id FROM weather_dates WHERE date = '%s'" % date)
        weather_date_id = r.dictresult()[0]["id"]
        
        q = """INSERT
               INTO weather_station2 (weather_date_id, wind_speed)
               VALUES (%s, %s)""" % (weather_date_id
                                   , w2_wind_speed
                                     )
        c.query(q)
    else:
        weather_date_id = r.dictresult()[0]["id"]

    q = """INSERT
           INTO forecasts (forecast_type_id, weather_date_id, wind_speed)
           VALUES (%s, %s, %s)""" % (forecast_id
                                   , weather_date_id
                                   , wind_speed
                                     )
    c.query(q)
    r = c.query('SELECT id from forecasts ORDER BY id DESC LIMIT 1')
    id = r.dictresult()[0]["id"]
    for i in xrange(4, len(xs), 2):
        opacity = float(xs[i+0])
        tsys    = float(xs[i+1])

        frequency = i/2
        q = """INSERT
                   INTO forecast_by_frequency (frequency, opacity, tsys, forecast_id)
                   VALUES(%s, %s, %s, %s)""" % (frequency, opacity, tsys, id)
        c.query(q)

def processFile(filename, forecast, date):
    for line in open(filename):
        xs = line.split()
        fill(xs, forecast, date)
        date += timedelta(hours = 1)

# ================================ program ================================

dbimport = DBImport()

PREFIX       = 'simulateTime_2006_'
SUFFIXES     = ['0-11',  '12-23', '24-35', '36-47', '48-59']
STARTDATE    = datetime(2006, 1, 1)

if len(sys.argv) != 3:
    print "populateForecastTables.py <database name> <read_directory>"
    exit()

database     = sys.argv[1]
read_dir     = sys.argv[2]

c = pg.connect(user="dss", dbname=database, port=settings.DATABASE_PORT)

for suffix in SUFFIXES:
    filename = ''.join([PREFIX, suffix, '.txt'])
    readpath = ''.join([read_dir, '/', filename])
    print readpath
    processFile(readpath, suffix, STARTDATE)

c.close()
