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

from datetime import datetime, timedelta
import sys, os


if len(sys.argv) != 6:
    print "Usage: python dbcmp.py <database_1> <database_1> <forecast_time> <duration _days>"
    exit(1)

database1 = sys.argv[1]
database2 = sys.argv[2]
start = datetime.strptime(sys.argv[3], "%Y-%m-%d") + timedelta(hours = int(sys.argv[4]))
end = start + timedelta(days = int(sys.argv[5]))


delta = timedelta(hours = 6)

while start < end:
    print "    ", start
    sql = "select fty.type, fti.date, wd.date, f.wind_speed, f.wind_speed_mph from weather_dates as wd, forecasts as f, forecast_times as fti, forecast_types as fty where f.forecast_type_id = fty.type_id and f.forecast_time_id = fti.id and f.weather_date_id = wd.id and fti.date = '%s' order by wd.date;" % start

    file1 = "%s_dmp.txt" % database1
    cmd = 'psql -U dss "%s" -c "%s" >%s' % (database1, sql, file1)
    os.system(cmd)

    file2 = "%s_dmp.txt" % database2
    cmd = 'psql -U dss "%s" -c "%s" >%s' % (database2, sql, file2)
    os.system(cmd)

    os.system("tkdiff %s %s" % (file1, file2))

    start += delta

os.system ("rm -f %s %s" % (file1, file2))
