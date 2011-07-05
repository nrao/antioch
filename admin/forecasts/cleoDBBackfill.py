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

import sys, traceback
from datetime      import datetime, timedelta
from CleoDBImport  import CleoDBImport

if len(sys.argv) != 5:
    print "Usage: python cleoDBBackfill.py <database> <start date (yyyy-mm-dd)> <start hour> <duration days>"
    exit(1)

database = sys.argv[1]
start = datetime.strptime(sys.argv[2], "%Y-%m-%d") + timedelta(hours = int(sys.argv[3]))
end = start + timedelta(days = int(sys.argv[4]))
print start, end

delta = timedelta(hours = 6)

while start < end:
    print "    ", start
    try:
        CleoDBImport(start, database, ".", True).performImport()
    except:
        t, v, tb = sys.exc_info()
        traceback.print_exception(t, v, tb)
        print "Continuing on to next forecast time ..."
    start += delta
