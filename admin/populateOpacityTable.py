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

import sys
import pg
import settings

if len(sys.argv) != 2:
    print >>sys.stderr, "Usage: python populateOpacityTable.py <database name>"
    exit()

database = sys.argv[1]
filepath = '/home/dss/data/OPACITYEFF3.OUT'

c = pg.connect(user   = "dss", dbname = database, port = settings.DATABASE_PORT)
if c == None:
    raise "Failed to connect to database."

for line in open(filepath, 'r'):
    fields = line.split()
    frequency = int(float(fields[0]))
    boundaries = [f for f in fields[1:]]
    
    for declination, boundary in zip(range(-46, 91), boundaries):
        c.query("INSERT INTO hour_angle_boundaries VALUES (DEFAULT, %d, %d, %s)" % \
                   (frequency, declination, boundary))
