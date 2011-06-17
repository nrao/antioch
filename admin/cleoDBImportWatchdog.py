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

# simple enough: if there isn't entries marking an import in the recent past,
# send a warning
# Someday we need to move a lot of this stuff in antioch/admin into
# nell, or some common code sharing spot.
# Story: https://www.pivotaltracker.com/story/show/14223967

import pg
import settings
from datetime import datetime
from emailNotifier import emailNotifier

def hasRecentImport():

    # how recent is recent?  usually they are spaced by 6 hours, so give
    # some buffer too.
    tolerance_hours = 8.0

    dbname = "weather"
    query = "select max(date) from import_times;"
    last_import = hours_since_last_import = None
    ok = False
    
    now = datetime.utcnow().replace(second = 0
                                  , microsecond = 0)
    
    # go to the DB and find out when the most recent import was
    c = pg.connect(user = "dss", dbname = dbname, port = settings.DATABASE_PORT)
    r = c.query(query)
    print r.dictresult()
    last_import_str = r.dictresult()[0]["max"]
    if last_import_str is not None:
        last_import = datetime.strptime(last_import_str, "%Y-%m-%d %H:%M:%S")
    if last_import is not None:
        # was that recent enough?
        hours_since_last_import = (now - last_import).seconds / (60.0 * 60.0)
        if hours_since_last_import < tolerance_hours:
           ok = True
    c.close()
    
    print "hasRecentImport: ", ok, hours_since_last_import, tolerance_hours
    return (ok, hours_since_last_import, tolerance_hours)

def sendWarning(since_hrs, tolerance_hrs):

    to_list = ['pmargani@nrao.edu'
             , 'mclark@nrao.edu'
             , 'rcreager@nrao.edu'
             , 'mmccarty@nrao.edu'
             , 'rmaddale@nrao.edu'
             ]
    subject = "Warning: Weather has not been imported for %5.2f hours" % since_hrs
    body = """
    Warning: Weather has not been imported for %5.2f hours.
    Warnings sent after %5.2f hours.
    """ % (since_hrs, tolerance_hrs)

    emailer = emailNotifier(smtp = "smtp.gb.nrao.edu"
                          , frm  = "dss@gb.nrao.edu")
    emailer.SetTo(to_list)
    emailer.SetSubject(subject)
    emailer.SetMessage(body)
    emailer.Notify()

def check():
     
    # if the DB doesn't have a recent import, start spamming.
    ok, since_hrs, tolerance_hrs = hasRecentImport()
    if not ok:
        sendWarning(since_hrs, tolerance_hrs)

if __name__ == "__main__":
     check()    
