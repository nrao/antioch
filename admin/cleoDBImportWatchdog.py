# simple enough: if there isn't entries marking an import in the recent past,
# send a warning
# TBF: someday we need to move a lot of this stuff in antioch/admin into
# nell, or some common code sharing spot.

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
    last_import = None
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
    
    print "hasRecentImport: ", ok
    return ok

def sendWarning():

    to_list = ['pmargani@nrao.edu'
             , 'ashelton@nrao.edu'
             , 'mclark@nrao.edu'
             , 'rcreager@nrao.edu'
             , 'mmccarty@nrao.edu'
             , 'rmaddale@nrao.edu'
             ]
    # TBF: more detailed comments here?
    subject = "Warning: Weather has not been imported recently"
    body = """
    Warning: Weather has not been imported recently
    """

    emailer = emailNotifier(smtp = "smtp.gb.nrao.edu"
                          , frm  = "dss@gb.nrao.edu")
    emailer.SetTo(to_list)
    emailer.SetSubject(subject)
    emailer.SetMessage(body)
    emailer.Notify()

def check():
     
    # if the DB doesn't have a recent import, start spamming.
    if not hasRecentImport():
        sendWarning()

if __name__ == "__main__":
     check()    
