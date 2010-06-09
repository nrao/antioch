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
