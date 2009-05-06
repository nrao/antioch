import sys
import pg

if len(sys.argv) != 2:
    print >>sys.stderr, "Usage: python populateOpacityTable.py <database name>"
    exit()

database = sys.argv[1]
filepath = '/home/dss/data/OPACITYEFF3.OUT'

c = pg.connect(user   = "dss", dbname = database)
if c == None:
    raise "Failed to connect to database."

for line in open(filepath, 'r'):
    fields = line.split()
    frequency = int(float(fields[0]))
    boundaries = [f for f in fields[1:]]
    
    for declination, boundary in zip(range(-46, 91), boundaries):
        c.query("INSERT INTO hour_angle_boundaries VALUES (DEFAULT, %d, %d, %s)" % \
                   (frequency, declination, boundary))
