import sys
import pg

if len(sys.argv) != 2:
    print >>sys.stderr, "Usage: python populateClimateTables.py <database name>"
    exit()

database = sys.argv[1]
filepath = '/home/dss/data/simulateFreq_2004-2007.txt'

c = pg.connect(user   = "dss", dbname = database)
if c == None:
    raise "Failed to connect to database."

for line in open(filepath, 'r'):
    fields = line.split()
    f = str(int(float(fields[0])))
    e = str(int(float(fields[1])))
    tsystotal  = fields[3]
    tsysprime  = fields[4]
    stringency = fields[10]

    c.query("""INSERT 
               INTO stringency (frequency, elevation, total)
               VALUES (%s, %s, %s)""" % (f, e, stringency))
    c.query("""INSERT 
               INTO t_sys (frequency, elevation, total, prime)
               VALUES (%s, %s, %s, %s)""" % (f, e, tsystotal, tsysprime))

