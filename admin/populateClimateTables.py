import MySQLdb as m

database = "dss_ashelton"
filepath = '/home/dss/data/simulateFreq_2004-2007.txt'

db = m.connect(host   = "trent.gb.nrao.edu",
               user   = "dss",
               passwd = "asdf5!",
               db     = database)
if db == None:
    raise "Failed to connect to receivers database."
else:
    cur = db.cursor()

for line in open(filepath, 'r'):
    fields = line.split()
    f = str(int(float(fields[0])))
    e = str(int(float(fields[1])))
    opacity  = fields[2]
    tsystotal  = fields[3]
    tsysprime  = fields[4]
    stringency = fields[10]
    cur.execute("""INSERT 
                   INTO min_weather (frequency, elevation, opacity)
                   VALUES (%s, %s, %s)""", (f, e, opacity))
    cur.execute("""INSERT 
                   INTO stringency (frequency, elevation, total)
                   VALUES (%s, %s, %s)""", (f, e, stringency))
    cur.execute("""INSERT 
                   INTO t_sys (frequency, elevation, total, prime)
                   VALUES (%s, %s, %s, %s)""", (f, e, tsystotal, tsysprime))

