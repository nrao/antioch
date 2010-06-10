import pg
from datetime import *

format = "%Y-%m-%d %H:%M:%S"

dbname = "weather"

c = pg.connect(user = "dss", dbname = dbname)

query = "SELECT * FROM forecast_times ORDER BY date;"

r = c.query(query)

rows = r.dictresult()

for i in range(len(rows)-1):
    r1 = rows[i]
    r2 = rows[i+1]
    dt1 = datetime.strptime(r1['date'], format)
    dt2 = datetime.strptime(r2['date'], format)
    hours = ((dt2 - dt1).seconds) / (60 * 60)
    
    if hours != 6:
        print dt1, dt2
c.close()
