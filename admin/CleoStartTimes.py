from datetime import datetime
import time
import os
import zipfile

def flatten(x):
    ans = []
    for i in range(len(x)):
        if isinstance(x[i],list):
            ans.extend(flatten(x[i]))
        else:
            ans.append(x[i])
    return ans

class CleoStartTimes:

    def __init__(self, dir = "/users/rmaddale/Weather/ArchiveNAM"):
        self.directory = dir

    def get(self):
        files = self.get_filenames()
        tss = map(self.get_ct, files)
        tss.sort()
        utss = self.f2(tss)
        return map(self.ts2dt, utss)

    def get_filenames(self):
        filenames = [f for f in os.listdir(self.directory) if self.match(f, ".buf")]
        zipnames  = [f for f in os.listdir(self.directory) if self.match(f, ".zip")]
        zipfilenames = []
        for z in zipnames:
            zf = zipfile.ZipFile(self.directory + "/" + z, "r")
            zipfilenames.append([zi.filename for zi in zf.infolist()])
        flattened = flatten(zipfilenames)
        return flatten(zipfilenames) + filenames

    def dt2ts(self, dt):
        # datetime to seconds (timestamp)
        return time.mktime(dt.timetuple())

    def ts2dt(self, ts):
        # seconds (timestamp) to datetime
        return datetime.utcfromtimestamp(ts)

    def match(self, fn, suffix):
        # like nam_c27_1275566400.buf?
        return fn[:8] == "nam_c27_" and fn[-4:] == suffix

    def get_ct(self, fn):
        # "nam_c27_1275566400.buf" to 1275566400
        return int(fn[8:-4])

    def f2(self, seq): 
        # http://www.peterbe.com/plog/uniqifiers-benchmark
        # order preserving
        checked = []
        for e in seq:
            if e not in checked:
                checked.append(e)
        return checked
