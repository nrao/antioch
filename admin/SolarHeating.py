import TimeAgent
from Sun          import Sun
from datetime     import datetime, timedelta

class SolarHeating:
    """
    Computes whether the differential solar heating can occur as used in
    section 3.1.2 "Telescope Surface Observing Efficiency" of
    DS Project Note 5.2.
    """

    def __init__(self):
        self.sun = Sun()
        self.day_offset = timedelta(hours=2)
        self.night_offset = timedelta(hours=3)
        self.cache = dict()

    def isDayTime(self, dt):
        value = self.cache.get(dt)
        if value is None:
            value = self.computeDayTime(dt)
            self.cache[dt] = value
        return value

    def computeDayTime(self, dt):
        long = TimeAgent.GBTLONG
        lat = TimeAgent.rad2deg(TimeAgent.GBTLAT)

        rise, set = self.sun.sunRiseSet(dt.year, dt.month, dt.day, long, lat)
        if rise >= 24.0:
            rise -= 24.0
            rise_delta = timedelta(days=1)
        else:
            rise_delta = timedelta()
        rise_hour = int(rise)
        rise_minute = int(60*(rise - rise_hour))
        today = datetime(dt.year, dt.month, dt.day, rise_hour, rise_minute) + \
              self.day_offset + rise_delta
        if set >= 24.0:
            set -= 24.0
            set_delta = timedelta(days=1)
        else:
            set_delta = timedelta()
        set_hour = int(set)
        set_minute = int(60*(set - set_hour))
        tonight = datetime(dt.year, dt.month, dt.day, set_hour, set_minute) + \
                  self.night_offset + set_delta

        yesterday = dt - timedelta(days = 1)
        _, set = self.sun.sunRiseSet(yesterday.year, yesterday.month,
                                     yesterday.day, long, lat)
        if set >= 24.0:
            set -= 24.0
            set_delta = timedelta(days=1)
        else:
            set_delta = timedelta()
        set_hour = int(set)
        set_minute = int(60*(set - set_hour))
        lastnight = datetime(yesterday.year, yesterday.month, yesterday.day,
                             set_hour, set_minute) + \
                             self.night_offset + set_delta
        if dt < lastnight:
            return True
        elif dt < today:
            return False
        elif dt < tonight:
            return True
        else:
            return False
