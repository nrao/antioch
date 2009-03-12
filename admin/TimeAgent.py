# Copyright (C) 2006 Associated Universities, Inc. Washington DC, USA.
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

# $Id: TimeAgent.py,v 1.4 2007/06/21 14:20:47 mclark Exp $

from mx       import DateTime
import math
import slalib
import datetime
import pytz

UTC = pytz.timezone('UTC')
EST = pytz.timezone('US/Eastern')
LATE_BUFFER = DateTime.TimeDelta(0, 10, 0)
GBT_LOCATION = ('-79:50:23.423',
                '38:25:59.266',
                855.0)

"""
Contains utility methods for translating time representations.
Though internally these functions use mx.DateTime classes, all
dates and times passed to and from use the built-in module datetime.
These functions are built around mx.DateTime with a little help
from slalib for LST stuff.  The core time frame is UTC which can
be represented by either DateTime (absolute or date + time) or
DateTimeDelta (relative or dateless).  LST is always taken to be 
relative, i.e., represented by a DateTimeDelta object.  Relative
times are translated into an absolute time for "today." Translations
to and from TimeStamp (MJD and seconds since midnight as the Ygor
system represents time) are also available.
"""

def mxDT2dt(mxDT):
    "Translates from mx.DateTime to built-in module datetime."
    fields = (mxDT.year, mxDT.month, mxDT.day,
              mxDT.hour, mxDT.minute, int(mxDT.second))
    return datetime.datetime(*fields)

def dt2mxDT(dt):
    "Translates from built-in module datetime to mx.DateTime."
    fields = (dt.year, dt.month, dt.day,
              dt.hour, dt.minute, dt.second)
    return DateTime.DateTime(*fields)

def utc2est(utc):
    """
    Translates a UTC datetime sans tzinfo into the associated
    EST/EDT datetime sans tzinfo.
    """
    n_utc = utc.replace(tzinfo=UTC)
    n_est = n_utc.astimezone(EST)
    return n_est.replace(tzinfo=None)

def est2utc(est):
    """
    Translates a EST/EDT datetime sans tzinfo into the associated
    UTC datetime sans tzinfo.
    """
    n_est = EST.localize(est, is_dst=True)
    n_utc = n_est.astimezone(UTC)
    return n_utc.replace(tzinfo=None)

def deg2rad(deg):
    "Translates degrees into radians."
    return math.pi * deg / 180

def rad2deg(rad):
    "Translates radians into degrees."
    return 180.0 * rad / math.pi

def hr2rad(hrs):
    "Translates hours into radians."
    return math.pi * hrs / 12

def rad2hr(rad):
    "Translates radians into hours."
    return 12 * rad / math.pi

def dt2mjd(dt):
    "Translates from built-in module datetime to MJD."
    return dt2mxDT(dt).mjd

def dt2tlst(dt):
    "Translates from UT datetime to LST time."
    relative_hours = Absolute2RelativeLST(dt)
    lst_hours = int(relative_hours)
    fractional_minutes = (relative_hours - lst_hours)*60.0
    lst_minutes = int(fractional_minutes)
    fractional_seconds =(fractional_minutes - lst_minutes)*60.0
    lst_seconds = int(round(fractional_seconds)) % 60
    return datetime.time(lst_hours, lst_minutes, lst_seconds)

def mjd2dt(mjd):
    "Translates from MJD to built-in module datetime."
    return mxDT2dt(DateTime.DateTimeFromMJD(mjd))

def timedelta2hours(td):
    "Returns a timedelta to the nearest integer hour."
    return int(round(24*td.days + td.seconds/3600.))

def timedelta2minutes(td):
    "Returns a timedelta to the nearest integer minute."
    return int(round(24*60*td.days + td.seconds/60.))

def hour(dt):
    "Rounds datetime to nearest hour."
    hours = int(round((60.0*dt.hour + dt.minute + dt.second/3600.0)/60.0))
    return dt.replace(hour=0, minute=0, second=0, microsecond=0) + \
                                           datetime.timedelta(hours=hours)

def quarter(dt):
    "Rounds datetime to nearest 15 minutes."
    minutes = int(round(dt.minute/15.0))*15
    return dt.replace(minute=0, second=0, microsecond=0) + datetime.timedelta(minutes=minutes)

GBTLAT  = deg2rad(DateTime.DateTimeDeltaFrom(GBT_LOCATION[1]).hours)
GBTLONG = DateTime.DateTimeDeltaFrom(GBT_LOCATION[0]).hours

def Absolute2RelativeLST(absolute):
    "Returns LST as hours given UTC as a datetime."
    absolute = dt2mxDT(absolute)
    gmst = (180.0/math.pi)*slalib.sla_gmst(absolute.mjd)
    gbls = (gmst + GBTLONG)/15.0
    if gbls < 0:
        gbls = 24 + gbls
    return gbls

def RelativeLST2AbsoluteTime(lst, now = None):
    """
    Returns today's DateTime in UTC, defined as first corresponding
    time after now, from given LST in hours.
    """
    lst = DateTime.DateTimeDelta(0, lst, 0, 0)
    if now is None:
        now = DateTime.gmt()
    else:
        now = dt2mxDT(now)

    # Now's mjd at 0h
    mjd0 = int(now.mjd)

    # Convert requested LST to degrees
    requested_lst = 15*lst.hours

    # Local LMST for 0h UT in degrees
    lst0 = (180.0/math.pi)*slalib.sla_gmst(mjd0) + GBTLONG

    # LST difference between 0h UT and requested LST
    lst_offset = requested_lst - lst0

    solar_sidereal_ratio = (365.25/366.25)

    # options for solar time at 1 day sidereal intervals
    options = []
    for cycle in range(720, -1080, -360):
        solar_time = ((lst_offset-cycle)/15.0)*solar_sidereal_ratio
        mjd = mjd0 + solar_time/24
        options.append(DateTime.DateTimeFromMJD(mjd))

    # Select the time following the target time
    target = DateTime.DateTimeFromMJD(now.mjd)
    for option in options:
        if target < option:
            return mxDT2dt(option)
    return mxDT2dt(option[-1])

def TimeStamp2DateTime(mjd, secs):
    "Translates MJD integer and double seconds since midnight into a datetime."
    mxDT = DateTime.DateTimeFromMJD(mjd + secs/86400)
    return mxDT2dt(mxDT)

def DateTime2TimeStamp(dt):
    """
    Translates mx.DateTime into an integer MJD and double seconds since
    midnight tuple a la ygor's TimeStamp.
    """
    mxDT = dt2mxDT(dt)
    mjdf = mxDT.mjd
    mjd = int(mjdf)
    secs = 86400.0*(mjdf - mjd)
    return (mjd, secs)

def GbtLatitudeInRadians():
    """
    Translates GBT latitude from degrees, minutes, and seconds to radians
    """
    return GBTLAT

def dt2semester(dt):
    trimesterMonth = [None,'C','A','A','A','A','B','B','B','B','C','C','C']
    year = dt.year - 2000
    month = dt.month
    if month == 1:
        year -= 1
    return "%02d%s" % (year, trimesterMonth[month])

