# Copyright (C) 2008 Associated Universities, Inc. Washington DC, USA.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 675 Mass Ave Cambridge, MA 02139, USA.
#
# Correspondence concerning GBT software should be addressed as follows:
#     GBT Operations
#     National Radio Astronomy Observatory
#     P. O. Box 2
#     Green Bank, WV 24944-0002 USA

import smtplib
from datetime import datetime

class emailNotifier:

    """
    This class is responsible for sending emails.
    Note: it was copied over from nell, which in turn was copied
    over from something in sparrow.
    """

    def __init__(self, smtp = None, frm = None, to = None):
        
        self.SetSmtp(smtp)
        self.SetTo(to)
        self.SetFrom(frm)

        self.SetDate(None)
        self.SetSubject(None)
        self.SetMessage(None)
        self.SetText(None)
        self.failed = {}

        if self.smtp != None:
            self.server = smtplib.SMTP(self.smtp)

    def SetSmtp(self, smtp):
        self.smtp = smtp

    def InitServer(self, smtp = None):
        if smtp != None:
            self.SetSmtp(smtp)
        if self.smtp == None:
            raise 'must set smtp before initing server'
        else:
            self.server = smtplib.SMTP(self.smtp)

    def SetFrom(self, frm):
        self.frm = frm
        
    def SetTo(self, to):
        if type(to) == str:
            separator = None
            if "," in to:
                to = to.replace(" ", "")
                separator = ","
            elif ";" in to:
                to = to.replace(" ", "")
                separator = ";"
            self.to = to.split(separator)
        else:
            self.to = to
 
    def SetSubject(self, subj):
        self.subject = subj
        
    def SetDate(self, date):
        self.date = date

    def SetMessage(self, msg):
        self.msg = msg

    def SetText(self, text):
        self.text = text
 
    def GetFailures(self):
        return self.failed
        
    def Notify(self):
        "sends text to list of subscribers, where text is based off subject,date, and message body"
        weekdays = {"0" : "Sun"
                  , "1" : "Mon"
                  , "2" : "Tue"
                  , "3" : "Wed"
                  , "4" : "Thu"
                  , "5" : "Fri"
                  , "6" : "Sat"}
        months = {"1"  : "Jan"
                , "2"  : "Feb"
                , "3"  : "Mar"
                , "4"  : "Apr"
                , "5"  : "May"
                , "6"  : "Jun"
                , "7"  : "Jul"
                , "8"  : "Aug"
                , "9"  : "Sep"
                , "10" : "Oct"
                , "11" : "Nov"
                , "12" : "Dec"}

        now = datetime.today()
        self.date = "%s, %s %s %s" % (weekdays[now.strftime("%w")]
                                       , now.strftime("%d")
                                       , months[str(int(now.strftime("%m")))]
                                       , now.strftime("%Y %H:%M:%S"))
                                       #, TimeAgent.utcoffset())
        if type(self.to) == list:
            to = ", ".join(self.to)
        else:
            to = self.to

        self.text =  'From: %s\r\nTo: %s\r\nDate: %s\r\nSubject: %s\r\n\r\n%s\r\n' \
    % (self.frm, to, self.date, self.subject, self.msg)

        for address in self.to:
            # This loop is necessary due to a bug in smtplib.  It only
            # emails the first person in the "to" list.  So, we need to
            # send an email for each person in the "to" list.
            self.failed = self.server.sendmail(self.frm, address, self.text)

        if len(self.failed) > 0:
            raise 'failure in notification:', self.failed

    def QuitServer(self):
        "destroys connection to smtp server; call on cleanup"
        self.server.quit()
