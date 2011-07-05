# Copyright (C) 2011 Associated Universities, Inc. Washington DC, USA.
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

from   utilities.SolarHeating import SolarHeating
import numpy
import matplotlib.pyplot as plt

class DBImport:

    def __init__(self):
        self.solar = SolarHeating()

        # -----wind speed coeff-----
        # 80 percentile
        self.windDayCoeff = [-1.70737148e-04,  6.56523342e-03,
                             -9.82652357e-02,  7.21325467e-01,
                             -2.68827245e+00,  5.24223121e+00,
                             -7.61618314e-01]
        self.windNightCoeff = [-3.38584062e-04,  1.19917649e-02,
                               -1.61474697e-01,  1.02041521e+00,
                               -2.98028690e+00,  3.89258501e+00,
                               -5.69079000e-01]
        # 90 percentile
        #self.windDayCoeff = [-2.32399972e-04, 8.71330690e-03,
        #                     -1.27619339e-01, 9.20605091e-01,
        #                     -3.38363363e+00, 6.41438846e+00,
        #                     -8.87433045e-01]
        #self.windNightCoeff = [-2.94322567e-04, 1.03856170e-02,
        #                       -1.39074415e-01, 8.69264137e-01,
        #                       -2.49828840e+00, 3.48695164e+00,
        #                       -4.13931124e-01]

    def correctWindSpeed(self, dt, windSpeedForcast):
        "Function to correct forcasted wind speeds to WS2 wind speeds."
        coeff = \
            self.windDayCoeff if self.solar.isDayTime(dt) else self.windNightCoeff
        
        if windSpeedForcast < 11.1:
            corrected = numpy.polyval(coeff, windSpeedForcast)
            return 0.0 if corrected < 0.0 else corrected
        else:
            return windSpeedForcast

    def plotCorrection(self):
        xs = numpy.arange(0.0, 15.0, 0.1)
        plt.plot(xs, numpy.polyval(self.windDayCoeff, xs))
        plt.plot(xs, numpy.polyval(self.windNightCoeff, xs))
        plt.xlabel("Forcasted")
        plt.ylabel("Corrected")
        plt.title("Forected vs. Corrected wind speed for 80th percentile")
        plt.show()
