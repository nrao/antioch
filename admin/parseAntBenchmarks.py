import os
from datetime import datetime
from pylab import *

# This Module is a complete hack to take the historical benchmark tests and
# parse and plot them.  We really need to refactor this shit.

colorNames = ['red', \
            'green', \
            'blue', \
            'purple', \
            'maroon', \
            'aqua', \
            'lime', \
            'fuchsia', \
            'olive', \
            'navy', \
            'teal', \
            'yellow', \
            'silver', \
            'gray', \
            'black' ]

# Get the list of files which is our historical record of benchmark data
# This is the dir that the nightly cronjob writes a new file to
dir = "/home/dss/antioch_benchmarks"
files = os.listdir(dir)
# all files that we want to read contain this substring:
bfs = [f for f in files if 'trent_antioch_benchmark_tests' in f]

# we'll create a dictionary of the form:
# results[test_name] = [(date ran, execution speed)]
results = {}

# we'll keep track of the units used like this:
# units[test_name] = "ms" or "seconds", etc.
units = {}

# for each file, read it in and parse it.
for bf in bfs:
    print bf
    f = open(dir +"/" + bf, 'r')
    lines = f.readlines()

    # When was this test run?
    # Here's what we need to do for old exported emails:
    # get the date this file was created from a line like the following:
    # 'Date: Mon, 3 May 2010 04:04:25 -0400'
    timeLines = [l for l in lines if "Date: " in l]
    if len(timeLines) > 0:
        timeLine = timeLines[0]
        pts1 = timeLine.split(",")
        pts2 = pts1[1]
        pts3 = pts2.split("-")
        timeStr = pts3[0].strip()
        dt = datetime.strptime(timeStr, "%d %b %Y %H:%M:%S")
    else:
        # else, it never came from an email, but directly from the cronjob
        # so look for a line like this one:
        # Tue May  4 03:30:01 EDT 2010
        timeLines = [l for l in lines if "EDT" in l]
        if len(timeLines) > 0:
            timeLine = timeLines[0]
            print timeLine
            parts = timeLine.split(" ")
            print parts
            month = parts[1].strip()
            day = parts[3].strip()
            time = parts[4].strip()
            year = parts[6].strip()
            timeStr = "%s %s %s %s" % (year, month, day, time)
            print timeStr
            dt = datetime.strptime(timeStr, "%Y %b %d %H:%M:%S")
        else:
            print "Cant' get time for: bf"
            continue

    # All the actual results we'll want to plot our on these kinds of lines:    
    # benchmark_packWorker_1 Execution Speed: 3376.487 ms\n
    speedLines = [l for l in lines if "Execution Speed" in l]
    for line in speedLines:
       pts1 = line.split(":")
       namePart = pts1[0].strip()
       timePart = pts1[1].strip()
       name = namePart.split(" ")[0]
       timeParts = timePart.split(" ")
       timeStr = timeParts[0]
       unit = timeParts[1]
       units[name] = unit
       executionSpeed = float(timeStr)
       if results.has_key(name):
           results[name].append((dt, executionSpeed))
       else:
           results[name] = [(dt, executionSpeed)]

# sort the results by the date ran
for k in results.keys():
    results[k].sort()

#print "************* results:  "
#print results           

# now plot the results

colorIndex = 0

for name in results.keys():
    # init the plot
    clf()
    xlabel("date")
    ylabel("execution time (%s)" % units[name])
    title(name)

    # we need to transform the dict we made while parsing into
    # the x and y axis lists
    dts = []
    speeds = []
    times = results[name]
    for t in times:
        dts.append(t[0])
        speeds.append(t[1])
    # for use when plotting more then one test in a graph    
    #thisColor = colorNames[colorIndex]
    #colorIndex = colorIndex + 1 if (colorIndex+1) < len(colorNames) else 0
    plot(dts, speeds, label=k, linestyle='-',marker='+', color='red')

    # legacy code that may help fix the tick issue on x axis
    #if len(dts) > len(maxDts): maxDts = dts 
    # fix the tick problem for large plots
    #if len(maxDts) > 20:
    #if 0:
    #    ticks = []
    #    lenX = len(maxDts)
    #    stepSize = int(lenX/5)
    #    step = 0
    #    for i in range(5):
    #        if step < lenX:
    #            ticks.append(maxDts[step])
    #        step +=stepSize
    #        ticks.append(maxDts[-1])    
    #    xticks(ticks)    

    print "saving: ", name
    savefig(dir + "/plots/" + name + ".png") 

# debug one of them
#rs = results["benchmark_pack_1"]
#for r in rs:
#    print "%s : %5.2f" % (r[0], r[1])
