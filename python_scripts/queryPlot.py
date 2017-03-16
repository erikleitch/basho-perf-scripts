import datetime, time
import numpy as np
import requests
import matplotlib.pyplot as plt
import sys
import os
import scipy
import erltestutil as etu

def retick(plt, ax):
    xticks = ax.get_xticks()

    newlabels = []
    for label in xticks:
        utc_dt = datetime.datetime.fromtimestamp(label)
        newlabels.append(utc_dt.strftime("%H:%M:%S"))

    print ' new = ' + str(newlabels)
    ax.set_xticklabels(newlabels)
    plt.setp( ax.xaxis.get_majorticklabels(), rotation=+0 )

def toK(d):
    if isinstance(d, dict):
        for key in d.keys():
            d[key]['y'] = toKArr(d[key]['y'])
    else:
        return toKArr(d)

def toKArr(y):
    n = np.size(y)
        
    for i in range(0, n):
        y[i] = y[i] / 1000

    return y

def getInfluxQuery(select, opts):

    query = "http://localhost:" + str(opts['port']) + "/query?db=collectd&q=" + select

    print "Query = '" + query + "'"

    r = requests.get(query)

    j      = r.json()
    res    = j['results']
    series = res[0]['series']
    vals   = series[0]['values']

    return vals

def parseDate(start, end, hoursafter, hoursbefore, lasthours):

    # If last hours specified, this is time before now()
    
    if lasthours != None:
        utcend   = datetime.datetime.utcnow()
        utcstart = utcend - datetime.timedelta(hours=float(lasthours))

    # Else if hoursafter was specified, this is time since utcstart
    
    elif hoursafter != None:
        print 'after = ' + str(hoursafter)
        utcstart = datetime.datetime.strptime(start, "%Y-%m-%dT%H:%M:%S.%fZ" )
        utcend   = utcstart + datetime.timedelta(hours=float(hoursafter))
    else:
        utcend = datetime.datetime.strptime(end, "%Y-%m-%dT%H:%M:%S.%fZ" )
        utcstart = utcend - datetime.timedelta(hours=float(hoursbefore))
        
    utcstart = utcstart.isoformat() + 'Z'
    utcend   = utcend.isoformat() + 'Z'

    print 'Parsing utcstart = ' + utcstart + ' end = ' + utcend + ' hours = ' + str(hoursafter)

    return utcstart, utcend

def storeXlim(ax, xlim, firstPlot):

    newlim = ax.get_xlim()
    retlim = []

    if firstPlot:
        firstPlot = False
        retlim = newlim
        print 'First plot: newlim = ' + str(newlim) + ' storing retlim = ' + str(retlim)
    else:
        retlim.append(etu.min(xlim[0], newlim[0]))
        retlim.append(etu.max(xlim[1], newlim[1]))
        ax.set_xlim(retlim)
        print 'Not first plot: newlim = ' + str(newlim) + ' setting retlim = ' + str(retlim)
        
    return retlim, firstPlot

def getAxes(plotWith, fileList, rt, statDict, overplot, pltsmooth, overplotsmooth):

    # If overplotting all traces we only have one plot, unless we are
    # also plotting smoothed versions and not overplotting those on
    # top of the raw data, in which case we have 2

    if overplot:
        if pltsmooth and not overplotsmooth:
            nPlot = 2
        else:
            nPlot = 1
    elif pltsmooth and not overplotsmooth:
        nPlot = 2*(np.size(fileDict.keys()) + np.size(statDict.keys()))
    else:
        nPlot = np.size(fileDict.keys()) + np.size(statDict.keys())

    axs = []

    for iPlot in range(0, nPlot):
        axs.append(fig.add_subplot(nPlot, 1, iPlot+1))

    return axs

def makePlot(ax, valDict, ind, opts):

    x = valDict[ind]['x']
    y = valDict[ind]['y']
    n = valDict[ind]['n']

    delta = 0
    if opts['smooth']:
        npt   = opts['npt']
        delta = npt
        y = etu.smooth(y, npt)

    if opts['perc']:
        y = etu.percentageArr(y, delta)

    if opts['logy']:
        ax.set_yscale('log')

    ax.plot(x[delta:n-delta], y[delta:n-delta])

    mn  = np.mean(y[delta:n-delta])
    std = np.std(y[delta:n-delta])

    rng = ax.get_ylim()
    newrng = [rng[0], rng[1]]
    
    if opts['min'] != '':
        newrng[0] = float(opts['min'])
    if opts['max'] != '':
        newrng[1] = float(opts['max'])

    ax.set_ylim(newrng[0], newrng[1])
    
    xaxis = ax.get_xlim()
    
    if opts['relx'] == False:
        retick(plt, ax)

#=======================================================================
# Script starts here
#=======================================================================

#------------------------------------------------------------
# Arg parsing
#------------------------------------------------------------

query = sys.argv[1]

print 'Foudn query = ' + str(query)

optsDict = {}
optsDict['port']   = etu.getOptArgs(sys.argv, 'port', 58086)
optsDict['min']    = etu.getOptArgs(sys.argv, 'min',  '')
optsDict['max']    = etu.getOptArgs(sys.argv, 'max',  '')
optsDict['relx']   = etu.str2bool(etu.getOptArgs(sys.argv, 'relx', 'false'))
optsDict['perc']   = etu.str2bool(etu.getOptArgs(sys.argv, 'perc', 'false'))
optsDict['logy']   = etu.str2bool(etu.getOptArgs(sys.argv, 'logy', 'false'))
optsDict['smooth'] = etu.str2bool(etu.getOptArgs(sys.argv, 'smooth', 'false'))
optsDict['npt']    = etu.getOptArgs(sys.argv, 'npt',  200)
optsDict['figsize']= etu.strToIntTuple(etu.getOptArgs(sys.argv, 'figsize', '18,6'))
optsDict['legend'] = etu.toStrArrayOrNone(etu.getOptArgs(sys.argv, 'legend',   None), ';')

statDict = {}

nStat = 1
iStat = 0
statDict[iStat] = {}

vals = getInfluxQuery(query, optsDict)

x = []
y = []

nv = np.size(vals,0)
for i in range(0, nv):

    if vals[i][0].find('.') >= 0:
        utc_dt = datetime.datetime.strptime(vals[i][0], "%Y-%m-%dT%H:%M:%S.%fZ")
    else:
        utc_dt = datetime.datetime.strptime(vals[i][0], "%Y-%m-%dT%H:%M:%SZ")
        
    xval = time.mktime(utc_dt.timetuple())
    yval = vals[i][1]
    x.append(xval)
    y.append(yval)

if optsDict['relx']:
    x = etu.relative(x)

n = np.size(x)

statDict[iStat]['x'] = x
statDict[iStat]['y'] = y
statDict[iStat]['n'] = n

#------------------------------------------------------------
# Make plots
#------------------------------------------------------------

fig = plt.figure(figsize=optsDict['figsize'])
fig.set_facecolor('w')

axs = []
axs.append(fig.add_subplot(1,1,1))

#------------------------------------------------------------
# Plot raw archival data first, then real-time data, if specified
#------------------------------------------------------------

for iStat in range(0, nStat):
    ax = axs[iStat]
    makePlot(ax, statDict, iStat, optsDict)

if optsDict['legend'] != None:
    plt.legend(optsDict['legend'])
    
if optsDict['relx']:
    plt.xlabel('Elapsed time (hours)')

plt.show(block=True)
