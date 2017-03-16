import datetime, time
import numpy as np
import requests
import matplotlib.pyplot as plt
import sys
import os
import scipy
import erltestutil as etu

def smooth(y, box_pts):

    n = np.size(y)
    ntotal = 2*box_pts + n
    print 'ntotal = ' + str(ntotal)
    yarr = np.ndarray(ntotal,dtype=np.double)
    print 'n = ' + str(n) + ' s1 = ' + str(np.shape(y)) + ' s2 = ' + str(np.shape(yarr))
    
    for i in range(0,ntotal):
        if i < box_pts:
            yarr[i] = 0.0
        elif i >= n + box_pts:
            yarr[i] = 0.0
        else:
            yarr[i] = y[i - box_pts]
            
    box = np.ones(box_pts)/box_pts
    yarr_smooth = np.convolve(yarr, box, mode='same')
    return yarr_smooth[box_pts:box_pts+n]
            
def gsmooth(x, y, sigma):
    n = np.size(x)
    s = np.ndarray(n, np.double)
    for i in range(0, n):
        dx = (x[i] - x[0])
        s[i] = np.exp(-dx*dx/(2*sigma*sigma))
    return s

def retick(plt, ax):
    xticks = ax.get_xticks()

    newlabels = []
    for label in xticks:
        utc_dt = datetime.datetime.fromtimestamp(label)
        newlabels.append(utc_dt.strftime("%H:%M:%S"))

    print ' new = ' + str(newlabels)
    ax.set_xticklabels(newlabels)
    plt.setp( ax.xaxis.get_majorticklabels(), rotation=+0 )

def relative(d):
    if isinstance(d, dict):
        for key in d.keys():
            d[key]['x'] = relativeArr(d[key]['x'])
    else:
        return relativeArr(d)

def relativeArr(x):
    n = np.size(x)
    x0 = x[0]
    for i in range(0,n):
        x[i] = (x[i] - x0) / 3600

    return x

def percentage(d, dn):
    if isinstance(d, dict):
        for key in d.keys():
            d[key]['y'] = percentageArr(d[key]['y'], dn)
    else:
        return percentageArr(d, dn)
        
def percentageArr(y, dn):
    n = np.size(y)

    ymax = y[dn]
    print 'ymax =  ' + str(ymax)
    for i in range(0, n):
        y[i] = y[i] / ymax

    return y

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

def getInfluxQueryHarness(port, optsDict, utcstart, utcend):
    return getInfluxQuery(port, optsDict, utcstart, utcend, etu.getHarnessHosts(optsDict).split(' '))

def getInfluxQuerySystem(port, optsDict, utcstart, utcend):
    return getInfluxQuery(port, optsDict, utcstart, utcend, etu.getSystemHosts(optsDict).split(' '))

#def getSysCpuQuery
#     "query": "SELECT non_negative_derivative(sum(\"value\"), $interval) FROM \"cpu_value\" WHERE \"type_instance\" = 'idle' AND  ($\
#systemFilter)  AND $timeFilter GROUP BY time($interval) fill(0)",
     
def getInfluxQuery(port, optsDict, utcstart, utcend, hosts):

    #------------------------------------------------------------
    # Construct value filter
    #------------------------------------------------------------
    
    if optsDict['derivative']:
        if optsDict['db'] == 'cpu_value':
            statFilter = "non_negative_derivative(sum(\"" + optsDict['stat'] + "\"),10s)"
            groupByFilter = " group by time(10s) fill(0)"
        else:
            statFilter = "non_negative_derivative(sum(\"" + optsDict['stat'] + "\"),10s)/10"
            groupByFilter = " group by time(10s) fill(0)"
    else:
        statFilter = "\"" + optsDict['stat'] + "\""
        groupByFilter = ''

    #------------------------------------------------------------
    # Construct type filter
    #------------------------------------------------------------

    selectFilter = ''
    print 'type Filter = ' + str(optsDict['typeFilter']) + ' select = ' + str(selectFilter)
    
    if optsDict['typeFilter'] != 'none':
        if selectFilter == '':
            selectFilter = " type = '" + optsDict['typeFilter'] + "'"
        else:
            selectFilter = " and type = '" + optsDict['typeFilter'] + "'"

    #------------------------------------------------------------
    # Construct type_instance filter
    #------------------------------------------------------------

    if optsDict['typeInstanceFilter'] == 'othercpu':
        if selectFilter == '':
            selectFilter = " type_instance != 'system' and type_instance != 'idle' and type_instance != 'user'"
        else:
            selectFilter += " and type_instance != 'system' and type_instance != 'idle' and type_instance != 'user'"
        
    elif optsDict['typeInstanceFilter'] != 'none':
        if selectFilter == '':
            selectFilter = " type_instance = '" + optsDict['typeInstanceFilter'] + "'"
        else:
            selectFilter += " and type_instance = '" + optsDict['typeInstanceFilter'] + "'"
            
    #------------------------------------------------------------
    # Construct instance filter
    #------------------------------------------------------------
    
    if optsDict['instanceFilter'] != 'none':
        if selectFilter == '':
            selectFilter = " instance = '" + optsDict['instanceFilter'] + "'"
        else:
            selectFilter += " and instance = '" + optsDict['instanceFilter'] + "'"

    if selectFilter != '':
        selectFilter += ' and '
        
    #------------------------------------------------------------        
    # Construct the host filter
    #------------------------------------------------------------
    
    nHost = np.size(hosts)
    hostFilter = " ( host = '" + hosts[0] + "' "
    for iHost in range(1, nHost):
        hostFilter += "or host = '" + hosts[iHost] + "' "
    hostFilter += ")"
    query = "http://localhost:" + str(port) + "/query?db=collectd&q=select " + statFilter + " from " + optsDict['db'] + " where " + selectFilter + " time > '" + utcstart + "' and time < '" + utcend + "' and " + hostFilter + groupByFilter

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

def getAxis(axs, iPlot, smooth, overplot, overplotsmooth):
    if not smooth or overplotsmooth:
        if overplot:
            return axs[0]
        else:
            return axs[iPlot]
    elif overplot:
        return axs[1]
    else:
        nTrace = np.size(axs)/2
        return axs[nTrace + iPlot]
    
def makePlot(ax, valDict, ind, relx, delta, xlim, firstPlot, smoothData, npt, perc, logy, opts):

    x = valDict[ind]['x']
    y = valDict[ind]['y']
    n = valDict[ind]['n']

    if smoothData:
        y = smooth(y, npt)

    if perc:
        y = percentageArr(y, delta)

    if logy:
        ax.set_yscale('log')

    print 'Plotting data to axis ' + str(ax)
    ax.plot(x[delta:n-delta], y[delta:n-delta])

    mn  = np.mean(y[delta:n-delta])
    std = np.std(y[delta:n-delta])

    rng = ax.get_ylim()
    print 'rng = ' + str(rng)
    newrng = [rng[0], rng[1]]

    if opts['min'] != None:
        newrng[0] = float(opts['min'])
    if opts['max'] != None:
        newrng[1] = float(opts['max'])

    print 'newrng = ' + str(newrng)
    print 'setting rng 0'
    ax.set_ylim(newrng[0], newrng[1])
    print 'setting rng 1'
    
    xaxis = ax.get_xlim()

    #newxlim = []
    #newxlim.append(etu.min(xaxis[0], np.min(x[delta:n-delta])))
    #newxlim.append(etu.max(xaxis[1], np.max(x[delta:n-delta])))
    
    if relx == False:
        retick(plt, ax)

    xlim, firstPlot = storeXlim(ax, xlim, firstPlot)

    return xlim, firstPlot

#=======================================================================
# Script starts here
#=======================================================================

#------------------------------------------------------------
# Arg parsing
#------------------------------------------------------------

figsize        = etu.strToIntTuple(etu.getOptArgs(sys.argv, 'figsize', '18,6'))
pltsmooth      = etu.str2bool(etu.getOptArgs(sys.argv, 'smooth', 'false'))
relx           = etu.str2bool(etu.getOptArgs(sys.argv, 'relx', 'false'))
npt            = int(etu.getOptArgs(sys.argv, 'npt', '200'))
rt             = etu.str2bool(etu.getOptArgs(sys.argv, 'rt', 'true'))
overplot       = etu.str2bool(etu.getOptArgs(sys.argv, 'overplot', 'false'))
overplotsmooth = etu.str2bool(etu.getOptArgs(sys.argv, 'overplotsmooth', 'true'))

perc        = etu.str2bool(etu.getOptArgs(sys.argv, 'perc', 'false'))
ports       = etu.strToIntArray(etu.getOptArgs(sys.argv, 'port', '58086'))
dbs         = etu.getOptArgs(sys.argv, 'db', 'ycsb_operations').split(' ')
stats       = etu.getOptArgs(sys.argv, 'stat', 'value').split(' ')
typeFilters = etu.getOptArgs(sys.argv, 'type', 'none').split(' ')
instanceFilters= etu.getOptArgs(sys.argv, 'instance', 'none').split(' ')
typeInstanceFilters= etu.getOptArgs(sys.argv, 'type_instance', 'none').split(';')
clusters    = etu.getOptArgs(sys.argv, 'cluster', 'sla').split(' ')
harnesses   = etu.strToBoolArray(etu.getOptArgs(sys.argv, 'harness', 'true'))
derivatives = etu.strToBoolArray(etu.getOptArgs(sys.argv, 'derivative', 'true'))
starts      = etu.getOptArgs(sys.argv, 'start', '2017-02-07T19:50:06.939342Z').split(' ');
end         = etu.getOptArgs(sys.argv, 'end',   '2017-02-07T20:55:06.000000Z');
hoursbefore = etu.strToFloatArrayOrNone(etu.getOptArgs(sys.argv, 'hoursbefore', None))
hoursafter  = etu.strToFloatArrayOrNone(etu.getOptArgs(sys.argv, 'hoursafter',  None))
lasthours   = etu.getOptArgs(sys.argv, 'lasthours',   None)
legend      = etu.toStrArrayOrNone(etu.getOptArgs(sys.argv, 'legend',   None), ';')
xlimopt     = etu.getOptArgs(sys.argv, 'xlim',  None)
query       = etu.getOptArgs(sys.argv, 'query',  None)

print 'Foudn query = ' + str(query)

optsDict = {}
optsDict['min'] = etu.getOptArgs(sys.argv, 'min',  None)
optsDict['max'] = etu.getOptArgs(sys.argv, 'max',  None)

nStat = np.size(ports)
nStat = etu.max(nStat, np.size(starts))
nStat = etu.max(nStat, np.size(dbs))
nStat = etu.max(nStat, np.size(stats))
nStat = etu.max(nStat, np.size(typeFilters))
nStat = etu.max(nStat, np.size(typeInstanceFilters))
nStat = etu.max(nStat, np.size(clusters))
nStat = etu.max(nStat, np.size(harnesses))
nStat = etu.max(nStat, np.size(derivatives))

if not rt:
    nStat = 0
    
logys = etu.strToBoolArray(etu.getOptArgs(sys.argv, 'logy', 'false'))

print 'rt = ' + str(rt)

plotWith  = etu.getOptArgs(sys.argv, 'file', None)
if plotWith != None:
    plotWith = plotWith.split(' ')

if pltsmooth:
    dn  = npt
else:
    dn = 0

#------------------------------------------------------------
# Extract data from any files that were specified
#------------------------------------------------------------

nFile = np.size(plotWith)
fileDict = {}
fileList = []
if plotWith != None:
    for iFile in range(0, nFile):
        fileName = plotWith[iFile]
        lx, ly = etu.loadDataFromFile(fileName)
        if relx:
            lx = relative(lx)
            ln = np.size(ly)
        fileDict[fileName] = {}
        fileDict[fileName]['file'] = fileName
        fileDict[fileName]['x'] = lx
        fileDict[fileName]['y'] = ly
        fileDict[fileName]['n'] = ln
        fileList.append(fileName)

#    toK(fileDict)

#------------------------------------------------------------
# If overplotting real-time data, construct the query and retrieve it now
#------------------------------------------------------------

statDict = {}

if rt:
    for iStat in range(0, nStat):

        
        utcstart, utcend = parseDate(etu.indOrVal(starts, iStat), end, etu.indOrVal(hoursafter, iStat), etu.indOrVal(hoursbefore, iStat), lasthours)
        harness    = etu.indOrVal(harnesses, iStat)


        optsDict['cluster']            = etu.indOrVal(clusters, iStat)
        optsDict['stat']               = etu.indOrVal(stats, iStat)
        optsDict['db']                 = etu.indOrVal(dbs, iStat)
        optsDict['typeFilter']         = etu.indOrVal(typeFilters, iStat)
        optsDict['instanceFilter']     = etu.indOrVal(instanceFilters, iStat)
        optsDict['typeInstanceFilter'] = etu.indOrVal(typeInstanceFilters, iStat)
        optsDict['derivative']         = etu.indOrVal(derivatives, iStat)

        port       = etu.indOrVal(ports, iStat)
        
        if harness:
            vals = getInfluxQueryHarness(port, optsDict, utcstart, utcend)
        else:
            vals = getInfluxQuerySystem(port, optsDict, utcstart, utcend)

        f = open('influxQuery_' + str(iStat) + '.txt', 'wb')
        
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

            f.write("%5.2f %5.2f\n" % (xval, yval))
            
        f.close()
        
        if relx:
            x = relative(x)
        n = np.size(x)

        statDict[iStat] = {}
        statDict[iStat]['x'] = x
        statDict[iStat]['y'] = y
        statDict[iStat]['n'] = n

#------------------------------------------------------------
# Make plots
#------------------------------------------------------------

fig = plt.figure(figsize=figsize)
fig.set_facecolor('w')

# If also plotting smoothed data, we will need two subplots

if pltsmooth:
    delta = dn
else:
    delta = 0

#------------------------------------------------------------
# Plot raw archival data first, then real-time data, if specified
#------------------------------------------------------------

if overplot:
    nPlot = 1
else:
    nPlot = np.size(fileDict.keys()) + np.size(statDict.keys())

if pltsmooth:
    if not overplotsmooth:
        nPlot = nPlot*2
    
iPlot = 1
axs   = []

firstPlot = True
xlim = []

print 'pltsmooth = ' + str(pltsmooth)

axs   = getAxes(plotWith, fileList, rt, statDict, overplot, pltsmooth, overplotsmooth)
nPlot = np.size(axs)

#------------------------------------------------------------
# Plot raw archival data first, then real-time data, if specified
#------------------------------------------------------------

iPlot = 0
for key in fileList:
    ax = getAxis(axs, iPlot, False, overplot, overplotsmooth)
    xlim, firstPlot = makePlot(ax, fileDict, key, relx, delta, xlim, firstPlot, False, npt, False, False, optsDict)
    iPlot += 1

for iStat in range(0, nStat):
    ax = getAxis(axs, iPlot, False, overplot, overplotsmooth)
    xlim, firstPlot = makePlot(ax, statDict, iStat, relx, delta, xlim, firstPlot, False, npt, False, etu.indOrVal(logys, iStat), optsDict)
    iPlot += 1
        
#------------------------------------------------------------
# Plot smoothed archival data first, then real-time data, if specified
#------------------------------------------------------------

if pltsmooth:
    
    iPlot = 0
    for key in fileList:
        ax = getAxis(axs, iPlot, True, overplot, overplotsmooth)
        xlim, firstPlot = makePlot(ax, fileDict, key, relx, delta, xlim, firstPlot, True, npt, perc, False, optsDict)
        iPlot += 1

    for iStat in range(0, nStat):
        ax = getAxis(axs, iPlot, True, overplot, overplotsmooth)
        xlim, firstPlot = makePlot(ax, statDict, iStat, relx, delta, xlim, firstPlot, True, npt, perc, etu.indOrVal(logys, iStat), optsDict)
        iPlot += 1

if legend != None:
    plt.legend(legend)
    
if relx:
    plt.xlabel('Elapsed time (hours)')

if xlimopt != None:
    lims = plt.xlim()
    newlims = []
    newlims.append(lims[0])
    newlims.append(float(xlimopt))
    plt.xlim(newlims)
    
plt.show(block=True)
