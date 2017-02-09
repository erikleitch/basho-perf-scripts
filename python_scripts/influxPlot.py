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
        print 'Label -= ' + str(label)
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

def str2bool(v):
  return v.lower() in ("yes", "true", "t", "1")

def str2boolArray(v):
    boolArr = []
    for str in v.split(' '):
        boolArr.append(str2bool(str))
    return boolArr

def getInfluxQueryHarness(db, stat, typeFilter, utcstart, utcend, derivative):
    return getInfluxQuery(db, stat, typeFilter, utcstart, utcend, etu.getHarnessHosts().split(' '), derivative)

def getInfluxQuerySystem(db, stat, typeFilter, utcstart, utcend, derivative):
    return getInfluxQuery(db, stat, typeFilter, utcstart, utcend, etu.getSystemHosts().split(' '), derivative)

def getInfluxQuery(db, stat, typeFilter, utcstart, utcend, hosts, derivative):

    # Construct value filter

    if derivative:
        statFilter = "non_negative_derivative(sum(\"" + stat + "\"),30s)/30"
        groupByFilter = " group by time(30s) fill(0)"
    else:
        statFilter = "\"" + stat + "\""
        groupByFilter = ''
        
    # Construct type_instance filter
    
    if typeFilter != 'none':
        typeFilter = " type_instance = '" + typeFilter + "' and"
    else:
        typeFilter = ""
        
    # Construct the host filter
    
    nHost = np.size(hosts)
    hostFilter = " ( host = '" + hosts[0] + "' "
    for iHost in range(1, nHost):
        hostFilter += "or host = '" + hosts[iHost] + "' "
    hostFilter += ")"
    query = "http://localhost:58086/query?db=collectd&q=select " + statFilter + " from " + db + " where " + typeFilter + " time > '" + utcstart + "' and time < '" + utcend + "' and " + hostFilter + groupByFilter
    print "Query = '" + query + "'"
    r = requests.get(query)

    j   = r.json()
    res = j['results']
    series = res[0]['series']
    vals = series[0]['values']
#    print str(vals)
    return vals

def parseDate(start, end, hoursafter, hoursbefore, lasthours):

    # If last hours specified, this is time before now()
    
    if lasthours != None:
        utcnow   = datetime.datetime.utcnow()
        utcstart = utcnow - datetime.timedelta(hours=float(lasthours))

    # Else if hoursafter was specified, this is time since utcstart
    
    elif hoursafter != None:
        utcstart = datetime.datetime.strptime(start, "%Y-%m-%dT%H:%M:%S.%fZ" )
        utcend   = utcstart + datetime.timedelta(hours=float(hoursafter))

    else:
        utcend = datetime.datetime.strptime(end, "%Y-%m-%dT%H:%M:%S.%fZ" )
        utcstart = utcend - datetime.timedelta(hours=float(hoursbefore))
        
    utcstart = utcstart.isoformat() + 'Z'
    utcend   = utcend.isoformat() + 'Z'

    return utcstart, utcend

def storeXlim(key, ax, xlim, firstPlot):

    if firstPlot:
        firstPlot = False
        xlim = ax.get_xlim()
        print 'Key = ' + str(key) + ' got xlim = ' + str(xlim)
    else:
        print 'Key = ' + str(key) + ' set xlim = ' + str(xlim)
        ax.set_xlim(xlim)
        
    return xlim, firstPlot

#=======================================================================
# Script starts here
#=======================================================================

#------------------------------------------------------------
# Arg parsing
#------------------------------------------------------------

pltsmooth = str2bool(etu.getOptArgs(sys.argv, 'smooth', 'false'))
relx      = str2bool(etu.getOptArgs(sys.argv, 'relx', 'false'))
npt       = int(etu.getOptArgs(sys.argv, 'npt', '200'))
rt        = str2bool(etu.getOptArgs(sys.argv, 'rt', 'true'))

db        = etu.getOptArgs(sys.argv, 'db', 'ycsb_operations').split(' ')
stat      = etu.getOptArgs(sys.argv, 'stat', 'value').split(' ')
typeFilter= etu.getOptArgs(sys.argv, 'type', 'none').split(' ')
harness   = str2boolArray(etu.getOptArgs(sys.argv, 'harness', 'true'))
derivative= str2boolArray(etu.getOptArgs(sys.argv, 'derivative', 'true'))

print 'rt = ' + str(rt)

plotWith  = etu.getOptArgs(sys.argv, 'file', None)
if plotWith != None:
    plotWith = plotWith.split(' ')

start       = etu.getOptArgs(sys.argv, 'start', '2017-02-07T19:50:06.939342Z');
end         = etu.getOptArgs(sys.argv, 'end',   '2017-02-07T20:55:06.000000Z');
hoursbefore = etu.getOptArgs(sys.argv, 'hoursbefore', None)
hoursafter  = etu.getOptArgs(sys.argv, 'hoursafter',  None)
lasthours   = etu.getOptArgs(sys.argv, 'lasthours',   None)

if pltsmooth:
    dn  = npt
else:
    dn = 0

nStat = np.size(db)

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

    toK(fileDict)

#------------------------------------------------------------
# If overplotting real-time data, construct the query and retrieve it now
#------------------------------------------------------------

statDict = {}

if rt:
    utcstart, utcend = parseDate(start, end, hoursafter, hoursbefore, lasthours)

    print 'utcstart = ' + str(utcstart)

    for iStat in range(0, nStat):

        if harness[iStat]:
            vals = getInfluxQueryHarness(db[iStat], stat[iStat], typeFilter[iStat], utcstart, utcend, derivative[iStat])
        else:
            vals = getInfluxQuerySystem(db[iStat], stat[iStat], typeFilter[iStat], utcstart, utcend, derivative[iStat])

        x = []
        y = []
        nv = np.size(vals,0)
        for i in range(0, nv):

            if derivative[iStat]:
                utc_dt = datetime.datetime.strptime(vals[i][0], "%Y-%m-%dT%H:%M:%SZ")
            else:
                utc_dt = datetime.datetime.strptime(vals[i][0], "%Y-%m-%dT%H:%M:%S.%fZ")
                
            xval = time.mktime(utc_dt.timetuple())
            yval = vals[i][1]
            x.append(xval)
            y.append(yval)


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

fig = plt.figure(figsize=(10,8))
fig.set_facecolor('w')

# If also plotting smoothed data, we will need two subplots

if pltsmooth:
    delta = dn
else:
    delta = 0

# Plot raw archival data first, then real-time data, if specified

plt.hold(True)
nPlot = np.size(fileDict.keys()) + np.size(statDict.keys())
iPlot = 1
axs = []

firstPlot = True
xlim = []

if plotWith != None:
    for key in fileList:
        ax = fig.add_subplot(nPlot,1,iPlot)    
        lx = fileDict[key]['x']
        ly = fileDict[key]['y']
        ln = fileDict[key]['n']
        ax.plot(lx[delta:ln-delta], ly[delta:ln-delta])
        if relx == False:
            retick(plt, ax)

        xlim, firstPlot = storeXlim(key, ax, xlim, firstPlot)

        axs.append(ax)
        
        iPlot += 1
if rt:
    for iStat in range(0, nStat):
        ax = fig.add_subplot(nPlot,1, iPlot)    
        x = statDict[iStat]['x']
        y = statDict[iStat]['y']
        n = statDict[iStat]['n']
        ax.plot(x[delta:n-delta], y[delta:n-delta])
        if relx == False:
            retick(plt, ax)

        xlim, firstPlot = storeXlim(iStat, ax, xlim, firstPlot)

        axs.append(ax)
        iPlot += 1

# Plot smoothed archival data first, then real-time data, if specified

iPlot = 0
if pltsmooth:

    plt.hold(True)

    if plotWith != None:
        plt.hold(True)
        for key in fileList:
            lx = fileDict[key]['x']
            ly = fileDict[key]['y']
            ln = fileDict[key]['n']
            ls = smooth(ly, npt)
            ax = axs[iPlot]
            iPlot += 1
            ax.plot(lx[dn:ln-dn],ls[dn:ln-dn])

            if relx == False:
                retick(plt, ax)

            xlim, firstPlot = storeXlim(key, ax, xlim, firstPlot)
                
    if rt:
        for iStat in range(0, nStat):
            x = statDict[iStat]['x']
            y = statDict[iStat]['y']
            n = statDict[iStat]['n']
            s = smooth(y, npt)
            ax = axs[iPlot]
            iPlot += 1
            ax.plot(x[dn:n-dn],s[dn:n-dn])

            if relx == False:
                retick(plt, ax)

            xlim, firstPlot = storeXlim(iStat, ax, xlim, firstPlot)


if relx:
    plt.xlabel('Elapsed time (hours)')
    
plt.show()
