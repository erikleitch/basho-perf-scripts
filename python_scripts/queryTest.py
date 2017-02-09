import datetime, time
import dateutil.parser
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

def getInfluxQueryHarness(db, utcstart, utcend):
    r = requests.get("http://localhost:58086/query?db=collectd&q=select non_negative_derivative(sum(value),10s)/10 from " + db + " where time > '" + utcstart + "' and time < '" + utcend + "' and (  host = 'ip-10-1-2-1' or host = 'ip-10-1-2-2' or host = 'ip-10-1-2-3' or host = 'ip-10-1-2-4' or host = 'ip-10-1-2-5'  ) group by time(10s) fill(0)")

    j   = r.json()
    res = j['results']
    series = res[0]['series']
    vals = series[0]['values']
    return vals

def getInfluxQuerySystem(db, utcstart, utcend):
    r = requests.get("http://localhost:58086/query?db=collectd&q=select non_negative_derivative(sum(value),10s)/10 from " + db + " where type_instance='ThrottleCompacts0' and time > '" + utcstart + "' and time < '" + utcend + "' and (  host = 'ip-10-1-3-1' or host = 'ip-10-1-3-2' or host = 'ip-10-1-3-3' or host = 'ip-10-1-3-4' or host = 'ip-10-1-3-5'  ) group by time(10s) fill(0)")

    j   = r.json()
    res = j['results']
    series = res[0]['series']
    vals = series[0]['values']
    return vals

#=======================================================================
# Script starts here
#=======================================================================

#------------------------------------------------------------
# Arg parsing
#------------------------------------------------------------

pltsmooth = str2bool(etu.getOptArgs(sys.argv, 'smooth', 'false'))
relx      = str2bool(etu.getOptArgs(sys.argv, 'relx', 'false'))
hours     = float(etu.getOptArgs(sys.argv, 'hours', '0.0'))
npt       = int(etu.getOptArgs(sys.argv, 'npt', '200'))
rt        = str2bool(etu.getOptArgs(sys.argv, 'rt', 'true'))

print 'rt = ' + str(rt)

plotWith  = etu.getOptArgs(sys.argv, 'file', None)
if plotWith != None:
    plotWith = plotWith.split(' ')

start     = etu.getOptArgs(sys.argv, 'start', '2017-02-07T19:50:06.939342Z');
end       = etu.getOptArgs(sys.argv, 'end',   '2017-02-07T20:55:06.000000Z');

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

    toK(fileDict)

#------------------------------------------------------------
# If overplotting real-time data, construct the query and retrieve it now
#------------------------------------------------------------

if rt:
    if hours > 0.0:
        print str(start)
        utcstart = datetime.datetime.strptime(start, "%Y-%m-%dT%H:%M:%S.%fZ" )
#        utcstart = dateutil.parser.parse(start)
        print str(hours)
        utcend   = utcstart + datetime.timedelta(hours=hours)
        utcstart = utcstart.isoformat() + 'Z'
        utcend   = utcend.isoformat() + 'Z'
        print 'Start = ' + str(utcstart) + ' End = ' + str(utcend)
#        utcnow   = datetime.datetime.utcnow()
#        utcend   = utcnow.isoformat() + 'Z'
#        utcend   = end
#        utcstart = start

        #        utcnow   = datetime.datetime.utcnow()
#        utcend   = utcnow.isoformat() + 'Z'
#        utcstart = utcnow - datetime.timedelta(hours=hours)
#        utcstart = utcstart.isoformat() + 'Z'
    else:
        utcnow   = datetime.datetime.utcnow()
        utcend   = utcnow.isoformat() + 'Z'
#        utcend   = end
        utcstart = start

    print 'utcstart = ' + str(utcstart)

    vals = getInfluxQueryHarness('ycsb_operations', utcstart, utcend)
    #vals = getInfluxQuerySystem('leveldb_value', utcstart, utcend)

    nv = np.size(vals,0)
    f = open('queryTest.txt', 'wb')
            
    x = []
    y = []
    for i in range(0, nv):
        utc_dt = datetime.datetime.strptime(vals[i][0], "%Y-%m-%dT%H:%M:%SZ")
        xval = time.mktime(utc_dt.timetuple())
        yval = vals[i][1]
        x.append(xval)
        y.append(yval)
        f.write("%5.2f %5.2f\n" % (xval, yval))

    f.close()

    y  = toK(y)

    if relx:
        x = relative(x)
    n = np.size(x)

#------------------------------------------------------------
# Make plots
#------------------------------------------------------------

fig = plt.figure(figsize=(10,8))
fig.set_facecolor('w')

# If also plotting smoothed data, we will need two subplots

if pltsmooth:
    delta = dn
    ax = fig.add_subplot(2,1,1)
else:
    delta = 0
    ax = fig.add_subplot(1,1,1)    

# Plot raw archival data first, then real-time data, if specified

plt.hold(True)
if plotWith != None:
    for key in fileList:
        lx = fileDict[key]['x']
        ly = fileDict[key]['y']
        ln = fileDict[key]['n']
        ax.plot(lx[delta:ln-delta], ly[delta:ln-delta])
if rt:
    ax.plot(x[delta:n-delta], y[delta:n-delta])

if relx == False:
    retick(plt, ax)

plt.ylabel("Raw integrated ops/sec (K)")
plt.title("YCSB Throughput 5+5 cluster, AWS")

# Plot smoothed archival data first, then real-time data, if specified

if pltsmooth:
    ax = fig.add_subplot(2,1,2)
    plt.hold(True)

    if plotWith != None:
        plt.hold(True)
        for key in fileList:
            lx = fileDict[key]['x']
            ly = fileDict[key]['y']
            ln = fileDict[key]['n']
            ls = smooth(ly, npt)
            ls = percentage(ls, dn)
            ax.plot(lx[dn:ln-dn],ls[dn:ln-dn])

    if rt:
        s = smooth(y, npt)
        s  = percentage(s, dn)
        ax.plot(x[dn:n-dn],s[dn:n-dn])

    if relx == False:
        retick(plt, ax)

    ax.set_ylim([0.5, 1.1])
    
    plt.ylabel("Smoothed integrated ops/sec\n(ratio to max)")

#    print '1.0-ratio = ' + str(1.0 - s[n-dn]/s[dn])
#    print 'sum - ' + str(np.sum(s[dn:n-dn]))

#    totalOps = np.sum(s[dn:n-dn]) * 10
#    print 'GB per node: ' + str((totalOps * 100 * 10.0)/(5 * 1e9))

if relx:
    plt.xlabel('Elapsed time (hours)')
    
plt.show()
