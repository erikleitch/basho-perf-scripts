import sys
import os
from os import listdir
from os.path import isfile, join
import numpy as np
import subprocess

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

def max(v1, v2):
    if v1 > v2:
        return v1
    else:
        return v2

def min(v1, v2):
    if v1 < v2:
        return v1
    else:
        return v2
    
def indOrVal(arr, ind):

    if arr == None:
        return None;
    
    if np.size(arr) > 1 and ind >= 0 and ind < np.size(arr):
        return arr[ind]
    else:
        return arr[0]
  
def strToIntTuple(str):
  iarr = listToInt(str.split(','))
  return tuple(iarr)

def strToFloatTuple(str):
  farr = listToFloat(str.split(','))
  return tuple(farr)

def strToBool(v):
  return str2bool(v)

def str2bool(v):
  return v.lower() in ("yes", "true", "t", "1")

def strToBoolArray(v):
  return str2boolArray(v)

def str2boolArray(v):
    boolArr = []
    for str in v.split(' '):
        boolArr.append(str2bool(str))
    return boolArr

def strToIntArray(v):
    intArr = []
    for str in v.split(' '):
        intArr.append(int(str))
    return intArr

def strToFloatArray(v):
    floatArr = []
    for str in v.split(' '):
        floatArr.append(float(str))
    return floatArr

def strToFloatArrayOrNone(v):
    floatArr = []

    if v == None:
        return None
    
    for str in v.split(' '):
        floatArr.append(float(str))
    return floatArr

def toStrArrayOrNone(v, splstr):
    strArr = []

    if v == None:
        return None
    
    for str in v.split(splstr):
        strArr.append(str)
    return strArr

def listToInt(larr):
  iarr = []
  for i in range(0, np.size(larr)):
    iarr.append(int(larr[i]))
  return iarr

def listToFloat(larr):
  farr = []
  for i in range(0, np.size(larr)):
    farr.append(float(larr[i]))
  return farr

def str2bool(v):
  return v.lower() in ("yes", "true", "t", "1")

def outlierRejectConf(dat, nsig):
    h = np.histogram(dat, bins=1000)
    mn, lb, ub = getConfidenceInterval(h[1], h[0], 0.68)
    sig = (ub-lb)/2
    n   = np.size(dat)
    ret = dat[np.where(np.abs(dat - mn) < nsig*sig)]
    if np.size(ret) < np.size(dat):
        return outlierReject(ret, nsig)
    else:
        return ret

def outlierReject(dat, nsig):
    mn  = np.mean(dat)
    sig = np.std(dat, ddof=0)
    n   = np.size(dat)
    ret = dat[np.where(np.abs(dat - mn) < nsig*sig)]
    if np.size(ret) < np.size(dat):
        return outlierReject(ret, nsig)
    else:
        return ret

def getOptArgs(args, name, defval):
    d = {}
    for arg in args:
        if '=' in arg:
            splargs = arg.split('=')
            d[splargs[0]] = splargs[1]

    if name in d.keys():
      return d[name]
    else:
      return defval

def getFiles(mypath):
    onlyFiles = [f for f in listdir(mypath) if isfile(join(mypath, f))]
    return onlyFiles

def interval(dat, bins, perc):
  h = np.histogram(dat, bins=bins)
  return getConfidenceInterval(h[1], h[0], perc)
  
def getStat(path, file, stat, opts):
    dat = np.loadtxt(path + '/' + file);

    if opts['nsig'] > 0.0:
      print 'Size before outlier rejection: ' + str(np.size(dat))
      dat = outlierReject(dat, float(opts['nsig']))
      print 'Size after outlier rejection: ' + str(np.size(dat))

    nline = np.shape(dat)[0];
    if stat == 'mean':
        return np.mean(dat)
    elif stat == 'median':
        return np.median(dat)
    elif stat == 'data':
      return dat
    elif 'conf' in stat:
      perc = float(stat.split('conf')[0])/100
      print 'Computing ' + str(perc) + ' confidence interval'
      return interval(dat, int(opts['bins']), perc)
    elif 'perc' in stat:
      perc = float(stat.split('perc')[0])
      print 'Computing ' + str(perc) + ' percentile'
      return np.percentile(dat, perc)

def loadDataFromFile(file):
    dat = np.loadtxt(file);
    nline = np.shape(dat)[0];
    ncol  = np.shape(dat)[1];
    x = dat[:,0]
    y = dat[:,1]
    return x, y

def getSystemHosts(opts):
    if opts['cluster'] == "sla":
        return getSLASystemHosts()
    else:
        return getAwsSystemHosts()

def getAwsSystemHosts():
  return "ip-10-1-3-1 ip-10-1-3-2 ip-10-1-3-3 ip-10-1-3-4 ip-10-1-3-5"

def getHarnessHosts(opts):
    if opts['cluster'] == "sla":
        return getSLAHarnessHosts()
    else:
        return getAwsHarnessHosts()

def getAwsHarnessHosts():
  return "ip-10-1-2-1 ip-10-1-2-2 ip-10-1-2-3 ip-10-1-2-4 ip-10-1-2-5"

def getSLASystemHosts():
  return "basho-c2s1 basho-c2s2 basho-c2s3 basho-c2s4 basho-c2s5"

def getSLAHarnessHosts():
  return "basho-c1s1 basho-c1s2 basho-c1s3 basho-c1s4"


def getNextValSymm(y, n, indlo, indhi, probSum, probTarget):

    print 'indlo = ' + str(indlo) + ' indhi = ' + str(indhi)
    stop     = False
    indstart = 0
    indend   = n-1
    
    # If we've reached the target probability, stop.  Else add the
    # next point

    if probSum >= probTarget:
        stop = True

    # Else if we haven't hit either end, add the highest probability next
    
    elif indlo > indstart and indhi < indend:
        hival = y[indhi+1]
        loval = y[indlo-1]

        if hival > loval:
            indhi   += 1
            probSum += hival
            if probSum < probTarget:
                indlo   -= 1
                probSum += loval
        else:
            indlo   -= 1
            probSum += loval
            if probSum < probTarget:
                indhi += 1
                probSum += hival

    # Else if we are already at the end of the array, add the lower value

    elif indlo > indstart:
        indlo   -= 1
        probSum += y[indlo]

    # Else if we are already at the start of the array, add the higher value
    
    else:
        indhi   += 1
        probSum += y[indhi]

    return indlo, indhi, probSum, stop

def getNextValHighest(y, n, indlo, indhi, probSum, probTarget):

    print 'indlo = ' + str(indlo) + ' indhi = ' + str(indhi)
    stop     = False
    indstart = 0
    indend   = n-1
    
    # If we've reached the target probability, stop.  Else add the
    # next point

    if probSum >= probTarget:
        stop = True

    # Else if we haven't hit either end, add the highest probability next
    
    elif indlo > indstart and indhi < indend:
        hival = y[indhi+1]
        loval = y[indlo-1]

        if hival > loval:
            indhi   += 1
            probSum += hival
        else:
            indlo   -= 1
            probSum += loval

    # Else if we are already at the end of the array, add the lower value

    elif indlo > indstart:
        indlo   -= 1
        probSum += y[indlo]

    # Else if we are already at the start of the array, add the higher value
    
    else:
        indhi   += 1
        probSum += y[indhi]

    return indlo, indhi, probSum, stop

def getConfidenceInterval(x, y, perc):
  return getConfidenceIntervalHighest(x, y, perc)
  
def getConfidenceIntervalHighest(x, y, perc):
    
    n = np.size(y)

    indmax = np.argmax(y)

    total      = np.sum(y)
    probTarget = perc * total
    probSum    = y[indmax]

    indhi = indmax
    indlo = indmax

    stop = False
    while not stop:
      print ' sum(0) = ' + str(probSum) + ' target = ' + str(probTarget)
      indlo, indhi, probSum, stop = getNextValHighest(y, n, indlo, indhi, probSum, probTarget)
      print ' sum(1) = ' + str(probSum) + ' target = ' + str(probTarget)
      
    return x[indmax], x[indlo], x[indhi]

def getConfidenceIntervalSymm(x, y, perc):
    
    n = np.size(y)

    indmax = np.argmax(y)

    total      = np.sum(y)
    probTarget = perc * total
    probSum    = y[indmax]

    indhi = indmax
    indlo = indmax

    stop = False
    while not stop:
      print ' sum(0) = ' + str(probSum) + ' target = ' + str(probTarget)
      indlo, indhi, probSum, stop = getNextValSymm(y, n, indlo, indhi, probSum, probTarget)
      print ' sum(1) = ' + str(probSum) + ' target = ' + str(probTarget)
      
    return x[indmax], x[indlo], x[indhi]

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
