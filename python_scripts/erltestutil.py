import sys
import os
from os import listdir
from os.path import isfile, join
import numpy as np
import subprocess
#from scipy import stats

def str2bool(v):
  return v.lower() in ("yes", "true", "t", "1")

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

    if opts['nsig'] != None:
      dat = outlierReject(dat, float(opts['nsig']))

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

def getSystemHosts():
  return "ip-10-1-3-1 ip-10-1-3-2 ip-10-1-3-3 ip-10-1-3-4 ip-10-1-3-5"

def getHarnessHosts():
  return "ip-10-1-2-1 ip-10-1-2-2 ip-10-1-2-3 ip-10-1-2-4 ip-10-1-2-5"

def getNextVal(y, n, indlo, indhi, probSum, probTarget):

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
    
def getConfidenceInterval(x, y, perc):
    
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
      indlo, indhi, probSum, stop = getNextVal(y, n, indlo, indhi, probSum, probTarget)
      print ' sum(1) = ' + str(probSum) + ' target = ' + str(probTarget)
      
    return x[indmax], x[indlo], x[indhi]

#def mode(arr):
#  return scipy.stats.mode(arr)
  
#print str(getSystemHosts())
