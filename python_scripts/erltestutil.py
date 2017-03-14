import sys
import os
from os import listdir
from os.path import isfile, join
import numpy as np
import subprocess
import scipy.interpolate as interp;
import scipy.stats as stats

def getSubplotDims(ntag):

    ns = np.int(np.sqrt(float(ntag)))

    if ns == 1:
        ny = 1
        nx = ntag
    else:
        ny = ns
        nx = np.ceil(float(ntag)/ns)

    return nx, ny

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
        intArr.append(np.int(str))
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
    iarr.append(np.int(larr[i]))
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

#------------------------------------------------------------
# Given a directory path, assuming it contains dumps of
# operational latencies, read all files in that directory and
# extract the relevant stats
#------------------------------------------------------------

def getGrid(mypath, stat, param1, param2, opts):

    files = getFiles(mypath)
    vals = {}

    for file in files:
        args = file.split('_')
        hasPar1 = False
        hasPar2 = False
        for arg in args:
            if param1 in arg:
                hasPar1 = True
                sargs = arg.split(param1)
                par1 = np.int(sargs[1])
            if param2 in arg:
                hasPar2 = True
                sargs = arg.split(param2)
                par2 = np.int(sargs[1])

        if hasPar1 and hasPar2:
            val = getStat(mypath, file, stat, opts)
            if par1 not in vals.keys():
                vals[par1] = {}
            vals[par1][par2] = val

    return vals

def gridToDat(vals):

    nline = 0
    skeys1 = np.sort(vals.keys())
    
    for skey1 in skeys1:
        skeys2 = np.sort(vals[skey1].keys())
        for skey2 in skeys2:
            nline += 1

    dat = np.ndarray(shape=(nline,3), dtype=np.double)

    iLine=0

    for skey1 in skeys1:
        skeys2 = np.sort(vals[skey1].keys())
        for skey2 in skeys2:
            dat[iLine][0] = skey1
            dat[iLine][1] = skey2
            dat[iLine][2] = vals[skey1][skey2]
            iLine = iLine+1

    return dat

def getDat(mypath, stat, param1, param2, opts):
    grid = getGrid(mypath, stat, param1, param2, opts)
    return gridToDat(grid)

#------------------------------------------------------------
# Given a file, compute the requested stat from it
#------------------------------------------------------------

def getStat(path, file, stat, opts):

    dat = np.loadtxt(path + '/' + file);

    return getStatArr(dat, stat, opts)

def getStatArr(dat, stat, opts):
    
    if ('nsig' in opts.keys()) and float(opts['nsig']) > 0.0:
      dat = outlierReject(dat, float(opts['nsig']))

    nline = np.shape(dat)[0];
    if stat == 'mean':
        return np.mean(dat)
    elif stat == 'std':
        return np.std(dat, ddof=1)
    elif stat == 'meanerr':
        return np.std(dat, ddof=1)/np.sqrt(np.size(dat))
    elif stat == 'median':
        return np.median(dat)
    elif stat == 'data':
      return dat
    elif 'conf' in stat:
      perc = float(stat.split('conf')[0])/100
      return interval(dat, int(opts['bins']), perc)
    elif 'perc' in stat:
      perc = float(stat.split('perc')[0])
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

#=======================================================================
# Profiler file output parsing
#=======================================================================

#------------------------------------------------------------
# Get the mean and error in the mean of the same tags over all files
#------------------------------------------------------------

def parseProfilerOutput(files, labelDict):

    for file in files:
        print 'file = ' + str(file)
        labelDict = parseProfilerOutputSingle(file, labelDict)

    for key in labelDict.keys():
        labelDict[key]['usec'] = np.mean(labelDict[key]['usecvals'])
        n = np.size(labelDict[key]['usecvals'])
        labelDict[key]['usecstd'] = np.std(labelDict[key]['usecvals'], ddof=1) / np.sqrt(n)

    return labelDict

#------------------------------------------------------------
# Parse a profiler output file and return a dictionary containing all encountered tags
#------------------------------------------------------------

def parseProfilerOutputSingle(fileName, labelDict):

  with open(fileName) as f:
    content = f.readlines()

    nline = len(content)

    totalcount = getProfLine('totalcount', content)
    labels     = getProfLine('label',      content)
    counts     = getProfLine('count',      content)
    usec       = getProfLine('usec',       content)

  if len(labels) != 0:
    for i in range(1, len(labels)):
      label = labels[i].replace("'", "")
      label = label.replace("\n", "")

      if len(label) > 0:
        if label not in labelDict.keys():
          labelDict[label] = {}
          labelDict[label]['usecvals'] = []
          labelDict[label]['count'] = []
          
        labelDict[label]['usecvals'].append(np.float(usec[i+1]))
        labelDict[label]['count'].append(np.int(counts[i+1]))
          
  return labelDict

#------------------------------------------------------------
# Extract a parameter value from a string of the form 'prefix_param1VAL1_param2VAL2_param3VAL3'
#------------------------------------------------------------

def getParam(field, paramname, doLog):

    ss = field.split(paramname)

    if doLog:
        val = np.log10(float(ss[1]))
    else:
        val = float(ss[1])

    return val

#------------------------------------------------------------
# Given a list of files, get the mean of all tags and return
# a grid over the requested parameters
#------------------------------------------------------------

def getProfilerOutputGrid(fileName, opts):

    d = parseProfilerOutput(fileName, {})

    print 'd = ' + str(d)
    cols  = []
    rows  = []
    us    = []
    
    for key in d.keys():

        # For each key, split into subfields
        
        s = key.split('_')

        xval   = None
        yval   = None
        niter  = 1
        
        xparam    = opts['xparam']
        yparam    = opts['yparam']
        iterparam = opts['iparam']
    
        for field in s:

            if xparam in field:
                xval = getParam(field, xparam, opts['logx'])

            if yparam in field:
                yval = getParam(field, yparam, opts['logy'])

            if iterparam in field:
                niter = int(getParam(field, iterparam, False))
                print 'Seartching for iter = ' + str(iterparam) + 'fieil = ' + str(field) + ' niter = ' + str(niter)

        if xparam != None and yparam != None:
            cols.append(xval)
            rows.append(yval)
            if opts['logz']:
                us.append(np.log10(d[key]['usec']/niter))
            else:
                print 'Appending ' + str(d[key]['usec']) + '/' + str(niter) + ' for key ' + key
                
                us.append(d[key]['usec']/niter)

    x,y,z = getGriddedData(cols, rows, us, opts)

    return x,y,z

def getRawDiffProfilerOutput(fileName1, fileName2, opts):

    d1 = parseProfilerOutput(fileName1, {})
    d2 = parseProfilerOutput(fileName2, {})

    cols  = []
    rows  = []
    fus   = []
    dus   = []
    
    for key in d1.keys():

        # For each key, split into subfields
        
        s = key.split('_')

        xval   = None
        yval   = None
        niter  = 1
        
        xparam    = opts['xparam']
        yparam    = opts['yparam']
        iterparam = opts['iparam']
    
        for field in s:

            if xparam in field:
                xval = getParam(field, xparam, opts['logx'])

            if yparam in field:
                yval = getParam(field, yparam, opts['logy'])

            if iterparam in field:
                niter = int(getParam(field, iterparam, False))
                print 'Seartching for iter = ' + str(iterparam) + 'fieil = ' + str(field) + ' niter = ' + str(niter)

        if xparam != None and yparam != None:
            if key in d2.keys():
                cols.append(xval)
                rows.append(yval)
                zval1 = d1[key]['usec']/niter
                zval2 = d2[key]['usec']/niter
                
                fus.append((zval2 - zval1)/zval1)
                dus.append(zval2 - zval1)

    return cols, rows, fus, dus, d1, d2

#------------------------------------------------------------
# Get the raw difference between two sets of files, and
# convert to gridded data as well as stats
#------------------------------------------------------------

def getDiffProfilerOutput(fileName1, fileName2, opts):

    c, r, fracdiff, lindiff, d1, d2 = getRawDiffProfilerOutput(fileName1, fileName2, opts)

    print 'fracdiff = ' + str(fracdiff) + ' max = ' + str(np.max(fracdiff)) + ' min = ' + str(np.min(fracdiff))
    
    chi2 = 0.0
    ndof = 0
    av = 0.0
    for key in d1.keys():
        if key in d2.keys():
            v1 = d1[key]['usec']
            s1 = d1[key]['usecstd']
            v2 = d2[key]['usec']
            s2 = d2[key]['usecstd']
            chi2 += np.power((v1 - v2), 2.0) / (np.power(s1, 2.0) + np.power(s2, 2.0))
            ndof += 1
            val = 100*(v2 - v1)/v1
            av += (val - av)/(ndof)

    print 'av = ' + str(av) + 'shape = ' + str(np.shape(fracdiff))
  
    pte = 1.0 - stats.chi2.cdf(chi2, ndof)

    statstr = '$\mu$ = ' + ("%.0f" % av) + '%, $\chi^2_{' + str(ndof) + '}$ = ' + ("%.2f" % chi2) + ' (PTE = ' + ("%.2g" % pte) + ')'

    x,y,frac = getGriddedData(c, r, fracdiff, opts)
    x,y,diff = getGriddedData(c, r, lindiff,  opts)

    return x,y,frac,diff,statstr

#------------------------------------------------------------
# Given sparsely sampled data, resample to a regular grid
#------------------------------------------------------------

def getGriddedData(x,y,d,opts):

    print 'Inside getGriddedData with x = ' + str(x)
    
    npoints=np.size(x);
    points = np.ndarray((npoints, 2), np.double);

    for i in range(0,npoints):
        points[i][0] = x[i];
        points[i][1] = y[i];

    ux = np.unique(x);
    uy = np.unique(y);
    
    x1=np.linspace(np.min(ux), np.max(ux), 200);
    y1=np.linspace(np.min(uy), np.max(uy), 200);
    x2,y2 = np.meshgrid(x1, y1);
    z2=interp.griddata(points, d, (x2, y2), method=opts['interp']);

    return x2, y2, z2

#------------------------------------------------------------
# Repopulate an axis with logarithmic labels
#------------------------------------------------------------

def retick(ax, axname):
  if axname == 'x':
    rng = ax.get_xlim()
  elif axname == 'y':
    rng = ax.get_ylim()
  else:
    rng = ax.get_zlim()

  mn = np.int(np.floor(rng[0]))
  mx = np.int(np.ceil(rng[1]))
  ticks = []
  ticklabels = []
  for i in range(mn, mx):
    if np.float(i) >= rng[0]:
      ticks.append(np.float(i))
      ticklabels.append('$10^{' + ("%d" % i) + '}$')

  if axname == 'x':
    ax.set_xticks(ticks)
    ax.set_xticklabels(ticklabels)
  elif axname == 'y':
    ax.set_yticks(ticks)
    ax.set_yticklabels(ticklabels)
  else:
    ax.set_zticks(ticks)
    ax.set_zticklabels(ticklabels)

  return

def getProfLine(label, content):
    for line in content:
        if line.split(' ')[0] == label:
            return line.split(' ')
    return []

def getRange2(z1, z2, frac):

    zmin1 = np.min(z1)
    zmin2 = np.min(z2)
    if zmin1 < zmin2:
        zmin = zmin1
    else:
        zmin = zmin2

    zmax1 = np.max(z1)
    zmax2 = np.max(z2)
    if zmax1 > zmax2:
      zmax = zmax1
    else:
      zmax = zmax2

    zrng = zmax - zmin
    zmin -= zrng * frac
    zmax += zrng * frac

    return zmin, zmax
  
def getRange1(z, frac):

    zmin = np.min(z)
    zmax = np.max(z)

    zrng = zmax - zmin
    zmin -= zrng * frac
    zmax += zrng * frac

    return zmin, zmax
  

