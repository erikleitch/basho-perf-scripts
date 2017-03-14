import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl
import scipy.interpolate as int;
from mpl_toolkits.mplot3d import Axes3D;
import sys
from os import listdir
from os.path import isfile, join
import scipy.stats as stats
import os
import erltestutil as etu

defaultFontsize=22

def getFiles(fileName):
  return fileName.split(' ')

def getLine(label, content):
  for line in content:
    if line.split(' ')[0] == label:
      return line.split(' ')
  return []

#------------------------------------------------------------
# Parse profiler output potentially from multiple files
#------------------------------------------------------------

def parseProfilerOutput(fileName, labelDict):
  files = getFiles(fileName)

  for file in files:
    labelDict = parseProfilerOutputVals(file, labelDict)

  for key in labelDict.keys():
    labelDict[key]['usec'] = np.mean(labelDict[key]['usecvals'])
    n = np.size(labelDict[key]['usecvals'])
    labelDict[key]['usecstd'] = np.std(labelDict[key]['usecvals'], ddof=1) / np.sqrt(n)

  return labelDict

#------------------------------------------------------------
# Parse a profiler output file and return a dictionary containing all encountered tags
#------------------------------------------------------------

def parseProfilerOutputVals(fileName, labelDict):

  with open(fileName) as f:
    content = f.readlines()

    nline = len(content)

    totalcount = getLine('totalcount', content)
    labels     = getLine('label',      content)
    counts     = getLine('count',      content)
    usec       = getLine('usec',       content)

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

def getRawDiffProfilerOutput(fileName1, fileName2, op, opts):

  print 'Here'
  if opts['dirs']:
    dat1 = etu.getDat(fileName1, 'mean',  'ncol', 'nbyte', opts)
    s1   = etu.getDat(fileName1, 'std',   'ncol', 'nbyte', opts)
    dat2 = etu.getDat(fileName2, 'mean',  'ncol', 'nbyte', opts)
    s2   = etu.getDat(fileName2, 'std',   'ncol', 'nbyte', opts)

    print 'd1 = ' + str(dat1)
    print 'd2 = ' + str(dat2)

    print 's1 = ' + str(s1)
    print 's2 = ' + str(s2)
    
    d1 = {}
    d2 = {}
    nline = np.shape(dat1)[0]

    cols = []
    btes = []
    us   = []
  
    for i in range(0,nline):
      d1[i] = {}
      d1[i]['usec']    = dat1[i,2]
      d1[i]['usecstd'] = s1[i,2]
      d2[i] = {}
      d2[i]['usec']    = dat2[i,2]
      d2[i]['usecstd'] = s2[i,2]
      cols.append(dat1[i,0])
      btes.append(dat1[i,1])

      if op == '-':
        us.append((d2[i]['usec'] - d1[i]['usec'])/d1[i]['usec'])
      elif op == '/':
        us.append(d2[i]['usec']/d1[key]['usec'])
        
  else:
    d1 = parseProfilerOutput(fileName1, {})
    d2 = parseProfilerOutput(fileName2, {})
    print 'd1 = ' + str(d1)
    print 'd2 = ' + str(d2)
    cols  = []
    btes  = []
    us    = []
    for key in d1.keys():
      s = key.split('_')
      if s[0] == 'put':

        if key in d2.keys():
          cols.append(float(s[1]))
          btes.append(np.log10(float(s[2])))
          if op == '-':
            us.append((d2[key]['usec'] - d1[key]['usec'])/d1[key]['usec'])
          elif op == '/':
            us.append(d2[key]['usec']/d1[key]['usec'])

  return cols, btes, us, d1, d2

def getProfilerOutput(fileName, opts):

  if opts['dirs']:
    d = etu.getDat(fileName, 'mean', 'ncol', 'nbyte', opts)
    print 'd = ' + str(d)
    cols = d[:,0]
    btes = np.log10(d[:,1])
    us   = np.log10(d[:,2])
  else:
    d = parseProfilerOutput(fileName, {})

    cols  = []
    btes  = []
    us    = []
    for key in d.keys():
      s = key.split('_')
      if s[0] == 'put':
        cols.append(float(s[1]))
        btes.append(np.log10(float(s[2])))
        ntrial = np.int(s[3])
        us.append(np.log10(d[key]['usec']/ntrial))
        
  x,y,z = getGriddedData(cols, btes, us, opts)
  
  return x,y,z

def getDiffProfilerOutput(fileName1, fileName2, op, opts):

  c, r, diff, d1, d2 = getRawDiffProfilerOutput(fileName1, fileName2, op, opts)

  if op == '-':
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
        
        pte = 1.0 - stats.chi2.cdf(chi2, ndof)
        
        if np.isnan(chi2):
          statstr = '$\mu$ = ' + ("%.0f" % av) + '%'
        else:
          statstr = '$\mu$ = ' + ("%.0f" % av) + '%, $\chi^2_{' + str(ndof) + '}$ = ' + ("%.2f" % chi2) + ' (PTE = ' + ("%.2g" % pte) + ')'

  else:
    statstr = ''
    
  x,y,z = getGriddedData(c, r, diff, opts)
  return x,y,z,statstr

def getGriddedData(x,y,d,opts):
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
  z2=int.griddata(points, d, (x2, y2), method=opts['interp']);
  return x2, y2, z2

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

def makePlot(ax, x, y, z, zmin, zmax, Color, Title=None, retickz=True, zlabel=None):
  ax.plot_surface(x, y, z, color=Color);
  if Title != None:
    ax.set_title(Title,fontsize=defaultFontsize)

  if zlabel:
    ax.set_zlabel('\n\n' + zlabel,fontsize=defaultFontsize)
  else:
    ax.set_zlabel('\n\n' + 'Latency ($\mu$sec)',fontsize=defaultFontsize)
    
  ax.set_xlabel('\nColumns',fontsize=defaultFontsize)
  ax.set_ylabel('\nBytes per column',fontsize=defaultFontsize)

  ax.set_zlim(zmin, zmax)
    
  retick(ax, 'y')

  if retickz:
    retick(ax, 'z')

  ax.tick_params(labelsize=defaultFontsize)
    
def str2bool(v):
  return v.lower() in ("yes", "true", "t", "1")

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

print 'Args = ' + str(sys.argv)

dataDir = str(os.environ['RIAK_TEST_BASE']) + '/data/'

file1  = sys.argv[1]
file2  = sys.argv[2]
title1 = sys.argv[3]
title2 = sys.argv[4]
title3 = sys.argv[5]

if np.size(sys.argv) > 6:
  zlabel3 = sys.argv[6]
else:
  zlabel=None

overplot = str2bool(getOptArgs(sys.argv, 'overplot', 'true'))
figfile  = getOptArgs(sys.argv, 'figfile',  None)

opts = {}
opts['dirs'] = str2bool(getOptArgs(sys.argv, 'dirs', 'false'))
opts['nsig'] = etu.getOptArgs(sys.argv, 'nsig', 0.0)
opts['interp'] = etu.getOptArgs(sys.argv, 'interp', 'cubic')

x1, y1, z1 = getProfilerOutput(file1, opts)
x2, y2, z2 = getProfilerOutput(file2, opts)

op = getOptArgs(sys.argv, 'op', '-')

if not overplot:
  xd, yd, diff, statstr = getDiffProfilerOutput(file1, file2, op, opts)

fig = plt.figure(figsize=(25,8))
fig.set_facecolor('white');

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
zmin -= zrng * 0.1
zmax += zrng * 0.1

ax = fig.add_subplot(1,3,1, projection='3d')
makePlot(ax, x1, y1, z1, zmin, zmax, 'c', title1)

ax = fig.add_subplot(1,3,2, projection='3d')
makePlot(ax, x2, y2, z2, zmin, zmax, 'm', title2)

if True:

  ax = fig.add_subplot(1,3,3, projection='3d')

  if overplot:
    makePlot(ax, x1, y1, z1, zmin, zmax, 'c', title3)
    plt.hold(True)
    makePlot(ax, x2, y2, z2, zmin, zmax, 'm')
  else:
    if op == '-':
      makePlot(ax, x2, y2, diff*100, -100, 100, 'y', title3 + '\n' + statstr, False, zlabel3)
    else:
      makePlot(ax, x2, y2, diff, np.min(diff), np.max(10), 'y', title3 + '\n' + statstr, False, zlabel3)

  print 'x2   = ' + str(x2)
  print 'y2   = ' + str(np.power(10, y2))
  print 'diff = ' + str(diff)
  

if figfile != None:
  plt.tight_layout(w_pad=2,pad=5)
  fig.savefig(figfile)
  
plt.show()
