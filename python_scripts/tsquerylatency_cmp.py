import numpy as np
import matplotlib.pyplot as plt
import scipy.interpolate as int;
from mpl_toolkits.mplot3d import Axes3D;
import sys
import os
from os import listdir
from os.path import isfile, join
import scipy.stats as stats

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

def getRawDiffProfilerOutput(fileName1, fileName2):
  d1 = parseProfilerOutput(fileName1, {})
  d2 = parseProfilerOutput(fileName2, {})
  cols  = []
  rows  = []
  rs  = []
  us  = []
  for key in d1.keys():
    s = key.split('_')
    if s[0] == 'query':
      if key in d2.keys():
        cols.append(float(s[1]))
        rows.append(np.log10(float(s[3])))
        rs.append((d2[key]['usec'] - d1[key]['usec'])/d1[key]['usec'])
        us.append(d2[key]['usec'] - d1[key]['usec'])

  return cols,rows,rs,us,d1,d2

def getProfilerOutput(fileName):
  d = parseProfilerOutput(fileName, {})

  cols  = []
  rows  = []
  us  = []
  for key in d.keys():
    s = key.split('_')
    if s[0] == 'query':
      cols.append(float(s[1]))
      bytes = s[2]
      rows.append(np.log10(float(s[3])))
      ntrial = np.int(s[4])
      us.append(np.log10(d[key]['usec']/ntrial))

  x,y,z = getGriddedData(cols, rows, us)
  return x,y,z,bytes

def getDiffProfilerOutput(fileName1, fileName2):
  c, r, fracdiff, lindiff, d1, d2 = getRawDiffProfilerOutput(fileName1, fileName2)

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

  print 'av = ' + str(av)
  
  pte = 1.0 - stats.chi2.cdf(chi2, ndof)

  statstr = '$\mu$ = ' + ("%.0f" % av) + '%, $\chi^2_{' + str(ndof) + '}$ = ' + ("%.2f" % chi2) + ' (PTE = ' + ("%.2g" % pte) + ')'
  
  x,y,frac = getGriddedData(c, r, fracdiff)
  x,y,diff = getGriddedData(c, r, lindiff)
                  
  return x,y,frac,diff,statstr

def getGriddedData(x,y,d):
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
  z2=int.griddata(points, d, (x2, y2), method='cubic');
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
  ax.set_ylabel('\nRows per query',fontsize=defaultFontsize)

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

#-----------------------------------------------------------------------
# Main script starts here
#-----------------------------------------------------------------------

print 'Args = ' + str(sys.argv)

dataDir = str(os.environ['RIAK_TEST_BASE']) + '/data/'

file1  = sys.argv[1]
file2  = sys.argv[2]
title1 = sys.argv[3].replace("\\n", "\n")
title2 = sys.argv[4].replace("\\n", "\n")
title3 = sys.argv[5].replace("\\n", "\n")

if np.size(sys.argv) > 6:
  zlabel3 = sys.argv[6].replace("\\n", "\n")
else:
  zlabel=None

overplot = str2bool(getOptArgs(sys.argv, 'overplot', True))
diffplot = str2bool(getOptArgs(sys.argv, 'diffplot', False))
figfile  = getOptArgs(sys.argv, 'figfile',  None)
  
x1, y1, z1, bytes1 = getProfilerOutput(file1)
x2, y2, z2, bytes2 = getProfilerOutput(file2)

if not overplot:
  xd, yd, frac, diff, statstr = getDiffProfilerOutput(file1, file2)

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

print 'zmin = ' + str(zmin) + ' zmax = ' + str(zmax)

ax = fig.add_subplot(1,3,1, projection='3d')
makePlot(ax, x1, y1, z1, zmin, zmax, 'c', title1)

ax = fig.add_subplot(1,3,2, projection='3d')
makePlot(ax, x2, y2, z2, zmin, zmax, 'm', title2)

ax = fig.add_subplot(1,3,3, projection='3d')

if overplot:
  makePlot(ax, x1, y1, z1, zmin, zmax, 'c', title3)
  plt.hold(True)
  makePlot(ax, x2, y2, z2, zmin, zmax, 'm')
else:
  if diffplot:
    nx = np.shape(diff)[0]
    ny = np.shape(diff)[1]

    for ix in range(0,nx):
      for iy in range(0,ny):
        diff[ix][iy] = np.power(10, z2[ix][iy]) - np.power(10,z1[ix][iy])

    diff = diff / 1e3
    zmin = np.min(diff)
    zmax = np.max(diff)
    zrng = zmax - zmin
    zmin -= zrng * 0.1
    zmax += zrng * 0.1
    print 'z1 = ' + str(z1) + ' z2 = ' + str(z2) + ' Diff = ' + str(diff)
    makePlot(ax, x1, y1, diff, zmin, zmax, 'y', title3 + '\n' + statstr, False, zlabel3)
  else:
    makePlot(ax, x1, y1, frac*100, -100, 100, 'y', title3 + '\n' + statstr, False, zlabel3)

if figfile != None:
  plt.tight_layout(w_pad=2,pad=5)
  fig.savefig(figfile);
  
print 'Here'

plt.show()
