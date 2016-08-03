import numpy as np
import matplotlib.pyplot as plt
import scipy.interpolate as int;
from mpl_toolkits.mplot3d import Axes3D;

def getLine(label, content):
  for line in content:
    if line.split(' ')[0] == label:
      return line.split(' ')
  return []

def parseProfilerOutput(fileName, labelDict):

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
        labelDict[label] = {}
        labelDict[label]['usec']  = np.float(usec[i+1])
        labelDict[label]['count'] = np.int(counts[i+1])
  return labelDict

def getProfilerOutput(fileName, ax, title):
  d = parseProfilerOutput(fileName, {})
  spcols = getPutNcols(d)

  ncols  = np.size(spcols)
  legend = []
  for i in range(0, ncols):
    x,y = getColData(d, spcols[i])
    legend.append('Ncol = ' + str(spcols[i]))
    plt.plot(x,y)
    plt.hold(True)

  ax.set_xscale('log')
  ax.set_yscale('log')
#  plt.xticks(ax.get_xticks(), fontsize=16)
#  plt.yticks(ax.get_yticks(), fontsize=16)
  plt.xlabel('Total bytes per write',fontsize=16)
  plt.ylabel('Put Latency ($\mu$sec)',fontsize=16)
  plt.title(title,fontsize=16)
  plt.legend(legend, loc='upper left')
  return

def getPutNcols(d):
  pcols = []
  for key in d.keys():
    s = key.split('_')
    if s[0] == 'put':
      pcols.append(float(s[1]))

  up = np.unique(pcols)
  ps = np.argsort(up)
  ncols  = np.size(up)
  spcols = np.ndarray(ncols, np.integer)

  for i in range(0, ncols):
    spcols[i] = up[ps[i]]

  return spcols

def getColData(d, cols):
  x = []
  y = []
  for key in d.keys():
    s = key.split('_')
    if s[0] == 'put':
      ncol = float(s[1])
      if ncol == cols:
        ntrial = np.int(s[3])
        usec = d[key]['usec']/ntrial
        totalbytes  = np.float(s[2]) * ncol
        print 'Found ncol = ' + str(ncol) + ' ntrial = ' + str(ntrial) + ' usec/trial = ' + str(usec) + ' bytes/col = ' + s[2] + ' total bytes = ' + str(totalbytes)
        x.append(totalbytes)
        y.append(usec)

  inds  = np.argsort(x)
  ndata = np.size(x)
  rx = np.ndarray(ndata, np.float)
  ry = np.ndarray(ndata, np.float)

  for i in range(0, ndata):
    rx[i] = x[inds[i]]
    ry[i] = y[inds[i]]

  return rx, ry

fig = plt.figure()
fig.set_facecolor('white');

ax = fig.add_subplot(1,2,1)
ax.tick_params(axis='both', which='major', labelsize=16)
getProfilerOutput('../data/tslatency_splits_nval1.txt', ax, 'TS Put Latency nval=1')

ax = fig.add_subplot(1,2,2)
ax.tick_params(axis='both', which='major', labelsize=16)
getProfilerOutput('../data/tslatency_splits_nval3.txt', ax, 'TS Put Latency nval=3')

plt.show()
