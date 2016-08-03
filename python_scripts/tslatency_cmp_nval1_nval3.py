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

def getProfilerOutput(fileName):
  d = parseProfilerOutput(fileName, {})
  pbytes = []
  pcols  = []
  putus  = []
  for key in d.keys():
    s = key.split('_')
    if s[0] == 'put':
      pcols.append(float(s[1]))
      pbytes.append(np.log10(float(s[2])))
      ntrial = np.int(s[3])
      putus.append(np.log10(d[key]['usec']/ntrial))

  x,y,z = getGriddedData(pcols, pbytes, putus)
  return x,y,z

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
    ticks.append(np.float(i))
    ticklabels.append('$10^{' + ("%d" % i) + '}$')

  if axname == 'x':
    ax.set_xticks(ticks)
    ax.set_xticklabels(ticklabels,fontsize=14)
  elif axname == 'y':
    ax.set_yticks(ticks)
    ax.set_yticklabels(ticklabels,fontsize=14)
  else:
    ax.set_zticks(ticks)
    ax.set_zticklabels(ticklabels,fontsize=14)

  return

def makePlot(ax, x, y, z, Color, Title=None):
  ax.plot_surface(x, y, z, color=Color);
  if Title != None:
    ax.set_title(Title,fontsize=16)
  ax.set_zlabel('Latency ($\mu$sec)',fontsize=16)
  ax.set_xlabel('Columns',fontsize=16)
  ax.set_ylabel('Bytes per column)',fontsize=16)

  retick(ax, 'y')
  retick(ax, 'z')

x1, y1, z1 = getProfilerOutput('../data/tslatency_2d_nval1.txt')
x2, y2, z2 = getProfilerOutput('../data/tslatency_2d_nval3.txt')

fig = plt.figure()
fig.set_facecolor('white');

ax = fig.add_subplot(1,3,1, projection='3d')
makePlot(ax, x1, y1, z1, 'c', 'TS Put Latency nval=1')
ax = fig.add_subplot(1,3,2, projection='3d')
makePlot(ax, x2, y2, z2, 'm', 'TS Put Latency nval=3')
ax = fig.add_subplot(1,3,3, projection='3d')
makePlot(ax, x1, y1, z1, 'c', 'TS Put Latency nval=1 vs nval=3')
plt.hold(True)
makePlot(ax, x2, y2, z2, 'm')

plt.show()
