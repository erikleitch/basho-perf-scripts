import scipy.interpolate as int;
import numpy as np;
import matplotlib.pyplot as plt;
from matplotlib import rcParams;
from mpl_toolkits.mplot3d import Axes3D;
import sys
import os

defaultFontsize=14

def getScalesAndUnits(fileNames):

  mxs = []
  iFile=0
  ncolmax=0

  for file in fileNames:
    dat = np.loadtxt(file);
    nline = np.shape(dat)[0];
    ncol  = np.shape(dat)[1];
    if ncol > ncolmax:
      ncolmax = ncol;

  mxs =[]
  for i in range(2,ncol):
    mxs.append([])

  for file in fileNames:
    dat = np.loadtxt(file);
    nline = np.shape(dat)[0];
    ncol  = np.shape(dat)[1];

    for i in range(2,ncol):
      mxs[i-2].append(np.max(dat[0:nline,i]))

  units  = []
  scales = []
  maxs = []

  for i in range(2,ncol):
    index = i-2
    if np.max(mxs[index]) > 1e9:
      scales.append(1e9)
      units.append('G')
      maxs.append(np.max(mxs[index])/1e9)
    elif np.max(mxs[index]) > 1e6:
      scales.append(1e6)
      units.append('M')
      maxs.append(np.max(mxs[index])/1e6)
    elif np.max(mxs[index]) > 1e3:
      scales.append(1e3)
      units.append('K')
      maxs.append(np.max(mxs[index])/1e3)
    else:
      scales.append(1)
      units.append('')
      maxs.append(np.max(mxs[index]))

  return scales, units, maxs

def getSubplots(fileNames, overplot):

  ncolmax = 0
  for file in fileNames:
    dat = np.loadtxt(file);
    nline = np.shape(dat)[0];
    ncol  = np.shape(dat)[1];
    if ncol > ncolmax:
      ncolmax = ncol;

  axes=[None] * (ncolmax-2)

  nColPerRow=ncolmax-2
  nFile=np.shape(fileNames)[0];

  if overplot:
    for iCol in range(0,nColPerRow):
      ax = fig.add_subplot(1, nColPerRow, iCol+1, projection='3d');
      axes[iCol] = []
      for iFile in range(0,nFile):
        axes[iCol].append(ax)
  else:
    for iFile in range(0,nFile):
      dat  = np.loadtxt(fileNames[iFile]);
      nCol = np.shape(dat)[1] - 2;

      # Iterate over all columns actually present in this file, adding
      # a subplot for each one

      for iCol in range(0,nCol):
        currSubplotInd=nColPerRow * iFile + iCol + 1
        if axes[iCol] == None:
          axes[iCol] = []
        axes[iCol].append(fig.add_subplot(nFile, nColPerRow, currSubplotInd, projection='3d'));

  return axes

def getData(fileName, index, interp):

  dat = np.loadtxt(fileName);
  nline = np.shape(dat)[0];
  x = dat[0:nline,0];
  nx = np.size(np.unique(x));
  y = dat[0:nline,1];
  ny = np.size(np.unique(y));
  [d, unit] = getDataAndUnits(dat, nline, index);
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
  if nx > 3 and ny > 3:
    z2=int.griddata(points, d, (x2, y2), method=interp)
  else:
    z2=int.griddata(points, d, (x2, y2), method=interp)
  return x2, y2, z2, unit

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

def makeSubPlot(fileName, index, ax, doHold, Color, xlabel, ylabel, zlabel, scale, unit, maxVal, interp):

  x,y,z,unit2 = getData(fileName, index, interp);
  plt.hold(doHold);
  ax.plot_surface(x, y, z/scale, color=Color);
  ax.set_zlabel('\n' + zlabel + ' (' + unit + ')', fontsize=defaultFontsize);
  ax.set_zlim(0, maxVal*1.1);
  ax.set_xlabel('\n' + xlabel, fontsize=defaultFontsize);
  ax.set_ylabel('\n' + ylabel, fontsize=defaultFontsize);
  ax.tick_params(labelsize=defaultFontsize)

def makeSubPlotTwo(fileName, fileName2, index, action, ax, doHold, Color, xlabel, ylabel, zlabel, scale, unit, maxVal, interp):

  print 'Inside sp2 with action = ' + str(action)
  
  x1,y1,z1,unitIgnore = getData(fileName,  index, interp);
  x2,y2,z2,unitIgnore = getData(fileName2, index, interp);
  plt.hold(doHold);
  if action == '/':
    plotunit = 'ratio'
  else:
    plotunit = unit
  if action == '-':
    ax.plot_surface(x1, y1, (z1 - z2)/scale, color=Color);
  elif action == '+':
    ax.plot_surface(x1, y1, (z1 + z2)/scale, color=Color);
  elif action == '/':

    r = z1/z2;
    ax.plot_surface(x1, y1, r, color=Color);
    maxVal = np.max(z1/z2);
    ax.set_zlabel('\n' + zlabel + ' (' + plotunit + ')');
    ax.set_zlim(0, maxVal*1.1);
  elif action == '*':
    ax.plot_surface(x1, y1, z1 * z2, color=Color);
  ax.set_xlabel('\n' + xlabel);
  ax.set_ylabel('\n' + ylabel);
#  retick(ax, 'y')

def plotFiles(files, plotwithfiles, plotwithaction, axes, colors, scales, units, maxs, param1, param2, interp):

  nfile=np.shape(files)[0]
  ncolor=np.shape(colors)[0];

  for iFile in range(0,nfile):

    if plotwithfiles != None:
      if iFile == 0:
        if plotwithaction == 'p':
          plotData(files, iFile, False, axes, 'c', scales, units, maxs, param1, param2, interp)
          plotData(plotwithfiles, iFile, True, axes, 'm', scales, units, maxs, param1, param2, interp)
        else:
          plotDataTwo(files, plotwithfiles, plotwithaction, iFile, False, axes, 'c', scales, units, maxs, param1, param2, interp)
      else:
        if plotwithaction == 'p':
          plotData(files, iFile,  True, axes, 'c', scales, units, maxs, param1, param2, interp)
          plotData(plotwithfiles, iFile, True, axes, 'm', scales, units, maxs, param1, param2, interp)
        else:
          plotDataTwo(files, plotwithfiles, plotwithaction, iFile, True, axes, 'c', scales, units, maxs, param1, param2, interp)
    else:
      if iFile == 0:
        plotData(files, iFile, False, axes, colors[iFile % ncolor], scales, units, maxs, param1, param2, interp)
      else:
        plotData(files, iFile,  True, axes, colors[iFile % ncolor], scales, units, maxs, param1, param2, interp)

def plotData(fileNames, iFile, doHold, axes, Color, scales, units, maxs, param1, param2, interp):

  naxes=np.shape(axes)[0]

  makeSubPlot(fileNames[iFile], 2, axes[0][iFile], doHold, Color, param1, param2, 'Ops/sec', scales[0], units[0], maxs[0], interp);

  if naxes > 1 and axes[1] != None:
    makeSubPlot(fileNames[iFile], 3, axes[1][iFile], doHold, Color, param1, param2, 'Writes/sec', scales[1], units[1], maxs[1], interp);

  if naxes > 2 and axes[2] != None:
    makeSubPlot(fileNames[iFile], 4, axes[2][iFile], doHold, Color, param1, param2, 'Bytes/sec', scales[2], units[2], maxs[2], interp);

def plotDataTwo(fileNames, fileNames2, action, iFile, doHold, axes, Color, scales, units, maxs, param1, param2, interp):

  naxes=np.shape(axes)[0]

  makeSubPlotTwo(fileNames[iFile], fileNames2[iFile], 2, action, axes[0][iFile], doHold, Color, param1, param2, 'Ops/sec', scales[0], units[0], maxs[0], interp);

  if naxes > 1 and axes[1] != None:
    makeSubPlotTwo(fileNames[iFile], fileNames2[iFile], 3, action, axes[1][iFile], doHold, Color, param1, param2, 'Writes/sec', scales[1], units[1], maxs[1], interp);

  if naxes > 2 and axes[2] != None:
    makeSubPlotTwo(fileNames[iFile], fileNames2[iFile], 4, action, axes[2][iFile], doHold, Color, param1, param2, 'Bytes/sec', scales[2], units[2], maxs[2], interp);

def getDataAndUnits(dat, nline, index):
  d=dat[0:nline,index];
  max=np.max(d);
  unit='';
  
  if False:
    if max > 1e6:
      d = d/1e6;
      unit = 'M';
    elif max > 1e3:
      d = d/1e3;
      unit = 'K';
  
  return [d, unit];

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

#------------------------------------------------------------
# Main Script starts here
#------------------------------------------------------------

files    = sys.argv[1].split(' ');

plotWith = getOptArgs(sys.argv, 'plotwith', None)
if plotWith != None:
  plotWith = plotWith.split(' ')

plotWithAction = getOptArgs(sys.argv, 'plotwithaction', 'p')
  
labels     = getOptArgs(sys.argv, 'labels', '').split(';')
title      = getOptArgs(sys.argv, 'title',  '')
param1     = getOptArgs(sys.argv, 'param1', 'x (unspecified)')
param2     = getOptArgs(sys.argv, 'param2', 'y (unspecified)')
interp     = getOptArgs(sys.argv, 'interp',  'linear')
figsizestr = getOptArgs(sys.argv, 'figsize', '18,12')

figsizearr=figsizestr.split(',')
figsize=(np.int(figsizearr[0]), np.int(figsizearr[1]))

fig = plt.figure(figsize=figsize)
fig.set_facecolor('white');

# Append empty labels out to the length of the files var, if not enough labels were provided

nFile=np.shape(files)[0]
nLabel=np.shape(labels)[0]
for i in range(nLabel, nFile):
  labels.append('')
  
print str(files) + ' ' + str(labels)

colors=['b', 'c', 'm', 'g', 'y', 'k'];
cellsizes=[10, 10];

scales,units,maxs  = getScalesAndUnits(files);
plt.axis('off')
plt.title(title)
axes = getSubplots(files, False)

plotFiles(files, plotWith, plotWithAction, axes, colors, scales, units, maxs, param1, param2, interp)

top   =plt.rcParams['figure.subplot.top']
bottom=plt.rcParams['figure.subplot.bottom']
hspace=plt.rcParams['figure.subplot.hspace']
yrange=top-bottom

yint = (yrange - hspace) / nFile
if nFile > 1:  sint = hspace / (nFile-1)
else:  sint = hspace / (nFile)

for i in range(0,nFile):
  y = top - (i + 0.5)*yint - i*sint
  plt.figtext(0.03, y, labels[i], fontsize=defaultFontsize)

plt.show();
#plt.savefig('ringComp.png', format='png', dpi=fig.dpi);
