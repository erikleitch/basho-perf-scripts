import scipy.interpolate as int;
import numpy as np;
import matplotlib.pyplot as plt;
from matplotlib import rcParams;
from mpl_toolkits.mplot3d import Axes3D;
import sys
import os
import erltestutil as etu

defaultFontsize=14

def getScalesAndUnits(fileNames, scale, opts):

  mxs = []
  ncolmax=0

  iFile=0
  for file in fileNames:
    print 'iFile = ' + str(iFile)
    opts['stat'] = etu.indOrVal(opts['stats'], iFile)
    dat = getDat(file, opts)
    nline = np.shape(dat)[0];
    ncol  = np.shape(dat)[1];

    if ncol > ncolmax:
      ncolmax = ncol;

    iFile += 1

  mxs =[]
  for i in range(2,ncol):
    mxs.append([])

  iFile=0
  for file in fileNames:
    opts['stat'] = etu.indOrVal(opts['stats'], iFile)
    dat = getDat(file, opts);
    nline = np.shape(dat)[0];
    ncol  = np.shape(dat)[1];

    for i in range(2,ncol):
      mxs[i-2].append(np.max(dat[0:nline,i]))

    iFile += 1
    
  units  = []
  scales = []
  maxs   = []

  for i in range(2,ncol):
    index = i-2
    if scale:
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
    else:
      scales.append(1)
      units.append('')
      maxs.append(np.max(mxs[index]))

  return scales, units, maxs

def getSubplots(fileNames, overplot, opts):

  if opts['cmp']:
    axes=[None] * 3
    axes[0] = []
    axes[0].append(fig.add_subplot(3, 1, 1, projection='3d'));
    axes[1] = []
    axes[1].append(fig.add_subplot(3, 1, 2, projection='3d'));
    axes[2] = []
    axes[2].append(fig.add_subplot(3, 1, 3, projection='3d'));
    return axes

  ncolmax = 0
  iFile=0
  for file in fileNames:
    opts['stat'] = etu.indOrVal(opts['stats'], iFile)
    dat   = getDat(file, opts)
    nline = np.shape(dat)[0];
    ncol  = np.shape(dat)[1];
    if ncol > ncolmax:
      ncolmax = ncol;

    iFile += 1
    
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
      opts['stat'] = etu.indOrVal(opts['stats'], iFile)
      dat  = getDat(fileNames[iFile], opts);
      nCol = np.shape(dat)[1] - 2;

      # Iterate over all columns actually present in this file, adding
      # a subplot for each one

      for iCol in range(0,nCol):
        currSubplotInd=nColPerRow * iFile + iCol + 1
        if axes[iCol] == None:
          axes[iCol] = []
        axes[iCol].append(fig.add_subplot(nFile, nColPerRow, currSubplotInd, projection='3d'));

  return axes

def getDat(fileName, opts):
  if opts['dirs']:
    print 'Calling getDat with stat = ' + str(opts['stat'])
    return etu.getDat(fileName, opts['stat'], opts['param1'], opts['param2'], opts)
  else:
    return np.loadtxt(fileName);

def getData(fileName, index, opts):

  dat = getDat(fileName, opts);

  print 'dat = ' + str(dat)
  
  nline = np.shape(dat)[0];

  x = dat[0:nline,0];
  if opts['logx']:
    x = np.log10(x)

  nx = np.size(np.unique(x));

  y = dat[0:nline,1];
  if opts['logy']:
    y = np.log10(y)

  ny = np.size(np.unique(y));

  [d, unit] = getDataAndUnits(dat, nline, index);
  if opts['logz']:
    d = np.log10(d)
    print 'Taking log of data'

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

  if opts['interp'] == 'gauss':
    z2=gaussInterp(points, d, x2, y2)
  else:
    print 'd = ' + str(d) + ' shape = ' + str(np.shape(d))
    print 'points = ' + str(points) + ' shape = ' + str(np.shape(points))
    print 'x2 = ' + str(x2) + ' shape = ' + str(np.shape(x2))
    z2=int.griddata(points, d, (x2, y2), method=opts['interp'])

  return x2, y2, z2, unit

def gaussInterp(points, z, x2, y2):

  npoints = np.shape(points)[0]
  x = []
  y = []
  for i in range(0,npoints):
    x.append(points[i][0])
    y.append(points[i][1])

  print ' points = ' + str(points) + ' shape = ' + str(np.shape(points)) + ' x = ' + str(x)

  
  z2 = np.zeros(np.shape(x2), dtype=np.double)
  w2 = np.zeros(np.shape(x2), dtype=np.double)

  n = np.size(x)
  n1 = np.shape(x2)[0]
  n2 = np.shape(x2)[1]

  xmax = np.max(x2)
  xmin = np.min(x2)
  ymax = np.max(y2)
  ymin = np.min(y2)
  
  sx = (xmax - xmin) / 2.0
  sy = (ymax - ymin) / 2.0

  print 'iterating over ' + str(n1) + ' ' + str(n2)
  
  for i1 in range(0,n1):
    for i2 in range(0,n2):
      x0 = x2[i1][i2]
      y0 = y2[i1][i2]

      for i in range(0,n):
        xv = x[i]
        yv = y[i]
        zv = z[i]
        zmn = z2[i1][i2]
        w = np.exp(-((x0-xv)*(x0-xv)/(2*sx*sx) + (y0-yv)*(y0-yv)/(2*sy*sy)))
#        w = 1.0
        w2[i1][i2] += w
        z2[i1][i2] += (zv * w - zmn) / w2[i1][i2]

  return z2
      
def retick(ax, axname):
  if axname == 'x':
    rng = ax.get_xlim()
  elif axname == 'y':
    rng = ax.get_ylim()
  else:
    rng = ax.get_zlim()
    print 'Z rng = ' + str(rng)

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

def makeSubPlot(fileName, index, ax, doHold, Color, zlabel, scale, unit, maxVal, opts):

  x,y,z,unit2 = getData(fileName, index, opts);
  plt.hold(doHold);
  print 'z = ' + str(z) + ' scale = ' + str(scale)
  ax.plot_surface(x, y, z/scale, color=Color);

  if unit != '':
    ax.set_zlabel('\n' + zlabel + ' (' + unit + ')', fontsize=defaultFontsize);
  else:
    ax.set_zlabel('\n' + zlabel, fontsize=defaultFontsize);

  minVal = np.min(z/scale)

  print 'minVal = ' + str(minVal)
  
  if opts['logz']:
    maxVal = np.log10(maxVal)

  ax.set_zlim(minVal, maxVal*1.1);
  ax.set_xlabel('\n' + opts['xlabel'], fontsize=defaultFontsize);
  ax.set_ylabel('\n' + opts['ylabel'], fontsize=defaultFontsize);

  if opts['logx']:
    retick(ax, 'x')
  if opts['logy']:
    retick(ax, 'y')
  if opts['logz']:
    retick(ax, 'z')

  ax.tick_params(labelsize=defaultFontsize)

def makeSubPlotTwo(fileName, fileName2, index, action, ax, doHold, Color, zlabel, scale, unit, maxVal, opts):

  print 'Inside sp2 with action = ' + str(action)
  
  x1,y1,z1,unitIgnore = getData(fileName,  index, opts);
  x2,y2,z2,unitIgnore = getData(fileName2, index, opts);
  plt.hold(doHold);
  if action == '/':
    plotunit = 'ratio'
  else:
    plotunit = unit
  if action == '-':
    ax.plot_surface(x1, y1, (z1 - z2)/scale, color=Color);

    maxVal = etu.max(np.max(z1)/scale, np.max(z2)/scale);
    minVal = etu.min(np.min(z1)/scale, np.min(z2)/scale);
    
    ax.set_zlim(minVal, maxVal*1.1);
    
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
  ax.set_xlabel('\n' + opts['xlabel']);
  ax.set_ylabel('\n' + opts['ylabel']);
  if opts['logx']:
    retick(ax, 'x')
  if opts['logy']:
    retick(ax, 'y')
  if opts['logz']:
    retick(ax, 'z')

def plotFiles(files, plotwithfiles, plotwithaction, axes, colors, scales, units, maxs, optDict):

  nfile=np.shape(files)[0]
  ncolor=np.shape(colors)[0];

  for iFile in range(0,nfile):

    if plotwithfiles != None:
      if iFile == 0:
        if plotwithaction == 'p':
          plotData(files, iFile, False, axes, 'c', scales, units, maxs, optDict)
          plotData(plotwithfiles, iFile, True, axes, 'm', scales, units, maxs, optDict)
        else:
          plotDataTwo(files, plotwithfiles, plotwithaction, iFile, False, axes, 'c', scales, units, maxs, optDict)
      else:
        if plotwithaction == 'p':
          plotData(files, iFile,  True, axes, 'c', scales, units, maxs, optDict)
          plotData(plotwithfiles, iFile, True, axes, 'm', scales, units, maxs, optDict)
        else:
          plotDataTwo(files, plotwithfiles, plotwithaction, iFile, True, axes, 'c', scales, units, maxs, optDict)
    else:
      if iFile == 0:
        plotData(files, iFile, False, axes, colors[iFile % ncolor], scales, units, maxs, optDict)
      else:
        plotData(files, iFile,  True, axes, colors[iFile % ncolor], scales, units, maxs, optDict)

def plotData(fileNames, iFile, doHold, axes, Color, scales, units, maxs, optDict):

  naxes=np.shape(axes)[0]

  optDict['stat'] = etu.indOrVal(optDict['stats'], iFile)
  
  makeSubPlot(fileNames[iFile], 2, axes[0][iFile], doHold, Color, optDict['zlabel'], scales[0], units[0], maxs[0], optDict);

  if naxes > 1 and axes[1] != None:
    makeSubPlot(fileNames[iFile], 3, axes[1][iFile], doHold, Color, 'Writes/sec', scales[1], units[1], maxs[1], optDict);

  if naxes > 2 and axes[2] != None:
    makeSubPlot(fileNames[iFile], 4, axes[2][iFile], doHold, Color, 'Bytes/sec', scales[2], units[2], maxs[2], optDict);

def plotDataTwo(fileNames, fileNames2, action, iFile, doHold, axes, Color, scales, units, maxs,  optDict):

  naxes=np.shape(axes)[0]

  optDict['stat'] = etu.indOrVal(optDict['stats'], iFile)
    
  makeSubPlotTwo(fileNames[iFile], fileNames2[iFile], 2, action, axes[0][iFile], doHold, Color,  optDict['zlabel'], scales[0], units[0], maxs[0], optDict);

  if naxes > 1 and axes[1] != None:
    makeSubPlotTwo(fileNames[iFile], fileNames2[iFile], 3, action, axes[1][iFile], doHold, Color,  'Writes/sec', scales[1], units[1], maxs[1], optDict);

  if naxes > 2 and axes[2] != None:
    makeSubPlotTwo(fileNames[iFile], fileNames2[iFile], 4, action, axes[2][iFile], doHold, Color,  'Bytes/sec', scales[2], units[2], maxs[2], optDict);

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

#------------------------------------------------------------
# Main Script starts here
#------------------------------------------------------------

files    = sys.argv[1].split(' ');

opts = {}

plotWith = etu.getOptArgs(sys.argv, 'plotwith', None)
if plotWith != None:
  plotWith = plotWith.split(' ')

plotWithAction = etu.getOptArgs(sys.argv, 'plotwithaction', 'p')

overplot       = etu.str2bool(etu.getOptArgs(sys.argv, 'overplot', 'false'))
opts['logx']   = etu.str2bool(etu.getOptArgs(sys.argv, 'logx', 'false'))
opts['logy']   = etu.str2bool(etu.getOptArgs(sys.argv, 'logy', 'false'))
opts['logz']   = etu.str2bool(etu.getOptArgs(sys.argv, 'logz', 'false'))
scale          = etu.str2bool(etu.getOptArgs(sys.argv, 'scale', 'false'))
labels         = etu.getOptArgs(sys.argv, 'labels', '').split(';')
title          = etu.getOptArgs(sys.argv, 'title',  '')
opts['xlabel'] = etu.getOptArgs(sys.argv, 'xlabel', 'x (unspecified)')
opts['ylabel'] = etu.getOptArgs(sys.argv, 'ylabel', 'y (unspecified)')
opts['zlabel'] = etu.getOptArgs(sys.argv, 'zlabel', 'z (unspecified)')
opts['interp'] = etu.getOptArgs(sys.argv, 'interp',  'linear')
figsizestr     = etu.getOptArgs(sys.argv, 'figsize', '15,10')

# If we are passing dirs instead, we need to know what parameters
# and stats to extract

opts['dirs']   = etu.str2bool(etu.getOptArgs(sys.argv, 'dirs', 'false'))
opts['param1'] = etu.getOptArgs(sys.argv, 'param1', 'ncol')
opts['param2'] = etu.getOptArgs(sys.argv, 'param2', 'nbyte')
opts['stats']  = etu.getOptArgs(sys.argv, 'stat', 'mean').split(' ')
print 'stats = ' + str(opts['stats'])
opts['nsig']   = etu.getOptArgs(sys.argv, 'nsig', '0.0')

# Lastly, see if we are comparing two files

opts['cmp']    = etu.str2bool(etu.getOptArgs(sys.argv, 'cmp', 'false'))

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

print 'Calling getScalesandunits with files = ' + str(files)

scales,units,maxs  = getScalesAndUnits(files, scale, opts);
plt.axis('off')
plt.title(title)
axes = getSubplots(files, overplot, opts)

plotFiles(files, plotWith, plotWithAction, axes, colors, scales, units, maxs, opts)

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
