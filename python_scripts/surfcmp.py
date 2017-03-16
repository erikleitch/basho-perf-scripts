import numpy as np
import matplotlib.pyplot as plt
import scipy.interpolate as int;
from mpl_toolkits.mplot3d import Axes3D;
import sys
import os
from os import listdir
from os.path import isfile, join
import scipy.stats as stats
import erltestutil as etu

defaultFontsize=22

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

  etu.retick(ax, 'y')

  if retickz:
    etu.retick(ax, 'z')

  ax.tick_params(labelsize=defaultFontsize)

#-----------------------------------------------------------------------
# Main script starts here
#-----------------------------------------------------------------------

#------------------------------------------------------------
# Arg parsing
#------------------------------------------------------------

print 'Args = ' + str(sys.argv)

dataDir = str(os.environ['RIAK_TEST_BASE']) + '/data/'

file1  = sys.argv[1].split(' ')
file2  = sys.argv[2].split(' ')

title1 = sys.argv[3].replace("\\n", "\n")
title2 = sys.argv[4].replace("\\n", "\n")
title3 = sys.argv[5].replace("\\n", "\n")

if np.size(sys.argv) > 6:
  zlabel3 = sys.argv[6].replace("\\n", "\n")
else:
  zlabel=None

overplot   = etu.str2bool(etu.getOptArgs(sys.argv, 'overplot', 'true'))
cmpplot    = etu.getOptArgs(sys.argv, 'cmpplot', 'frac')
figfile    = etu.getOptArgs(sys.argv, 'figfile',  None)
chis       = etu.str2bool(etu.getOptArgs(sys.argv, 'chis', 'true'))
azstr      = etu.getOptArgs(sys.argv, 'az', '-45 -45 -45')
elstr      = etu.getOptArgs(sys.argv, 'el', '30 30 30')
figsizestr = etu.getOptArgs(sys.argv, 'figsize', '25,8')
zmaxstr    = etu.getOptArgs(sys.argv, 'zmax', '6.0')

opts = {}
opts['xparam'] = etu.getOptArgs(sys.argv, 'xparam', 'ncol')
opts['yparam'] = etu.getOptArgs(sys.argv, 'yparam', 'nrow')
opts['iparam'] = etu.getOptArgs(sys.argv, 'iparam', 'niter')
opts['interp'] = etu.getOptArgs(sys.argv, 'interp', 'cubic')

opts['logx']   = etu.str2bool(etu.getOptArgs(sys.argv, 'logx', 'false'))
opts['logy']   = etu.str2bool(etu.getOptArgs(sys.argv, 'logy', 'true'))
opts['logz']   = etu.str2bool(etu.getOptArgs(sys.argv, 'logz', 'true'))

figsizearr=figsizestr.split(',')
figsize=(np.int(figsizearr[0]), np.int(figsizearr[1]))

print 'figsize = ' + str(figsize)

azs = azstr.split(' ')
els = elstr.split(' ')

#------------------------------------------------------------
# Get gridded data
#------------------------------------------------------------
x1, y1, z1 = etu.getProfilerOutputGrid(file1, opts)
x2, y2, z2 = etu.getProfilerOutputGrid(file2, opts)

if not overplot:
  xd, yd, frac, diff, statstr = etu.getDiffProfilerOutput(file1, file2, opts)

fig = plt.figure(figsize=figsize)
fig.set_facecolor('white');

zmin, zmax = etu.getRange2(z1, z2, 0.1)

ax = fig.add_subplot(1,3,1, projection='3d')
ax.view_init(elev=np.double(els[0]), azim=np.double(azs[0]))
makePlot(ax, x1, y1, z1, zmin, zmax, 'c', title1)

ax = fig.add_subplot(1,3,2, projection='3d')
ax.view_init(elev=np.double(els[1]), azim=np.double(azs[1]))
makePlot(ax, x2, y2, z2, zmin, zmax, 'm', title2)

ax = fig.add_subplot(1,3,3, projection='3d')
ax.view_init(elev=np.double(els[2]), azim=np.double(azs[2]))

if not chis:
  statstr = ' '
  
if overplot:
  makePlot(ax, x1, y1, z1, zmin, zmax, 'c', title3)
  plt.hold(True)
  makePlot(ax, x2, y2, z2, zmin, zmax, 'm')
else:
  if cmpplot == 'diff':
    nx = np.shape(diff)[0]
    ny = np.shape(diff)[1]

    for ix in range(0,nx):
      for iy in range(0,ny):
        diff[ix][iy] = np.power(10, z2[ix][iy]) - np.power(10,z1[ix][iy])

    diff = diff / 1e3
    zmin, zmax = etu.getRange1(diff, 0.1)
    makePlot(ax, x1, y1, diff, zmin, zmax, 'y', title3 + '\n' + statstr, False, zlabel3)
  elif cmpplot == 'div':
    nx = np.shape(diff)[0]
    ny = np.shape(diff)[1]

    for ix in range(0,nx):
      for iy in range(0,ny):
        diff[ix][iy] = np.power(10, z2[ix][iy]) / np.power(10,z1[ix][iy])

    zmin = 0.0
    zmax = np.float(zmaxstr)

    makePlot(ax, x1, y1, diff, zmin, zmax, 'y', title3 + '\n' + statstr, False, zlabel3)
  elif cmpplot == 'frac':
    makePlot(ax, x1, y1, frac*100, -100, 100, 'y', title3 + '\n' + statstr, False, zlabel3)

if figfile != None:
  plt.tight_layout(w_pad=4,pad=5)
  fig.savefig(figfile);

plt.show()
