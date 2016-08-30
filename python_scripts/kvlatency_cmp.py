import numpy as np
#from matplotlib import rc
#rc('text', usetex=True)
#rc('text.latex', preamble='\usepackage{color}')
import matplotlib.pyplot as plt
import os
import profiler_util as pu
from mpl_toolkits.axes_grid1 import make_axes_locatable
import sys

getcolor='b'
putcolor='r'

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
        labelDict[label]['usec']  = float(usec[i+1])
        labelDict[label]['count'] = int(counts[i+1])
  return labelDict

def getProfilerOutput(fileName):
  d = pu.parseProfilerOutput(fileName, {})

  gbytes = []
  pbytes = []

  getus = []
  getus_std = []

  putus = []
  putus_std = []

  for key in d.keys():
    s = key.split('_')
    if s[0] == 'put':
      pbytes.append(float(s[1]))
      ntrial = int(s[2])
      putus.append(d[key]['usec']/ntrial)
      putus_std.append(d[key]['usecstd']/ntrial)
    elif s[0] == 'get':
      gbytes.append(float(s[1]))
      ntrial = int(s[2])
      getus.append(d[key]['usec']/ntrial)
      getus_std.append(d[key]['usecstd']/ntrial)

  npt = np.size(pbytes)
  btes = np.ndarray(npt)
  puts = np.ndarray(npt)
  gets = np.ndarray(npt)
  puts_std = np.ndarray(npt)
  gets_std = np.ndarray(npt)
  
  ps = np.argsort(pbytes)
  gs = np.argsort(gbytes)

  for i in range(0,np.size(ps)):
    btes[i] = pbytes[ps[i]]
    puts[i] = putus[ps[i]]
    gets[i] = getus[gs[i]]
    puts_std[i] = putus_std[ps[i]]
    gets_std[i] = getus_std[gs[i]]
    if i==0:
      print 'mean = ' + str(puts[i]) + ' std = ' + str(puts_std[i])
      print 'mean = ' + str(puts[i]) + ' std = ' + str(puts_std[i])
      
  return btes,puts,puts_std,gets,gets_std

def makePlot(ax, bytes, puts, gets, ls, xlabel=True, ylabel=True):
  cstr = getcolor + ls
  plt.plot(bytes,gets,cstr)
  plt.hold(True)
  cstr = putcolor + ls
  plt.plot(bytes,puts,cstr)
  ax.set_xscale('log')
  ax.set_yscale('log')
  ax.xaxis.set_visible(False)
  if xlabel:
    plt.xlabel('Data size (bytes)',fontsize=16)
  if ylabel:
    plt.ylabel('Latency ($\mu$sec)',fontsize=16)

def makeDiffPlot(ax, bytes, p1, p1_std, g1, g1_std, p2, p2_std, g2, g2_std, ls, xlabel=True, ylabel=True, rat=None):

  puts = p2/p1
  puts_std = pu.getRatioSigma(p1, p1_std, p2, p2_std)
  gets = g2/g1
  gets_std = pu.getRatioSigma(g1, g1_std, g2, g2_std)
  
  cstr = getcolor + ls
  ax.plot(bytes,gets,cstr)
  #  ax.plot(bytes,gets-gets_std,cstr)
  plt.hold(True)
  #  ax.plot(bytes,gets+gets_std,cstr)
    
  cstr = putcolor + ls
  plt.plot(bytes,puts,cstr)
  #  plt.plot(bytes,puts-puts_std,cstr)
  #  plt.plot(bytes,puts+puts_std,cstr)

  ax.set_xscale('log')
  ax.set_ylim([0.4, 1.5])

  grey=0.75
  ax.fill_between(bytes, gets-gets_std, gets+gets_std, color=getcolor,edgecolor='none')
  ax.fill_between(bytes, puts-puts_std, puts+puts_std, color=putcolor,edgecolor='none')
  
  if xlabel:
    plt.xlabel('Data size (bytes)',fontsize=16)
  if ylabel:
    plt.ylabel('Ratio',fontsize=16)

  if rat != None:
    plt.text(2.0, 1.3, '(' + rat + ')')

def retick(ax, size):
  ax.set_xticklabels(ax.get_xticklabels(), fontsize=size)
  ax.set_yticklabels(ax.get_yticklabels(), fontsize=size)

def makePanel(x, p1, p1_std, g1, g1_std, p2, p2_std, g2, g2_std, sp, leglabels, title, doX, doY, rat):
  ax = fig.add_subplot(2,2,sp)
  retick(ax, 14)

  makePlot(ax, x, p1, g1, '-', doX, doY)
  makePlot(ax, x, p2, g2, '--', doX, doY)
  
  plt.legend(leglabels, loc='upper left')
  plt.title(title, fontsize=16)

  divider = make_axes_locatable(ax)
  axdiff  = divider.append_axes("bottom", 1.2, pad=0.1, sharex=ax)
  makeDiffPlot(axdiff, x, p1, p1_std, g1, g1_std, p2, p2_std, g2, g2_std, '-', doX, doY, rat)

  return ax

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

dataDir = str(os.environ['RIAK_TEST_BASE']) + '/data/'

usersuffix = getOptArgs(sys.argv, 'suffix', '')
if usersuffix != '':
  suffix = usersuffix + '_'
else:
  suffix = ''

print 'Using suffix = ' + str(suffix)

filesn1 = pu.getCmd('ls ' + dataDir + '/kvlatency_nval1_' + suffix + 'iter*.txt').replace('\n', ' ')
bytes1, puts1, puts1_std, gets1, gets1_std = getProfilerOutput(filesn1)

filesn3 = pu.getCmd('ls ' + dataDir + '/kvlatency_nval3_' + suffix + 'iter*.txt').replace('\n', ' ')
bytes3, puts3, puts3_std, gets3, gets3_std = getProfilerOutput(filesn3)

filesw1c = pu.getCmd('ls ' + dataDir + '/kvlatency_nval3_w1c_' + suffix + 'iter*.txt').replace('\n', ' ')
bytesw1c3, putsw1c3, putsw1c3_std, getsw1c3, getsw1c3_std = getProfilerOutput(filesw1c)

filesbc = pu.getCmd('ls ' + dataDir + '/kvlatency_nval3_bitcask_' + suffix + 'iter*.txt').replace('\n', ' ')
bytesb3, putsb3, putsb3_std, getsb3, getsb3_std = getProfilerOutput(filesbc)

#filesoff = pu.getCmd('ls ' + dataDir + '/kvlatency_nval3_aae_passive_' + suffix + 'iter*.txt').replace('\n', ' ')
#bytesaaeoff, putsaaeoff, putsaaeoff_std, getsaaeoff, getsaaeoff_std = getProfilerOutput(filesoff)

fileson  = pu.getCmd('ls ' + dataDir + '/kvlatency_nval3_aae_active_' + suffix + 'iter*.txt').replace('\n', ' ')
bytesaaeon,  putsaaeon, putsaaeon_std, getsaaeon, getsaaeon_std  = getProfilerOutput(fileson)

fig = plt.figure(figsize=(16,16))
fig.set_facecolor('white');

print 'Here 0'

leg = ['leveldb GET (nval=3)', 'leveldb PUT (nval=3)', 'leveldb GET (nval=1)', 'leveldb PUT (nval=1)']
if suffix != '':
  suffixstr=' (' + usersuffix + ') '
else:
  suffixstr=' '

title = 'Riak KV' + suffixstr + 'Round-trip Latencies\nleveldb nval=3 vs. nval=1, erlang client'
rat='nval=1/nval=3'
ax = makePanel(bytes1, puts3, puts3_std, gets3, gets3_std, puts1, puts1_std, gets1, gets1_std, 1, leg, title, False, True, rat)

print 'Here 1'

leg = ['leveldb GET', 'leveldb PUT', 'bitcask GET', 'bitcask PUT)']
title = 'Riak KV' + suffixstr + 'Round-trip Latencies\nleveldb vs. bitcask, nval=3, erlang client'
rat='bitcask/leveldb'
ax = makePanel(bytes3, puts3, puts3_std, gets3, gets3_std, putsb3, putsb3_std, getsb3, getsb3_std, 2, leg, title, False, False, rat)

print 'Here 2'

leg = ['normal GET', 'normal PUT', 'w1c GET', 'w1c PUT']
title = 'Riak KV' + suffixstr + 'Round-trip Latencies\nnormal vs. write-once, nval=3, erlang client'
rat='w1c/normal'
ax = makePanel(bytes3, puts3, puts3_std, gets3, gets3_std, putsw1c3, putsw1c3_std, getsw1c3, getsw1c3_std, 3, leg, title, True, True, rat)

print 'Here 3'

leg = ['normal GET, AAE off', 'normal PUT, AAE off', 'normal GET, AAE on', 'normal PUT, AAE on']
title = 'Riak KV' + suffixstr + 'Round-trip Latencies\nAAE off vs. on, normal bucket, nval=3, erlang client'
rat='AAE on/off'
ax = makePanel(bytes3, puts3, puts3_std, gets3, gets3_std, putsaaeon, putsaaeon_std, getsaaeon, getsaaeon_std, 4, leg, title, True, False, rat)
  
fig.savefig('kvlatency_cmp.png')
plt.show()


