import numpy as np
import matplotlib.pyplot as plt
import os
import profiler_util as pu
from mpl_toolkits.axes_grid1 import make_axes_locatable

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
  putus = []
  for key in d.keys():
    s = key.split('_')
    if s[0] == 'put':
      pbytes.append(float(s[1]))
      ntrial = int(s[2])
      putus.append(d[key]['usec']/ntrial)
    elif s[0] == 'get':
      gbytes.append(float(s[1]))
      ntrial = int(s[2])
      getus.append(d[key]['usec']/ntrial)

  npt = np.size(pbytes)
  btes = np.ndarray(npt)
  puts = np.ndarray(npt)
  gets = np.ndarray(npt)
  
  ps = np.argsort(pbytes)
  gs = np.argsort(gbytes)
  for i in range(0,np.size(ps)):
    btes[i] = pbytes[ps[i]]
    puts[i] = putus[ps[i]]
    gets[i] = getus[gs[i]]

  return btes,puts,gets

def makePlot(ax, bytes, puts, gets, color, xlabel=True, ylabel=True):
  cstr = str(color) + '-'
  plt.plot(bytes,gets,cstr)
  plt.hold(True)
  cstr = str(color) + ':'
  plt.plot(bytes,puts,cstr)
  ax.set_xscale('log')
  ax.set_yscale('log')
  ax.xaxis.set_visible(False)
  if xlabel:
    plt.xlabel('Data size (bytes)',fontsize=16)
  if ylabel:
    plt.ylabel('Latency ($\mu$sec)',fontsize=16)

def makeDiffPlot(ax, bytes, p1, g1, p2, g2, color, xlabel=True, ylabel=True):
  puts = 100*(p2-p1)/p1
  gets = 100*(g2-g1)/g1

  puts = p2/p1
  gets = g2/g1
  
  cstr = str(color) + '-'
  ax.plot(bytes,gets,cstr)
  plt.hold(True)
  cstr = str(color) + ':'
  plt.plot(bytes,puts,cstr)

  ax.set_xscale('log')
#  ax.set_ylim([-100, 100])
  ax.set_ylim([0, 2])

  if xlabel:
    plt.xlabel('Data size (bytes)',fontsize=16)
  if ylabel:
    plt.ylabel('Ratio',fontsize=16)

def retick(ax, size):
  ax.set_xticklabels(ax.get_xticklabels(), fontsize=size)
  ax.set_yticklabels(ax.get_yticklabels(), fontsize=size)

def makePanel(x, p1, g1, p2, g2, sp, leglabels, title, doX, doY):
  ax = fig.add_subplot(2,2,sp)
  retick(ax, 14)

  makePlot(ax, x, p1, g1, 'r', doX, doY)
  makePlot(ax, x, p2, g2, 'b', doX, doY)
  
  plt.legend(leglabels, loc='upper left')
  plt.title(title, fontsize=16)

  divider = make_axes_locatable(ax)
  axdiff  = divider.append_axes("bottom", 1.2, pad=0.1, sharex=ax)
  makeDiffPlot(axdiff, x, p1, g1, p2, g2, 'c', doX, doY)

  return ax

dataDir = str(os.environ['RIAK_TEST_BASE']) + '/data/'

filesn1 = pu.getCmd('ls ' + dataDir + '/kvlatency_nval1_iter*.txt').replace('\n', ' ')
bytes1, puts1, gets1 = getProfilerOutput(filesn1)

filesn3 = pu.getCmd('ls ' + dataDir + '/kvlatency_nval3_iter*.txt').replace('\n', ' ')
bytes3, puts3, gets3 = getProfilerOutput(filesn3)

filesw1c = pu.getCmd('ls ' + dataDir + '/kvlatency_nval3_w1c_iter*.txt').replace('\n', ' ')
bytesw1c3, putsw1c3, getsw1c3 = getProfilerOutput(filesw1c)

filesbc = pu.getCmd('ls ' + dataDir + '/kvlatency_nval3_bitcask_iter*.txt').replace('\n', ' ')
bytesb3, putsb3, getsb3 = getProfilerOutput(filesbc)

filesoff = pu.getCmd('ls ' + dataDir + '/kvlatency_nval3_aae_passive_iter*.txt').replace('\n', ' ')
bytesaaeoff, putsaaeoff, getsaaeoff = getProfilerOutput(filesoff)

fileson  = pu.getCmd('ls ' + dataDir + '/kvlatency_nval3_aae_active_iter*.txt').replace('\n', ' ')
bytesaaeon,  putsaaeon,  getsaaeon  = getProfilerOutput(fileson)

fig = plt.figure(figsize=(16,16))
fig.set_facecolor('white');

leg = ['leveldb GET (nval=3)', 'leveldb PUT (nval=3)', 'leveldb GET (nval=1)', 'leveldb PUT (nval=1)']
title = 'Riak KV Round-trip Latencies\nleveldb nval=3 vs. nval=1, erlang client'
ax = makePanel(bytes1, puts3, gets3, puts1, gets1, 1, leg, title, False, True)
  
leg = ['leveldb GET', 'leveldb PUT', 'bitcask GET', 'bitcask PUT)']
title = 'Riak KV Round-trip Latencies\nleveldb vs. bitcask, nval=3, erlang client'
ax = makePanel(bytes3, puts3, gets3, putsb3, getsb3, 2, leg, title, False, False)
    
leg = ['normal GET', 'normal PUT', 'w1c GET', 'w1c PUT']
title = 'Riak KV Round-trip Latencies\nnormal vs. write-once, nval=3, erlang client'
ax = makePanel(bytes3, puts3, gets3, putsw1c3, getsw1c3, 3, leg, title, True, True)
  
leg = ['normal GET, AAE off', 'normal PUT, AAE off', 'normal GET, AAE on', 'normal PUT, AAE on']
title = 'Riak KV Round-trip Latencies\nAAE off vs. on, normal bucket, nval=3, erlang client'
ax = makePanel(bytesaaeoff, putsaaeoff, getsaaeoff, putsaaeon, getsaaeon, 4, leg, title, True, False)
  
fig.savefig('kvlatency_cmp.png')
plt.show()


