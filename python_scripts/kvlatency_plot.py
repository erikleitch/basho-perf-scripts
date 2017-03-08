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
delcolor='c'

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
  dbytes = []

  getus = []
  getus_std = []

  putus = []
  putus_std = []

  delus = []
  delus_std = []

  ret = {}
  ret['hasd'] = False
  
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
    elif s[0] == 'del':
      ret['hasd'] = True
      dbytes.append(float(s[1]))
      ntrial = int(s[2])
      delus.append(d[key]['usec']/ntrial)
      delus_std.append(d[key]['usecstd']/ntrial)

  npt = np.size(pbytes)
  
  btes = np.ndarray(npt)

  puts = np.ndarray(npt)
  gets = np.ndarray(npt)
  dels = np.ndarray(npt)

  puts_std = np.ndarray(npt)
  gets_std = np.ndarray(npt)
  dels_std = np.ndarray(npt)
  
  ps = np.argsort(pbytes)
  gs = np.argsort(gbytes)

  if ret['hasd']:
    ds = np.argsort(dbytes)

  for i in range(0,np.size(ps)):
    btes[i] = pbytes[ps[i]]

    puts[i] = putus[ps[i]]
    gets[i] = getus[gs[i]]
    if ret['hasd']:
      dels[i] = delus[ds[i]]

    puts_std[i] = putus_std[ps[i]]
    gets_std[i] = getus_std[gs[i]]
    if ret['hasd']:
      dels_std[i] = delus_std[ds[i]]

  ret['bytes'] = btes
  ret['pv'] = puts
  ret['ps'] = puts_std
  ret['gv'] = gets
  ret['gs'] = gets_std
  ret['dv'] = dels
  ret['ds'] = dels_std
  
  return ret

def makePlot(ax, d, ls, xlabel=True, ylabel=True):
  cstr = getcolor + ls
  plt.plot(d['bytes'], d['gv'], cstr)
  plt.hold(True)
  cstr = putcolor + ls
  plt.plot(d['bytes'], d['pv'], cstr)

  if d['hasd']:
    cstr = delcolor + ls
    plt.plot(d['bytes'], d['dv'], cstr)

  ax.set_xscale('log')
  ax.set_yscale('log')
  
  ax.xaxis.set_visible(False)
  if xlabel:
    plt.xlabel('Data size (bytes)',  fontsize=16)
  if ylabel:
    plt.ylabel('Latency ($\mu$sec)', fontsize=16)

def makeDiffPlot(ax, d1, d2, args, ls, xlabel=True, ylabel=True):

  bytes = d1['bytes']

  print 'd1 = ' + str(d1['pv']) + ' d2 = ' + str(d2['pv'])
  
  puts = d2['pv'] / d1['pv']
  puts_std = pu.getRatioSigma(d1['pv'], d1['ps'], d2['pv'], d2['ps'])

  gets = d2['gv'] / d1['gv']
  gets_std = pu.getRatioSigma(d1['gv'], d1['gs'], d2['gv'], d2['gs'])

  if d1['hasd'] and d2['hasd']:
    dels = d2['dv'] / d1['dv']
    dels_std = pu.getRatioSigma(d1['dv'], d1['ds'], d2['dv'], d2['ds'])
  
  cstr = getcolor + ls
  ax.plot(bytes,gets,cstr)
  plt.hold(True)
  cstr = putcolor + ls
  plt.plot(bytes,puts,cstr)

  if d1['hasd'] and d2['hasd']:
    cstr = delcolor + ls
    plt.plot(bytes,dels,cstr)

  ax.set_xscale('log')
  ax.set_ylim([0.4, 1.5])

  grey=0.75
  ax.fill_between(bytes, gets-gets_std, gets+gets_std, color=getcolor,edgecolor='none')
  ax.fill_between(bytes, puts-puts_std, puts+puts_std, color=putcolor,edgecolor='none')

  if d1['hasd'] and d2['hasd']:
    ax.fill_between(bytes, dels-dels_std, dels+dels_std, color=delcolor,edgecolor='none')
  
  if xlabel:
    plt.xlabel('Data size (bytes)',fontsize=16)
  if ylabel:
    plt.ylabel('Ratio',fontsize=16)

  if args['rat'] != None:
    plt.text(2.0, 1.3, '(' + args['rat'] + ')')

def retick(ax, size):
  ax.set_xticklabels(ax.get_xticklabels(), fontsize=size)
  ax.set_yticklabels(ax.get_yticklabels(), fontsize=size)

def makePanel(d1, d2, args, doX, doY):

  ax = fig.add_subplot(2,3,args['pl'])
  retick(ax, 14)

  makePlot(ax, d1,  '-', doX, doY)
  makePlot(ax, d2, '--', doX, doY)
  
#  plt.legend(args['leg'], loc='upper left')
  plt.title(args['title'], fontsize=16)

  divider = make_axes_locatable(ax)
  axdiff  = divider.append_axes("bottom", 1.2, pad=0.1, sharex=ax)

  makeDiffPlot(axdiff, d1, d2, args, '-', doX, doY)

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

#------------------------------------------------------------
#
#------------------------------------------------------------

files= getOptArgs(sys.argv, 'files', '').replace('\n', ' ')

usersuffix = getOptArgs(sys.argv, 'suffix', '')
if usersuffix != '':
  suffix = usersuffix + '_'
else:
  suffix = ''

print 'Using suffix = ' + str(suffix)

d1 = getProfilerOutput(files)

print 'd1 = ' + str(d1)

fig = plt.figure(figsize=(10,10))
fig.set_facecolor('white');

print 'Here 0'

if suffix != '':
  suffixstr=' (' + usersuffix + ') '
else:
  suffixstr=' '

args = {}

if d1['hasd']:
  args['leg']   = ['nval=3 GET', 'nval=3 PUT', 'nval=3 DEL', 'nval=1 GET', 'nval=1 PUT', 'nval=1 DEL']
else:
  args['leg']   = ['nval=3 GET', 'nval=3 PUT', 'nval=1 GET', 'nval=1 PUT']
    
args['title'] = 'Riak KV' + suffixstr + 'Round-trip Latencies\nleveldb nval=3 vs. nval=1, erlang client'
args['rat']   = 'nval=1/nval=3'
args['pl']    = 1
print 'Here 0'
ax = makePanel(d1, d1, args, False, True)
  
fig.savefig('kvlatency_cmp.png')
plt.show()


