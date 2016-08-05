import numpy as np
import matplotlib.pyplot as plt
import os

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
  d = parseProfilerOutput(fileName, {})
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
  btes = []
  puts = []
  gets = []
  ps = np.argsort(pbytes)
  gs = np.argsort(gbytes)
  for i in range(0,np.size(ps)):
    btes.append(pbytes[ps[i]])
    puts.append(putus[ps[i]])
    gets.append(getus[gs[i]])

  return btes,puts,gets

def makePlot(bytes, puts, gets):
  plt.plot(bytes,gets)
  plt.hold(True)
  plt.plot(bytes,puts)
  ax.set_xscale('log')
  ax.set_yscale('log')
  plt.xlabel('Data size (bytes)',fontsize=16)
  plt.ylabel('Latency ($\mu$sec)',fontsize=16)

def retick(ax, size):
  ax.set_xticklabels(ax.get_xticklabels(), fontsize=size)
  ax.set_yticklabels(ax.get_yticklabels(), fontsize=size)

dataDir = str(os.environ['RIAK_TEST_BASE']) + '/data/'
  
bytes1, puts1, gets1 = getProfilerOutput(dataDir + 'kvlatency_nval1.txt')
bytes3, puts3, gets3 = getProfilerOutput(dataDir + 'kvlatency_nval3.txt')
bytesw1c3, putsw1c3, getsw1c3 = getProfilerOutput(dataDir + 'kvlatency_nval3_w1c.txt')
bytesb3, putsb3, getsb3 = getProfilerOutput(dataDir + 'kvlatency_nval3_bitcask.txt')

fig = plt.figure()
fig.set_facecolor('white');

ax = fig.add_subplot(1,3,1)
retick(ax, 14)
makePlot(bytes3, puts3, gets3)
makePlot(bytes1, puts1, gets1)

plt.legend(['leveldb GET (nval=3)', 'leveldb PUT (nval=3)', 'leveldb GET (nval=1)', 'leveldb PUT (nval=1)'],loc='upper left')
plt.title('Riak KV Round-trip Latencies\nleveldb nval=3 vs. nval=1, erlang client',fontsize=16)

ax = fig.add_subplot(1,3,2)
retick(ax, 14)
makePlot(bytes3, puts3, gets3)
makePlot(bytesb3, putsb3, getsb3)

plt.legend(['leveldb GET', 'leveldb PUT', 'bitcask GET', 'bitcask PUT)'],loc='upper left')
plt.title('Riak KV Round-trip Latencies\nleveldb vs. bitcask, nval=3, erlang client',fontsize=16)

ax = fig.add_subplot(1,3,3)
retick(ax, 14)
makePlot(bytes3, puts3, gets3)
makePlot(bytesw1c3, putsw1c3, getsw1c3)

plt.legend(['normal GET', 'normal PUT', 'w1c GET', 'w1c PUT'],loc='upper left')
plt.title('Riak KV Round-trip Latencies\nnormal vs. write-once, nval=3, erlang client',fontsize=16)

plt.show()
