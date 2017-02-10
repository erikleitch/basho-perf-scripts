import erltestutil as etu
import sys
import numpy as np
import matplotlib.pyplot as plt

def getData(mypath, file, stat, opts):
    return etu.getStat(mypath, file, stat, opts)

stat = etu.getOptArgs(sys.argv, 'stat', 'mean')
path = etu.getOptArgs(sys.argv, 'path', '/tmp/kvlatency_perc')
file = etu.getOptArgs(sys.argv, 'file', '')
nsig = etu.getOptArgs(sys.argv, 'nsig', None)
bins = int(etu.getOptArgs(sys.argv, 'bins', '10'))

opts = {}
opts['nsig'] = nsig

fig = plt.figure(figsize=(10,10))
fig.set_facecolor('white');

ax = fig.add_subplot(1,1,1)

dat = getData(path, file, 'data', opts)
h = getData(path, file, '1sig', opts)

y = h[0]
x = h[1]

interval = etu.getConfidenceInterval(x, y, 0.68)

print 'interval = ' + str(interval)

plt.hist(dat, bins=bins)
plt.show()

