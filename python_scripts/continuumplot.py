import matplotlib.pyplot as plt
import numpy as np


def resetLimits(ax):
    lims = ax.axis()
    xrng = lims[1] - lims[0]
    yrng = lims[3] - lims[2]

    print 'lims = ' + str(lims)
    
    ax.set_xlim(lims[0] - 0.15*xrng, lims[1] + 0.15*xrng)
    ax.set_ylim(1, 2000)

x = [1,2,3,4,5,6]
y = [2.95, 29.13, 173.70, 234.07, 639.22, 791.78]

fig = plt.figure(figsize=(10,6))
fig.set_facecolor('white');

ax = fig.add_subplot(1,1,1)

ax.plot(x,y, 'b.')
plt.hold(True)
ax.plot(x,y, 'b-')
locs, labels = plt.xticks()
ax.set_ylabel('MB/sec')
ax.set_yscale('log')
              
nlabels = ('1c x 10b', '10c x 10b', '100c x 10b', '10c x 100b', '100c x 100b', '100c x 100b B')
zlabels = ('', '', '', '', '', '')


for i in range(0, np.shape(labels)[0]):
    plt.text(x[i], 1.2*y[i], nlabels[i], horizontalalignment='right')
    if i > 0:
        print 'Ratio of ' + str(i) + ' = ' + str(y[i]/y[i-1])
    
plt.xticks(locs, zlabels)
resetLimits(ax)
plt.title('Max Throughput 15 + 15 Cluster\n')

plt.savefig('continuum.png')

plt.show()
