import numpy as np
from matplotlib.lines import Line2D
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from matplotlib.patches import Polygon
import profiler_util as pu
import sys

class Cluster(object):
    
    def __init__(self, ax, tag, fileNames):
        self.nNode = np.size(fileNames)
        self.nodes = []
        self.ax = ax;
        self.firstLine = True;
        self.tag = tag
        
        ind = 1
        for fileName in fileNames:
            self.nodes.append(Node(ax, tag, fileName, ind))
            ind += 1

        self.totalNode = Node(ax, tag, fileNames[0], 0)
        
        # Get limits over all nodes

        self.getLimits()

    def getLimits(self):
        first = True
        for node in self.nodes:
            if first:
                first = False
                self.nLine = node.nLine
                self.tagLims = node.tagLims
                self.tagTotals = node.tagTotals
            else:
                for key in self.tagLims.keys():
                    print 'node = ' + str(node.fileName) + ' tags = ' + str(node.tagLims)
                    print 'key  = ' + str(key) + ' totals = ' + str(node.tagTotals)
                    self.tagTotals[key] += node.tagTotals[key]

                    if node.tagLims[key] > self.tagLims[key]:
                        self.tagLims[key] = node.tagLims[key]
                
    def update(self, val):
        self.drawRing(val);

    def drawRing(self, val):
        if val == self.nLine-1:
            return
        self.ax.clear()
        plt.axis('off')

        for node in self.nodes:
            node.drawRing(val, self.tagLims)
            if self.firstLine:
                self.totals = node.vals
                self.firstLine = False
            else:
                self.totals += node.vals

        self.totalNode.drawRing(val, self.tagLims, self.totals, self.tagTotals)

        r = 0.5
        g = 0.3
        b = 1.0

        plt.text(0, 0, self.tag, color=[r,g,b], ha='center', size=18)
        
class Node(object):

    def __init__(self, ax, tag, fileName, ind):
        self.ax = ax
        self.ind = 0
        self.fileName = fileName
        self.radiusb = 1.0 - 0.1*ind
        self.radiusa = self.radiusb - 0.1
        self.initializeRingSizeAndLimits()
        self.tag = tag
        
    def initializeRingSizeAndLimits(self):

        self.f = open(self.fileName);

        partitionContent = self.f.readline()
        tagContent = self.f.readline()

        #------------------------------------------------------------
        # Count and order the partitions
        #------------------------------------------------------------
        
        parts = partitionContent.split(' ')
        self.partitionList = []
        for i in range(1, np.size(parts)-1):
            self.partitionList.append(int(parts[i]))
            
        self.ps = np.argsort(self.partitionList)
#        for i in range(0, np.size(self.ps)):
#            print str(self.partitionList[self.ps[i]])

        self.nPart = np.size(self.ps)

        #------------------------------------------------------------
        # Get a map of tags
        #------------------------------------------------------------

        tags = tagContent.split(' ')
        self.nTag = np.size(tags)-2
        self.tagList = []
        self.tagInds = {}
        self.tagLims = {}
        self.tagTotals = {}
        for i in range(1, np.size(tags)-1):
            self.tagList.append(tags[i])
            self.tagInds[tags[i]] = i-1
            self.tagLims[tags[i]] = 0
            self.tagTotals[tags[i]] = np.zeros(self.nPart, dtype=np.int)
            print 'Found tag = ' + str(tags[i])

        #------------------------------------------------------------
        # Now read the rest of the file, and get the limits
        #------------------------------------------------------------

        self.nLine = 0
        for line in self.f:
            self.nLine += 1
            counters = line.split(' ')
            nitem = np.size(counters)
            counters = counters[1:nitem]
            self.getLims(counters)

#        for key in self.tagLims:
#            print 'tag ' + key + ' has lim ' + str(self.tagLims[key])
#            print 'tag ' + key + ' has totals ' + str(self.tagTotals[key])

        # And reset the file descriptor

        self.f = open(self.fileName);
        self.f.readline()
        self.f.readline()

    def getLims(self, counters):

        # For each key, extract counters for all partitions
        
        for key in self.tagLims:
            print 'key = ' + str(key) + ' fileName  = ' + str(self.fileName)
            vals = self.getOrderedCounts(counters, key)
            self.tagTotals[key] += vals
            if int(np.max(vals)) > int(self.tagLims[key]):
                self.tagLims[key] = int(np.max(vals))

    def getOrderedCounts(self, counters, key):
        vals = np.ndarray(self.nPart, dtype=np.int)
        tagInd = self.tagInds[key]
        for i in range(0, self.nPart):
            partInd  = self.ps[i]
            countInd = partInd * self.nTag + tagInd
            vals[i] = int(counters[countInd])

        return vals
            
    def drawRing(self, val, tagLims, inpVals=None, tagTotals=None):

        line = self.f.readline()

        if line == '':
            return
        
        counters = line.split(' ')
        nitem = np.size(counters)
        counters = counters[1:nitem]

        if inpVals == None:
            vals = self.getOrderedCounts(counters, self.tag)
        else:
            vals = inpVals

        # Make the max the global max for this tag, either the
        # instantaneous max or the total max, depending on usage
        
        if inpVals != None:
            tagMax = np.max(tagTotals[self.tag])
        else:
            tagMax = tagLims[self.tag]
        
        dtheta = (np.pi * 2) / self.nPart

        radiusa = self.radiusa
        radiusb = self.radiusb

        for i in range(0, self.nPart):
            theta = dtheta * i
            
            xa1 = radiusa * np.cos(theta)
            xb1 = radiusb * np.cos(theta)
            ya1 = radiusa * np.sin(theta)
            yb1 = radiusb * np.sin(theta)
            
            xa2 = radiusa * np.cos(theta + dtheta)
            xb2 = radiusb * np.cos(theta + dtheta)
            ya2 = radiusa * np.sin(theta + dtheta)
            yb2 = radiusb * np.sin(theta + dtheta)
            
            patch = self.getPatch(theta, dtheta, 30)

            if vals[i] == 0:
                mult = 0.1
            else:
                mult = float(vals[i]) / tagMax

            r = 0.5 * mult
            g = 0.3 * mult
            b = 1.0 * mult
            self.ax.add_patch(Polygon(patch, closed=True, facecolor=[r,g,b]))

            self.vals = vals
        
    def getPatch(self, theta, dtheta, npt):                        
        patch = []
        dt = float(dtheta) / npt
        
        for i in range(0, npt+1):
            th = theta + dt * i
            x = self.radiusb * np.cos(th)
            y = self.radiusb * np.sin(th)
            patch.append((x,y))

        for i in range(0, npt+1):
            th = theta + dtheta - dt * i
            x = self.radiusa * np.cos(th)
            y = self.radiusa * np.sin(th)
            patch.append((x,y))

        return patch
    
def emitter():
    yield np.random.rand(1)

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

#=======================================================================
# Main script
#=======================================================================

fig = plt.figure(figsize=(10,10))
fig.set_facecolor('black');

plt.axis('off')

plt.xlim([-1,1])
plt.ylim([-1,1])

plt.axes().set_aspect('equal', 'datalim')

ax   = plt.gca()

#------------------------------------------------------------
# Get optional args, and instantiate the Cluster object
#------------------------------------------------------------

files = getOptArgs(sys.argv, 'files',  'dev1_atomicCounters.txt dev2_atomicCounters.txt dev3_atomicCounters.txt')
tag   = getOptArgs(sys.argv, 'tag',  'asyncput')

ring = Cluster(ax, tag, files.split(' '))

#------------------------------------------------------------
# And run the animation
#------------------------------------------------------------

anim = animation.FuncAnimation(fig, ring.update, ring.nLine, interval=10,
                               blit=False, repeat=False)

# Set up formatting for the movie files

print 'Writers = ' + str(animation.writers)

Writer = animation.writers['ffmpeg']
writer = Writer(fps=15, metadata=dict(artist='Me'), bitrate=1800)

anim.save('im.mp4', writer=writer)

plt.show()
