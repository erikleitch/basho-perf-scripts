import numpy as np
from matplotlib.lines import Line2D
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from matplotlib.patches import Polygon
import sys
import pytz
from datetime import datetime, timedelta

#=======================================================================
# A file object
#=======================================================================

class File(object):

    # Init function reads through the whole file, determines:
    #
    # 1) The smallest delta between any events in this file -- the min
    # over all files sets the resolution of our animation
    #
    # 2) The first and last timestamp for this file
    
    def __init__(self, fileName):

        self.fileName = fileName
        self.nLine    = 0
        self.nFrame   = 0
        
        self.firstTimestamp = None
        self.lastTimestamp  = None
        self.minDelta       = None
        self.partSum        = 0
        
        f = open(fileName)

        self.partitions = f.readline()

        first = True
        firstDelta = True
        ts    = 0
        for line in f:
            self.nLine += 1
            counters = line.split(' ')
            ts = int((counters[0].split(':'))[0])

            nameTag = counters[2]
            if "partition_" in nameTag:
                partIndUnordered = int(nameTag.split('_')[1])
                state   = int(counters[3])

                if state == 1:
                    self.partSum += 1
#                    print 'Incremented ' + str(partIndUnordered) + ' sum = ' + str(self.partSum)
                else:
                    self.partSum -= 1
#                    print 'Decremented ' + str(partIndUnordered) + ' sum = ' + str(self.partSum)

            if first:
                first = False;
                self.firstTimestamp = ts
            else:
                delta = ts - lastTs

                if delta <= 0:
                    delta = 1
                    
                if firstDelta:
                    self.minDelta = delta
                    firstDelta = False
                else:
                    if delta < self.minDelta:
                        self.minDelta = delta

            lastTs = ts

        self.lastTimestamp = ts

        if self.minDelta == None:
            self.nFrame = 0
        else:
            self.nFrame = (self.lastTimestamp - self.firstTimestamp) / self.minDelta

        f.close()

    def open(self):
        self.f = open(self.fileName)

        # Skip the first line
        
        self.f.readline()

        # And read the first line

        self.getNextLine()
        print 'Read first line ' + str(self.currentLine)

    def getNextLine(self):
        
        self.currentLine = self.f.readline()
        
        if self.currentLine != '':
            self.splitLine = self.currentLine.split(' ')
            self.currentTs = int(self.splitLine[0])
        else:
            self.splitLine = None
            self.currentTs = None

    def close(self):
        self.f.close()

    def advanceToTimestamp(self, timeStamp):
        self.advanceStartFrame((timeStamp - self.firstTimestamp) / 1000000)
        
    def truncateToTimestamp(self, timeStamp):
        self.truncateEndFrame((self.lastTimestamp - timeStamp) / 1000000)

#=======================================================================
# Class for managing a group of clusters
#=======================================================================

class ClusterGroup(object):

    def __init__(self, labels, axisDict, tags, fileDict, skipstart, nFrame, delta, floor):

        self.clusters = []
        self.labels = labels

        if delta == 0:
            raise ValueError("Delta can't be zero for ClusterGroup")

        for iCluster in range(0, np.size(labels)):
            key = labels[iCluster]
            self.clusters.append(Cluster(axisDict[key], tags, fileDict[key], skipstart, nFrame, delta, floor))

        self.nFrame = 0
        for cluster in self.clusters:
            if cluster.nFrame > self.nFrame:
                self.nFrame = cluster.nFrame

    def update(self, val):
        iCluster = 0
        for cluster in self.clusters:
            if self.labels == None:
                cluster.updateCluster(val, None, self.nFrame)
            else:
                cluster.updateCluster(val, self.labels[iCluster], self.nFrame)
            iCluster += 1
        
#=======================================================================
# Class for managing a single cluster
#=======================================================================

class Cluster(object):
    
    def __init__(self, axes, tags, fileNames, skipstart, nFrame, delta, floor):

        self.nNode     = np.size(fileNames)
        self.nodes     = []
        self.axes      = axes;
        self.firstLine = True;
        self.tags      = tags
        self.ntag      = np.size(tags)
        self.skipstart = skipstart
        self.nFrame    = nFrame
        self.delta     = delta
        self.files     = []
        self.floor     = floor
        self.firstFrame=True
        self.epoch = datetime(1970, 1, 1, tzinfo=pytz.UTC)
        
        for fileName in fileNames:
            if fileName != '':
                self.files.append(File(fileName))

        # Get limits
        
        firstFirst = True
        firstLast  = True
        firstDelta = True

        for file in self.files:

            if file.firstTimestamp != None:
                if firstFirst or file.firstTimestamp < self.firstTimestamp:
                    self.firstTimestamp = file.firstTimestamp
                    firstFirst = False

            if file.lastTimestamp != None:
                if firstLast or file.lastTimestamp > self.lastTimestamp:
                    self.lastTimestamp = file.lastTimestamp
                    firstLast = False

            if file.minDelta != None:
                if firstDelta or file.minDelta < self.minDelta:
                    self.minDelta = file.minDelta
                    firstDelta = False

        if self.nFrame > 0:
            self.minDelta = float(self.lastTimestamp - self.firstTimestamp) / (self.nFrame);
        elif self.delta > 0:
            self.nFrame   = int(float(self.lastTimestamp - self.firstTimestamp) / self.delta) + 1;
            self.minDelta = self.delta
        else:
            self.nFrame   = int(float(self.lastTimestamp - self.firstTimestamp) / self.minDelta) + 1;

        self.firstTimestamp -= self.minDelta

        print 'First = ' + str(self.firstTimestamp) + ' last = ' + str(self.lastTimestamp) + ' delta = ' + str(self.minDelta) + ' nframe = ' + str(self.nFrame)
        
        #------------------------------------------------------------
        # Now append nodes
        #------------------------------------------------------------

        # Outermost ring will have dR = 0.1 (1.0 - 0.9).  each
        # successive ring will have radii that conserve the area of
        # the outer ring
        #
        # dANp is the total conserved area that we want to match for
        # each successive ring

        outerRadius = 0.93
        dANp = 4*np.pi * (1.0*1.0 - outerRadius*outerRadius)

        self.totalNode = Node(axes, tags, self.files[0], 1.0, dANp)
                
        for f in self.files:
            node = Node(axes, tags, f, outerRadius, dANp)
            outerRadius = node.radiusa
            self.nodes.append(node)

        self.ndig = int(np.floor(np.log10(float(self.nFrame))) + 1)
        
    #=======================================================================
    # Cluster::update is the main update method called during the animation
    #=======================================================================
    
    def update(self, val):
        self.updateCluster(val, None, self.nFrame)
        
    def updateCluster(self, val, textStr, nFrame):

        sys.stdout.write('Frame = ' + str(val) + '/' + str(nFrame) + '\n')
        sys.stdout.flush()
        
        # For some reason, the animation is called twice for the first
        # frame, so ignore the first call
        
        if self.firstFrame:
            self.firstFrame = False
            return

        nUpdated = 0
        for node in self.nodes:
            nUpdated += node.updateEvents(val, self.firstTimestamp, self.minDelta)

        ts = self.firstTimestamp + val*self.minDelta
        
        date = self.epoch + timedelta(microseconds=ts)
        fmtStr = '%0' + str(self.ndig) + 'd'
        tsStr = date.strftime('UTC: %d %b %Y %H:%M:%S.%f') + ' (' + (fmtStr % val) + ')'
        elapsedStr = 'Elapsed: ' + str(int(np.ceil(val*self.minDelta))) + ' microseconds'
        
        # Iterate over tags we are plotting, but only if there were any updates

        #        color = [0.5,0.3,1.0]
        color = [142.0/255, 220.0/255, 143.0/255]

        for tagInd in range(0, self.ntag):
            if textStr == None:
                textStr = self.tags[tagInd]
            self.drawRing(val, textStr, tagInd, tsStr, color)

        if val == 0:
            self.elapsedStr = plt.figtext(0.5, 0.1, elapsedStr, color=color, size=16, ha='center')
        else:
            self.elapsedStr.set_text(elapsedStr)

    #=======================================================================
    # Cluster::drawRing calls down to Node::drawRing for all nodes
    #=======================================================================
    
    def drawRing(self, val, textStr, tagInd, tsStr, color):

        tag = self.tags[tagInd]
        text = tag

        ax = self.axes[tagInd]
        ax.clear()
        ax.axis('off')

        ax.text(0, 0, textStr, color=color, size=18, ha='center', va='center')

        #------------------------------------------------------------
        # Now iterate over nodes, drawing the instantaneous records
        #------------------------------------------------------------

        for node in self.nodes:
            node.drawRing(tagInd, tsStr, color)
        
class Node(object):

    def __init__(self, axes, tags, f, outerRadius, dANp):
        self.axes = axes
        self.f = f

        # Outer radius for this ring is the specified start radius
        # Inner radius is calculated to conserve area
        
        self.radiusb = outerRadius
        self.radiusa = np.sqrt(outerRadius*outerRadius  - dANp/(4*np.pi))
        self.tags    = tags
        self.nTag    = np.size(tags)
        self.ringVals  = {}
        self.floorVals = {}
        self.initializeRingSizeAndLimits()
        self.partSum = 0
        
    def initializeRingSizeAndLimits(self):

        self.f.open()

        partitionContent = self.f.partitions

        #------------------------------------------------------------
        # Count and order the partitions
        #------------------------------------------------------------
        
        parts = partitionContent.split(' ')
        self.partitionList = []

        for i in range(1, np.size(parts)-1):
            self.partitionList.append(int(parts[i]))
            
        self.ps    = np.argsort(self.partitionList)
        self.nPart = np.size(self.ps)
        
        self.unsortedToSorted = np.zeros(self.nPart, dtype=np.int)

        for i in range(0, np.size(self.ps)):
            self.unsortedToSorted[self.ps[i]] = i

        #------------------------------------------------------------
        # Initialize the ring to zero for all tags
        #------------------------------------------------------------

        for tag in self.tags:
            self.ringVals[tag]  = np.zeros(self.nPart, dtype=float);
            self.floorVals[tag] = 0.0

    #=======================================================================
    # This is called at each step of the animation.  The behavior we want is:
    #
    # If the timestamp of the last event read from the file lies
    # within the current time step, use it to (potentially) update the
    # ringVals for the corresponding tag, and then read the next line
    # from the file.
    #
    # If the timestamp if the last event read from the file has not
    # yet been encountered, do nothing (don't update the ringVals from
    # the last value)
    #
    # If the timestamp is NULL (file has ended), do nothing
    #=======================================================================

    def updateEvents(self, val, firstTs, deltaTs):
        tsLow  = int(np.floor(firstTs + (val-1)*deltaTs))
        tsHigh = int(np.ceil(firstTs + val*deltaTs))

        # Read events from the file while timestamps lie within the current interval

        print 'current = ' + str(self.f.currentTs) + ' tsLow = ' + str(tsLow) + ' tsHigh = ' + str(tsHigh)
        
        nUpdated = 0
        #        while self.f.currentTs != None and self.f.currentTs > tsLow and self.f.currentTs <= tsHigh:
        while self.f.currentTs != None and self.f.currentTs <= tsHigh:
            self.updateTags(tsHigh)
            self.f.getNextLine()
            nUpdated += 1

        return nUpdated

    def updateTags(self, ts):
        seqTag  = self.f.splitLine[1]
        nameTag = self.f.splitLine[2]
        state   = int(self.f.splitLine[3])

        print 'Inside updateTags with currentLine = ' + str(self.f.currentLine)
        
        if "partition_" in nameTag:

            partIndUnordered = int(nameTag.split('_')[1])

            # Get the index in the sorted list of partitions

            partInd = self.unsortedToSorted[partIndUnordered]

            if state == 1:
                self.ringVals[seqTag][partInd] += 1
            else:
                self.partSum -= partInd
                print 'Decrementing partInd = ' + str(partInd) + ' sum = ' + str(self.partSum)
                self.ringVals[seqTag][partInd] -= 1

        else:
            
            if state == 1:
                self.floorVals[seqTag] += 1
            else:
                self.floorVals[seqTag] -= 1

    #=======================================================================
    # Node::drawRing
    #=======================================================================
    
    def drawRing(self, tagInd, tsStr, rgb=[0.5,0.3,1.0]):

        # If explicit input vals were passed, use those instead of reading from the file

        tag = self.tags[tagInd]
        ax  = self.axes[tagInd]

        ringVals = self.ringVals[tag]
        floorVal = self.floorVals[tag]
        
        dtheta = (np.pi * 2) / self.nPart

        radiusa = self.radiusa
        radiusb = self.radiusb

        for i in range(0, self.nPart):
            theta = dtheta * i
            
            # Construct a patch.  For large number of partitions, we
            # don't need many points for our partitions to look
            # circular (3 is sufficient for npart = 64).  But we need
            # more as the number of partitions gets small, else the
            # 'ring' will look very angular
            
            patch = self.getPatch(theta, dtheta, int(24 * (16.0/self.nPart)))

            # If a partition is highlighted, set its multiplier to 1.
            # If a specific partition is no highlighted, set its
            # multiplier to whatever the ring's multiplier currently
            # is
            
            if ringVals[i] > 0:
                mult = 1.0
            else:
                if floorVal > 0:
                    mult = 0.3
                else:
                    mult = 0.0

            fc = np.asarray(rgb) * mult
            ec = np.asarray(rgb) * 0.7

            ax.add_patch(Polygon(patch, closed=True, facecolor=fc, linewidth=0.3, edgecolor=ec))
        
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

def arrange(ntag):

    ns = int(np.sqrt(float(ntag)))
    if ns == 1:
        ny = 1
        nx = ntag
    else:
        ny = ns
        nx = int(np.ceil(float(ntag)/ns))

    return nx, ny

def getAxisDict(labels, nTag, nPanel):

    nxTag, nyTag     = arrange(nTag)
    nxPanel, nyPanel = arrange(nPanel)

    nxTotal = nxPanel * nxTag
    nyTotal = nyPanel * nyTag
    
    figsize=(5*nxTotal, 5*nyTotal)
    fig = plt.figure(figsize=figsize)

    fig.set_facecolor('black');

    axisDict = {}
    for iPanel in range(0, nPanel):
        if labels == None:
            tag = 'Cluster' + str(iPanel)
        else:
            tag = labels[iPanel]
        axisDict[tag] = []
        
    for iPlot in range(0, nxTotal * nyTotal):
        ax = fig.add_subplot(nyTotal, nxTotal, iPlot+1)
        ax.axis('off')
        ax.set_aspect('equal', 'datalim')
        ax.set_xlim([-1.2, 1.2])
        ax.set_ylim([-1.2, 1.2])

        # Now figure out which cluster this axis belongs to

        iRow = iPlot / nxTotal
        iCol = iPlot % nxTotal

        iColMaj = iCol / nxTag
        iRowMaj = iRow / nyTag

        iCluster = iRowMaj * nxPanel + iColMaj

        if labels == None:
            tag = 'Cluster' + str(iCluster)
        else:
            tag = labels[iCluster]

        axisDict[tag].append(ax)

    return fig, axisDict

def getAxes(tags):

    ntag = np.size(tags)
    ns = int(np.sqrt(float(ntag)))
    if ns == 1:
        ny = 1
        nx = ntag
    else:
        ny = ns
        nx = np.ceil(float(ntag)/ns)

    figsize=(8*nx,8*ny)
    fig = plt.figure(figsize=figsize)

    fig.set_facecolor('black');

    axes = []
    for i in range(0, ntag):
        ax = fig.add_subplot(ny,nx,i+1)
        ax.axis('off')
        ax.set_aspect('equal', 'datalim')
        ax.set_xlim([-1.2, 1.2])
        ax.set_ylim([-1.2, 1.2])
        axes.append(ax)

    return fig, axes

def getFileDict(labels, files):
    fileSplit = files.split(';')
    fileDict = {}
    clusterInd = 0
    retLabels = []
    for fileList in fileSplit:
        if labels == None:
            tag = 'Cluster' + str(clusterInd)
        else:
            tag = labels[clusterInd]

        fileDict[tag] = fileList.split(' ')
        retLabels.append(tag)
        
        clusterInd += 1

    return retLabels, fileDict

#=======================================================================
# Main script
#=======================================================================

#plt.rcParams["font.family"] = "cursive"

#------------------------------------------------------------
# Get optional args, and instantiate the Cluster object
#------------------------------------------------------------

files     = getOptArgs(sys.argv,       'files',     'dev1_atomicCounters.txt dev2_atomicCounters.txt dev3_atomicCounters.txt')
labels    = getOptArgs(sys.argv,       'labels',    '')
tags      = getOptArgs(sys.argv,       'tags',     'query')
skipstart = int(getOptArgs(sys.argv,   'skipstart', 0))
nframe    = int(getOptArgs(sys.argv,   'nframe',    0))
delta     = int(getOptArgs(sys.argv,   'delta',     0))
save      = getOptArgs(sys.argv,       'save',      False) == "True"
floor     = float(getOptArgs(sys.argv, 'floor',     0.0))

if np.size(files.split(';')) == 1:
    fig, axes = getAxes(tags.split(' '))
    ring = Cluster(axes, tags.split(' '), files.split(' '), skipstart, nframe, delta, floor)
else:
    if labels == '':
        labelSplit = None
    else:
        labelSplit = labels.split(';')

    labelSplit, fileDict  = getFileDict(labelSplit, files)
    fig, axisDict = getAxisDict(labelSplit, np.size(tags.split(' ')), np.size(fileDict.keys()))
    ring = ClusterGroup(labelSplit, axisDict, tags.split(' '), fileDict, skipstart, nframe, delta, floor)
    
#------------------------------------------------------------
# And run the animation
#------------------------------------------------------------

anim = animation.FuncAnimation(fig, ring.update, ring.nFrame+2, interval=1,
                               blit=False, repeat=False)

# Set up formatting for the movie files

if save:
    Writer = animation.writers['ffmpeg']
    writer = Writer(fps=10, metadata=dict(artist='Me'), bitrate=1800)
    
    anim.save('im.mp4', writer=writer, savefig_kwargs={'facecolor':'black'})
else:
    plt.show()
