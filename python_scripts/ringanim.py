import numpy as np
from matplotlib.lines import Line2D
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from matplotlib.patches import Polygon
import sys
import pytz
from datetime import datetime, timedelta

import warnings

warnings.simplefilter(action = "ignore", category = FutureWarning)

def str2bool(v):
  return v.lower() in ("yes", "true", "t", "1")

#=======================================================================
# A file object
#=======================================================================

class File(object):

    def __init__(self, fileName):

        self.fileName = fileName

        self.nFrameAdvance  = 0
        self.nFrame         = 0
        
        f = open(fileName)

        self.partitions = f.readline()
        self.tags       = f.readline()            
        
        line     = f.readline()
        counters = line.split(' ')

        self.firstTimestamp = int((counters[0].split(':'))[0])
        self.nLine = 1

        for line in f:
            self.nLine += 1

        counters = line.split(' ')
        self.lastTimestamp = int((counters[0].split(':'))[0])

        print 'First time = ' + str(self.firstTimestamp) + ' last = ' + str(self.lastTimestamp) + ' nline = ' + str(self.nLine)
        f.close()

    def advanceStartFrame(self, nFrame):
        self.nFrameAdvance += nFrame

    def setNframe(self, nFrame):
        self.nFrame = nFrame

    def open(self):
        self.f = open(self.fileName)

        # And skip the first two lines
        
        self.f.readline()
        self.f.readline()

        # And advance by any requested number of frames

        self.nLineRemaining = self.nLine
        for i in range(0, self.nFrameAdvance):
            self.f.readline()
            self.nLineRemaining -= 1

        # Set the remaining number of lines to the total, minus any lines we want to discard

        if self.nFrame > 0 and self.nFrame < self.nLineRemaining:
            self.nLineRemaining = self.nFrame

    def close(self):
        self.f.close()

    def advanceToTimestamp(self, timeStamp):
        self.advanceStartFrame((timeStamp - self.firstTimestamp) / 1000000)
        
    def truncateToTimestamp(self, timeStamp):
        self.truncateEndFrame((self.lastTimestamp - timeStamp) / 1000000)

    def getNextCounters(self):
        line = self.f.readline()
        counters = line.split(' ')
        timesplit = counters[0].split(':')
        timestamp = int(timesplit[0])

        nitem = np.size(counters)

        counters = counters[1:nitem]
        self.nLineRemaining -= 1

        return timestamp, counters

class Cluster(object):
    
    def __init__(self, axes, tags, fileNames, skipstart, nFrame, floor, dispLabels):

        self.nNode     = np.size(fileNames)
        self.nodes     = []
        self.axes      = axes;
        self.firstLine = True;
        self.tags      = tags
        self.ntag      = np.size(tags)
        self.skipstart = skipstart
        self.nFrame    = nFrame
        self.files     = []
        self.floor     = floor
        self.firstFrame=True
        self.epoch = datetime(1970, 1, 1, tzinfo=pytz.UTC)
        self.firstTimestamp = None
        
        for fileName in fileNames:
            if fileName != '':
                self.files.append(File(fileName))

        firstCommonTimestamp = self.files[0].firstTimestamp
        lastCommonTimestamp  = self.files[0].lastTimestamp

        #------------------------------------------------------------
        # Trim to the bounds where we have common timestamps
        #------------------------------------------------------------
        
        for f in self.files:
            
            if f.firstTimestamp > firstCommonTimestamp:
                firstCommonTimestamp = f.firstTimestamp

            if f.lastTimestamp < lastCommonTimestamp:
                lastCommonTimestamp = f.lastTimestamp

        for f in self.files:
            f.advanceToTimestamp(firstCommonTimestamp)
            f.advanceStartFrame(skipstart)
            f.setNframe(nFrame)
            
        #------------------------------------------------------------
        # Now append nodes
        #------------------------------------------------------------

        # Outermost ring will have dR = 0.1 (1.0 - 0.9).  each
        # successive ring will have radii that conserve the area of
        # the outer ring
        #
        #
        # dANp is the total conserved area that we want to match for
        # each successive ring
        #

        minRadius = 0.6
        totalArea = 4 * np.pi * (1.0*1.0 - minRadius*minRadius)
        nRing = np.size(self.files) + 1
        print 'nRing = ' + str(nRing)
        dANp  = totalArea / nRing
        outerRadius = np.sqrt(1.0 - dANp / (4*np.pi))

        print 'outerRadius = ' + str(outerRadius)
        
        self.totalNode = Node(axes, tags, self.files[0], 1.0, dANp)
        self.totalNode.dispLabels = dispLabels
                
        for f in self.files:
            node = Node(axes, tags, f, outerRadius, dANp)
            node.dispLabels = False
            outerRadius = node.radiusa
            self.nodes.append(node)

        #------------------------------------------------------------
        # Get limits over all nodes
        #------------------------------------------------------------

        self.getLimits()
        self.ndig = int(np.floor(np.log10(float(self.nLine))) + 1)
        
    #=======================================================================
    # Update limits against values for the current line
    #=======================================================================

    def getLimits(self):
        first = True
        
        for node in self.nodes:
            if first:
                first = False
                self.nLine     = node.nLine
                self.tagLims   = node.tagLims

                self.totals = {}
                for key in node.tagTotals.keys():
                    self.totals[key] = np.zeros(node.nPart, dtype=np.int)

                self.tagTotals = {}
                for key in node.tagTotals.keys():
                    self.tagTotals[key] = 0

                self.tagTotalLims = {}
                for key in node.tagTotals.keys():
                    self.tagTotalLims[key] = 0

            else:
                if node.nLine < self.nLine:
                    self.nLine = node.nLine

            for key in self.tagLims.keys():
                self.tagTotals[key] += node.tagTotals[key]

                # Store the max totals over all nodes
                
                if np.max(node.tagTotals[key]) > self.tagTotalLims[key]:
                    self.tagTotalLims[key] = np.max(node.tagTotals[key])

                # Store the max instantaneous rates over all nodes

                if node.tagLims[key] > self.tagLims[key]:
                    self.tagLims[key] = node.tagLims[key]

            if self.nFrame > 0 and self.nLine > self.nFrame:
                self.nLine = self.nFrame

    #=======================================================================
    # Cluster::update is the main update method called during the animation
    #=======================================================================
    
    def update(self, val):

        # For some reason, the animation is called twice for the first frame, so ignore the first call
        
        if self.firstFrame:
            self.firstFrame = False
            return

        # If this isn't the last line (for which we just display
        # totals), read the next line for all nodes

        if val < self.nLine:
            for node in self.nodes:
                node.getNextCounters()

        if self.firstTimestamp == None:
            self.firstTimestamp=self.nodes[0].ts

        orange  = [255.0/255, 165.0/255,   0.0/255]
        magenta = [255.0/255,   0.0/255, 255.0/255]
        purple  = [0.5,0.3,1.0]

        ccolor = magenta
        
        if val == self.nLine:
            color = magenta
            date0 = self.epoch + timedelta(microseconds=self.firstTimestamp)
            date1 = self.epoch + timedelta(microseconds=self.nodes[0].ts)
            fmtStr = '%0' + str(self.ndig) + 'd'
            tsStr = date0.strftime('UTC: %d %b %Y %H:%M:%S') + ' - ' + date1.strftime('UTC: %d %b %Y %H:%M:%S')
        else:
            color = purple
            date = self.epoch + timedelta(microseconds=self.nodes[0].ts)
            fmtStr = '%0' + str(self.ndig) + 'd'
            tsStr = date.strftime('UTC: %d %b %Y %H:%M:%S') + ' (' + (fmtStr % val) + ')'

        # And iterate over tags we are plotting
        
        for i in range(0, self.ntag):
            self.drawRing(val, i, tsStr, color, ccolor)

        # Print the timestamp too

        if val == 0:
            self.timeStr = plt.figtext(0.5, 0.1, tsStr, color=color, size=16, ha='center')
        else:
            self.timeStr.set_text(tsStr)
            self.timeStr.set_color(color)

    #=======================================================================
    # Cluster::drawRing calls down to Node::drawRing for all nodes
    #=======================================================================
    
    def drawRing(self, val, tagInd, tsStr, color, ccolor):

        tag = self.tags[tagInd]

        if tagInd == 0:
            sys.stdout.write('Frame: ' + str(("%4d" % val)) + ' (' + ("%3d" % (100*float(val)/self.nLine)) + '%) ts = ' + str(self.nodes[0].ts) + ' (' + tsStr + ')' + '\r')
            sys.stdout.flush()

        ax = axes[tagInd]
        ax.clear()
        ax.axis('off')

        if val == self.nLine:
            text  = tag + '\n(cumulative)'
        else:
            text  = tag

        #------------------------------------------------------------
        # Now iterate over nodes, drawing the instantaneous records
        #------------------------------------------------------------

        self.nonzero = 0
        self.nops = 0
        totals = []
        
        for node in self.nodes:
                
            #------------------------------------------------------------
            # If this is the last frame, display cumulative totals for all nodes
            #------------------------------------------------------------
            
            if val == self.nLine:
                vals = node.tagTotals[tag]
                nz=vals[np.where(vals > 0.0)]
                mn=np.mean(nz)
#                print 'Totals = ' + str(nz/mn) + ' mean = ' + str(np.mean(nz))
                totals.append(np.sum(nz))
                node.drawRing(val, tagInd, self.tagLims, node.tagTotals[tag], self.tagTotalLims, self.floor, color)

            #------------------------------------------------------------
            # Else display the instantaneous count
            #------------------------------------------------------------
            
            else:
                    
                node.drawRing(val, tagInd, self.tagLims, None, None, self.floor, color)
                if self.firstLine:
                    self.totals[tag] = node.vals[tag]
                    self.firstLine = False
                else:
                    self.totals[tag] += node.vals[tag]
                    self.nonzero += np.count_nonzero(node.vals[tag])
                    self.nops    += np.sum(node.vals[tag])

        #------------------------------------------------------------
        # Finally, draw the outermost ring with the cumulative totals
        #------------------------------------------------------------

        self.totalNode.drawRing(val, tagInd, self.tagLims, self.totals[tag], self.tagTotals, self.floor, ccolor)

        if val != self.nLine:
            text += '\n' + 'n_active = ' + str(self.nonzero)
            text += '\n' + 'n_ops    = ' + str(self.nops)
        else:
          print 'totals = ' + str(totals)
          print 'Total = ' + str(np.sum(totals)) + ' '
            
        ax.text(0, 0, text, color=color, size=18, ha='center', va='center')

        
class Node(object):

    def __init__(self, axes, tags, f, outerRadius, dANp):
        self.axes = axes
        self.f = f
        self.initializeRingSizeAndLimits()

        # Outer radius for this ring is the specified start radius
        # Inner radius is calculated to conserve area
        
        self.radiusb = outerRadius
        self.radiusa = np.sqrt(outerRadius*outerRadius  - dANp/(4*np.pi))
        self.tags    = tags
                                          
    def initializeRingSizeAndLimits(self):

        self.f.open()

        partitionContent = self.f.partitions
        tagContent       = self.f.tags

        #------------------------------------------------------------
        # Count and order the partitions
        #------------------------------------------------------------
        
        parts = partitionContent.split(' ')
        self.partitionList = []
        for i in range(1, np.size(parts)-1):
            self.partitionList.append(int(parts[i]))
            
        self.ps = np.argsort(self.partitionList)

        self.nPart = np.size(self.ps)

        self.pls = []
        for i in range(0, self.nPart):
            self.pls.append(self.partitionList[self.ps[i]])

#        print 'ps       = ' + str(self.ps)
#        print 'partList = ' + str(self.partitionList)
#        print 'pls      = ' + str(self.pls)
        
        #------------------------------------------------------------
        # Get a map of tags
        #------------------------------------------------------------

        tags = tagContent.split(' ')
        self.nTag = np.size(tags)-2
        self.tagList = []
        self.tagInds = {}
        self.tagLims = {}
        self.tagTotals = {}
        self.vals    = {}
        for i in range(1, np.size(tags)-1):
            self.vals[tags[i]] = []
            self.tagList.append(tags[i])
            self.tagInds[tags[i]] = i-1
            self.tagLims[tags[i]] = 0
            self.tagTotals[tags[i]] = np.zeros(self.nPart, dtype=np.int)

        #------------------------------------------------------------
        # Now read the rest of the file, and get the limits
        #------------------------------------------------------------

        self.nLine = 0

        
        for i in range(0, self.f.nLineRemaining):
            timestamp, counters = self.f.getNextCounters()
            self.nLine += 1
            self.getLims(counters)

        self.f.close()

        # And reset the file descriptor

        self.f.open()

        self.nLine = self.f.nLineRemaining

    def getLims(self, counters):

        # For each key, extract counters for all partitions
        
        for key in self.tagLims:
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
#            print 'Part = ' + str(self.pls[i]) + ' Val = ' + str(vals[i])

        return vals

    def getNextCounters(self):
        self.ts, self.counters = self.f.getNextCounters()

    #=======================================================================
    # Node::drawRing
    #=======================================================================
    
    def drawRing(self, val, tagInd, tagLims, inpVals=None, tagTotals=None, floorval=0.0, rgb=[0.5,0.3,1.0]):

        # If explicit input vals were passed, use those instead of reading from the file

        tag = self.tags[tagInd]
        ax  = self.axes[tagInd]
        
        if inpVals == None:
            vals = self.getOrderedCounts(self.counters, tag)
#            print 'vals = ' + str(vals)
        else:
            vals = inpVals

        # Make the max the global max for this tag, either the
        # instantaneous max or the total max, depending on usage
        
        if inpVals != None:
            tagMax = np.max(tagTotals[tag])
        else:
            tagMax = tagLims[tag]
        
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
            
            patch = self.getPatch(theta, dtheta, int(24 * (8.0/self.nPart)))

            if self.dispLabels:
                x = 1.1*np.cos(theta)
                y = 1.1*np.sin(theta)

                pstr = str(self.pls[i])
                ax.text(x, y, pstr[0:4], color='m', rotation=theta*180.0/np.pi, horizontalalignment='center')

            if vals[i] == 0:
                mult = floorval
            else:
                mult = (float(vals[i]) / tagMax + floorval) / (1.0 + floorval)

            fc = np.asarray(rgb) * mult
            ec = np.asarray(rgb) * 0.7

            ax.add_patch(Polygon(patch, closed=True, facecolor=fc, linewidth=0.3, edgecolor=ec))

            self.vals[tag] = vals
        
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

#=======================================================================
# Main script
#=======================================================================

#plt.rcParams["font.family"] = "cursive"

#------------------------------------------------------------
# Get optional args, and instantiate the Cluster object
#------------------------------------------------------------

files     = getOptArgs(sys.argv,       'files',     'dev1_atomicCounters.txt dev2_atomicCounters.txt dev3_atomicCounters.txt')
tags      = getOptArgs(sys.argv,       'tags',      'syncput')
skipstart = int(getOptArgs(sys.argv,   'skipstart', 0))
nframe    = int(getOptArgs(sys.argv,   'nframe',    0))
save      = getOptArgs(sys.argv,  'save',      False) == "True"
floor     = float(getOptArgs(sys.argv, 'floor',     0.0))
dispLabels= getOptArgs(sys.argv, 'labels', False) == "True"

print 'Tags = ' + str(tags.split(' ')) + ' Labels = ' + str(dispLabels)

fig, axes = getAxes(tags.split(' '))
ring = Cluster(axes, tags.split(' '), files.split(' '), skipstart, nframe, floor, dispLabels)

#------------------------------------------------------------
# And run the animation
#------------------------------------------------------------

print 'Save = ' + str(save) + ' Running with ring.nLine = ' + str(ring.nLine)
print 'Tags = ' + str(tags)




anim = animation.FuncAnimation(fig, ring.update, ring.nLine+1, interval=1,
                               blit=False, repeat=False)

# Set up formatting for the movie files

if save:
    print 'Writers = ' + str(animation.writers)

    Writer = animation.writers['ffmpeg']
    writer = Writer(fps=5, metadata=dict(artist='Me'), bitrate=1800)
    
    anim.save('im.mp4', writer=writer, savefig_kwargs={'facecolor':'black'})
else:
    plt.show()
