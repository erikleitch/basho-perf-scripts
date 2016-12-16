import numpy as np
from matplotlib.lines import Line2D
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from matplotlib.patches import Polygon
import sys
import pytz
from datetime import datetime, timedelta

class Cluster(object):
    
    def __init__(self, axes, cd, tags):

        self.nodes     = []
        self.axes      = axes;
        self.firstLine = True;
        self.tags      = tags
        self.ntag      = np.size(tags)
        self.cd        = cd
        
        self.nNode = 0
        for key in cd.keys():
            if key != 'name':
                self.nNode += 1

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

        self.totalNode = Node(axes, tags, cd['totals'], cd['totals'], cd['total'], 1.0, dANp)

        nd = cd['nodes']
        for key in nd.keys():
            node = Node(axes, tags, nd[key]['nkeys'], cd['totals'], cd['total'], outerRadius, dANp)
            outerRadius = node.radiusa
            self.nodes.append(node)

    #=======================================================================
    # Cluster::drawRing calls down to Node::drawRing for all nodes
    #=======================================================================
    
    def drawRing(self):

        orange  = [255.0/255, 165.0/255,   0.0/255]
        magenta = [255.0/255,   0.0/255, 255.0/255]
        purple  = [0.5,0.3,1.0]

        ccolor = magenta
        color  = purple

        tagInd = 0
        print 'tagInd = ' + str(tagInd) + ' tags = ' + str(self.tags)
        
        tag = self.tags[tagInd]
        ax  = axes[tagInd]

        print 'Ax = ' + str(ax)
        ax.clear()
        ax.axis('off')

        text = 'N$_{key} = $' + str(int(self.cd['total']))

        #------------------------------------------------------------
        # Now iterate over nodes, drawing the instantaneous records
        #------------------------------------------------------------

        self.nonzero = 0
        for node in self.nodes:
            node.drawRing(0, color)
            self.nonzero += np.count_nonzero(node.nkeys)
            
        #------------------------------------------------------------
        # Finally, draw the outermost ring with the cumulative totals
        #------------------------------------------------------------

        self.totalNode.drawRing(0, ccolor)

        text += '\n' + 'N$_{pop}$ = ' + str(self.nonzero)
        totals = self.cd['totals'] / self.cd['total']
        std = np.std(totals)
        print 'Std = ' + str(std)
        text += '\n' + 'rms = ' + ('%.2f%%' % (std * 100))
        
        ax.text(0, 0, text, color=color, size=18, ha='center', va='center')
        
class Node(object):

    def __init__(self, axes, tags, nkeys, totals, total, outerRadius, dANp):
        self.axes   = axes
        self.nkeys  = nkeys
        self.total  = total
        self.totals = totals
        self.nPart  = np.size(nkeys)
        
        # Outer radius for this ring is the specified start radius
        # Inner radius is calculated to conserve area
        
        self.radiusb = outerRadius
        self.radiusa = np.sqrt(outerRadius*outerRadius  - dANp/(4*np.pi))
        self.tags    = tags

    #=======================================================================
    # Node::drawRing
    #=======================================================================
    
    def drawRing(self, tagInd, rgb=[0.5,0.3,1.0]):

        # If explicit input vals were passed, use those instead of reading from the file

        tag = self.tags[tagInd]
        ax  = self.axes[tagInd]
        
        vals = self.nkeys
        
        # Make the max the global max for this tag, either the
        # instantaneous max or the total max, depending on usage
        
        valMax = np.max(self.totals)
        
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
            
            mult = float(vals[i]) / valMax

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

def parseFile(file):

    f = open(file)
    nodes = {}
    sum = 0
    for line in f:
        ls = line.split(' ')
        node = ls[1]
        
        if node not in nodes.keys():
            nodes[node] = {}
                    
        if ls[2] == 'sum':
            sum += int(ls[3])
        elif ls[2] == 'partition':
            pn = ls[3]
            if pn not in nodes[node].keys():
                nodes[node][pn]  = {}
            nodes[node][pn]['nkeys'] = ls[5]            

    # Now construct an ordered list of partitions
    # Build a unique dictionary of partitions

    pd = {}
    pl = []
    print 'keys = ' + str(nodes.keys())
    for node in nodes.keys():
        nm = nodes[node]
        for partition in nm.keys():
           if partition not in pd.keys():
               pd[partition] = partition
               pl.append(partition)

    print 'PL = ' + str(pl)
    ps = np.argsort(pl)
    pls = []

    npart = np.size(ps)
    
    for i in range(0, npart):
        pls.append(pl[ps[i]])

    # Now that we have partitions in sorted order, construct a return dictionary 
    
    cluster = {}
    cluster['nodes'] = {}
    cluster['partitions'] = pls
    totals = np.zeros(npart, dtype=np.float)
    
    for node in nodes.keys():
        cluster['nodes'][node] = {}
        nd = cluster['nodes'][node]
        nd['partitions'] = pls
        pa = np.zeros(npart, dtype=np.float)
        for i in range(0, npart):
            part = pls[i]
            if part in nodes[node].keys():
                print 'Node part map = ' + str(nodes[node][part])
                pa[i] = int(nodes[node][part]['nkeys'])
                totals[i] += pa[i]
            else:
                pa[i] = 0.0
                
        nd['nkeys'] = pa

    cluster['totals'] = totals
    cluster['total'] = np.sum(totals)
            
    return cluster

#=======================================================================
# Main script
#=======================================================================

#plt.rcParams["font.family"] = "cursive"

#------------------------------------------------------------
# Get optional args, and instantiate the Cluster object
#------------------------------------------------------------

file      = getOptArgs(sys.argv,       'file',     'ring.txt')
save      = getOptArgs(sys.argv,       'save',      False) == "True"
floor     = float(getOptArgs(sys.argv, 'floor',     0.0))

cd   = parseFile(file)
tags = ['N$_{keys}$']
fig, axes = getAxes(tags)

print str(cd)

ring = Cluster(axes, cd, tags)
ring.drawRing()
plt.show()

totals = cd['totals']
print 'totals = ' + str(totals) + ' var = ' + str(np.std(totals))
