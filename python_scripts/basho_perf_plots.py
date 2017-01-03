import sys
import os
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.lines as mlines
#------------------------------------------------------------
# Read an output file from scraping basho-perf logs, and create a sorted grid from it
#------------------------------------------------------------

def getSortedDat(file):

    dat   = np.loadtxt(file);
    nline = np.shape(dat)[0];
    ncol  = np.shape(dat)[1];
    
    uparam1 = np.sort(np.unique(dat[:,0]))
    uparam2 = np.sort(np.unique(dat[:,1]))
    
    np1 = np.shape(uparam1)[0]
    np2 = np.shape(uparam2)[0]
    
    ind1 = dict()
    ind2 = dict()
    
    for i in range(0, np1):
        ind1[uparam1[i]] = i
        
    for i in range(0, np2):
        ind2[uparam2[i]] = i
            
    arr = np.ndarray((ncol-2, np1, np2), np.double);

    for iLine in range(0, nline):
        row = dat[iLine]
        p1  = row[0]
        p2  = row[1]
        for iCol in range(0, ncol-2):
            arr[iCol, ind1[p1], ind2[p2]] = row[2 + iCol]

    return arr, uparam1, uparam2

def getData(iCol, datum, dat1, dat2, dat3):
    x = [5.0, 10.0, 15.0]
    y = []
    e = None
    
    d1 = dat1[iCol]
    d2 = dat2[iCol]
    d3 = dat3[iCol]
    
    if datum == 'mean':
        y.append(np.mean(d1))
        y.append(np.mean(d2))
        y.append(np.mean(d3))
        e = []
        e.append(np.std(d1, ddof=1))
        e.append(np.std(d2, ddof=1))
        e.append(np.std(d3, ddof=1))
    elif datum == 'meanratio':
        y.append(np.mean(d1/d1))
        y.append(np.mean(d2/d1))
        y.append(np.mean(d3/d1))
        e = []
        e.append(np.std(d1/d1, ddof=1))
        e.append(np.std(d2/d1, ddof=1))
        e.append(np.std(d3/d1, ddof=1))
    elif datum == 'meanratiocolmax':
        print 'd1 = ' + str(d1) + ' col0 = ' + str(d1[:,0])
        cm1 = getColMax(d1)
        cm2 = getColMax(d2)
        cm3 = getColMax(d3)
        y.append(np.mean(cm1/cm1))
        y.append(np.mean(cm2/cm1))
        y.append(np.mean(cm3/cm1))
        e = []
        e.append(np.std(cm1/cm1, ddof=1))
        e.append(np.std(cm2/cm1, ddof=1))
        e.append(np.std(cm3/cm1, ddof=1))
    elif datum == 'ratiomax':
        y.append(np.max(d1)/np.max(d1))
        y.append(np.max(d2)/np.max(d1))
        y.append(np.max(d3)/np.max(d1))
    elif datum == 'max':
        y.append(np.max(d1))
        y.append(np.max(d2))
        y.append(np.max(d3))

    x = np.asarray(x)
    y = np.asarray(y)

    if e is not None:
        e = np.asarray(e)

    if np.max(y) > 1.0e9:
        scale = 1.0e9
        unit = 'G'
    elif np.max(y) > 1.0e6:
        scale = 1.0e6
        unit = 'M'
    elif np.max(y) > 1.0e3:
        scale = 1.0e3
        unit = 'K'
    else:
        scale = 1.0
        unit  = None

    return x,y,e,scale,unit

def getColMax(d):
    ncols = np.shape(d)[1]
    print 'd = ' + str(d)
    print 'shap = ' + str(np.shape(d)) + ' ncol = ' + str(ncols)
    
    cm = []
    for iCol in range(0, ncols):
        cm.append(np.max(d[:,iCol]))

    return np.asarray(cm)
    
def makeCompPlots():
    dataDir = str(os.environ['RIAK_TEST_BASE'])

    dat5,  uparam1, uparam2 = getSortedDat(dataDir + '/data/ycsb_5by5_100.txt')
    dat10, uparam1, uparam2 = getSortedDat(dataDir + '/data/ycsb_10by10_100.txt')
    dat15, uparam1, uparam2 = getSortedDat(dataDir + '/data/ycsb_15by15_100.txt')
    dat1510, uparam1, uparam2 = getSortedDat(dataDir + '/data/ycsb_15by10_100.txt')

    dat5b,  uparam1, uparam2 = getSortedDat(dataDir + '/data/ycsb_5by5_100byte_100col.txt')
    dat10b, uparam1, uparam2 = getSortedDat(dataDir + '/data/ycsb_10by10_100byte_100col.txt')
    dat15b, uparam1, uparam2 = getSortedDat(dataDir + '/data/ycsb_15by15_100byte_100col.txt')

    dat5t,  uparam1, uparam2 = getSortedDat(dataDir + '/data/ycsb_5by5_10.txt')
    dat10t, uparam1, uparam2 = getSortedDat(dataDir + '/data/ycsb_10by10_10.txt')
    dat15t, uparam1, uparam2 = getSortedDat(dataDir + '/data/ycsb_15by15_10.txt')

    fig = plt.figure(figsize=(12,12))
    fig.set_facecolor('white');

    legFontSize = 11
    bline = mlines.Line2D([], [], color='b', label='10-byte cols')
    cline = mlines.Line2D([], [], color='c', label='100-byte cols')
    mline = mlines.Line2D([], [], color='c', linestyle='--', label='100-byte cols, batched')

    #------------------------------------------------------------
    # First plot is ops/sec
    #------------------------------------------------------------
    
    ax = fig.add_subplot(2,2,1)
    makePlot(ax, dat5t, dat10t, dat15t, 0, 'max', 'Max ops/sec','b')
    makePlot(ax, dat5, dat10, dat15, 0, 'max', 'Max ops/sec','c')
    resetLimits(ax)
    plt.legend(handles=[bline, cline], fontsize=legFontSize, loc='upper left')

    #------------------------------------------------------------
    # Second plot is ratio ops/sec
    #------------------------------------------------------------
    
    ax = fig.add_subplot(2,2,2)
    makePlot(ax, dat5t, dat10t, dat15t, 0, 'meanratio', 'Ratio ops/sec', 'b')
#    makePlot(ax, dat5t, dat10t, dat15t, 0, 'ratiomax', 'Ratio ops/sec', 'b', '--')
    makePlot(ax, dat5t, dat10t, dat15t, 0, 'meanratiocolmax', 'Ratio ops/sec', 'b', '--')
    
    makePlot(ax, dat5, dat10, dat15, 0, 'meanratio', 'Ratio ops/sec', 'c')
 #   makePlot(ax, dat5, dat10, dat15, 0, 'ratiomax', 'Ratio ops/sec', 'c', '--')

    makePlot(ax, dat5, dat10, dat15, 0, 'meanratiocolmax', 'Ratio ops/sec', 'c', '--')
    resetLimits(ax)


    rbline  = mlines.Line2D([], [], color='b', label='10-byte cols, mean')
    rbbline = mlines.Line2D([], [], color='b', linestyle='--', label='10-byte cols, max')
    
    rcline  = mlines.Line2D([], [], color='c', label='100-byte cols, mean')
    rcbline = mlines.Line2D([], [], color='c', linestyle='--', label='100-byte cols, max')

    plt.legend(handles=[rbline, rbbline, rcline, rcbline], fontsize=legFontSize, loc='upper left')

    #------------------------------------------------------------
    # Third plot is cols/sec
    #------------------------------------------------------------
    
    ax = fig.add_subplot(2,2,3)
    makePlot(ax, dat5t, dat10t, dat15t, 1, 'max', 'Max cols/sec', 'b')
    makePlot(ax, dat5, dat10, dat15, 1, 'max', 'Max cols/sec', 'c')
    makePlot(ax, dat5b, dat10b, dat15b, 1, 'max', 'Max cols/sec', 'c', '--')
    resetLimits(ax)
    plt.legend(handles=[bline, cline, mline], fontsize=legFontSize, loc='upper left')

    #------------------------------------------------------------
    # Fourth plot is bytes/sec
    #------------------------------------------------------------
    
    ax = fig.add_subplot(2,2,4)
    makePlot(ax, dat5t, dat10t, dat15t, 2, 'max', 'Max bytes/sec', 'b')
    makePlot(ax, dat5, dat10, dat15, 2, 'max', 'Max bytes/sec', 'c')
    makePlot(ax, dat5b, dat10b, dat15b, 2, 'max', 'Max bytes/sec', 'c', '--')
    resetLimits(ax)
    plt.legend(handles=[bline, cline, mline], fontsize=legFontSize, loc='upper left')

    fig.savefig(dataDir + '/images/nodeCompPlot.png')
    
    plt.show()

def makePlot(ax, dat1, dat2, dat3, iCol, datum, axName, color, ls='-'):

    x,y,e,scale,unit = getData(iCol, datum, dat1, dat2, dat3)

    print 'Plotting with color = ' + color
    ax.plot(x, y/scale, color + '.')
    plt.hold(True)
    ax.plot(x, y/scale, color + ls)
        
    if e is not None:
        plt.hold(True)
        print 'e = ' + str(e) + ' scale = ' + str(scale)
        ax.errorbar(x, y/scale, e/scale, ecolor=color, fmt='none')

    if unit == None:
        ax.set_ylabel(axName)
    else:
        ax.set_ylabel(axName + ' (' + unit + ')')

    ax.set_xlabel('Cluster size (nodes)')
    
def resetLimits(ax):
    lims = ax.axis()
    xrng = lims[1] - lims[0]
    yrng = lims[3] - lims[2]
    
    ax.set_xlim(lims[0] - 0.1*xrng, lims[1] + 0.1*xrng)
    ax.set_ylim(lims[2] - 0.1*yrng, lims[3] + 0.1*yrng)
    
def testCompPlots():
    dataDir = str(os.environ['RIAK_TEST_BASE'])
    dat5,  uparam1, uparam2 = getSortedDat(dataDir + '/data/ycsb_5by5_100.txt')
    getData(1, 'meancolmax', dat5, dat5, dat5)

makeCompPlots()
#testCompPlots()



