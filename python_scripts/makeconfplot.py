import erltestutil as etu
import sys
import numpy as np
import matplotlib.patches as mpatches
import matplotlib.lines as mlines
import matplotlib.pyplot as plt

def getPatchHandle(color, text):
    return mpatches.Patch(color=color, label=text)

def getLineHandle(color, marker, text):
    return mlines.Line2D([], [], color=color, label=text, linestyle=marker)

def getGridAsLine(mypath, stat, param1, param2, param1match, opts):
    files = etu.getFiles(mypath)
    xvals = []
    yvals = []
    for file in files:
        args = file.split('_')
        hasPar1 = False
        hasPar2 = False

        par1 = None
        par2 = None
        for arg in args:
            if param1 in arg:
                hasPar1 = True
                sargs = arg.split(param1)
                par1 = int(sargs[1])
            if param2 in arg:
                hasPar2 = True
                sargs = arg.split(param2)
                par2 = int(sargs[1])

        print 'par1 = ' + str(par1) + ' par2 = ' + str(par2) + ' match = ' + param1match       
        if hasPar1 and hasPar2 and (par1 == int(param1match)):
            val = etu.getStat(mypath, file, stat, opts)
            print ' appendiung ' + str(par1*par2) + ' y = ' + str(val)
            xvals.append(np.double(par1*par2))
            yvals.append(val)

    inds = np.argsort(xvals)
    xs = []
    ys = []
        
    for i in range(0, np.size(inds)):
        xs.append(xvals[inds[i]])
        ys.append(yvals[inds[i]])   

    return xs, ys

def getLine(mypath, stat, param, opts):
    files = etu.getFiles(mypath)
    retVals = {}

    xvals = []
    yvals = []
    yvals1 = []
    yvals2 = []

    mult = False
    if 'conf' in stat:
        mult = True;
        
    for file in files:
        args = file.split('_')
        hasPar = False

        par = None
        for arg in args:
            if param in arg:
                hasPar = True
                sargs  = arg.split(param)
                par    = int(sargs[1])

        if hasPar:
            if mult:
                mx, lb, ub = etu.getStat(mypath, file, stat, opts)
                yvals.append(mx)
                yvals1.append(lb)
                yvals2.append(ub)
            else:
                val = etu.getStat(mypath, file, stat, opts)
                yvals.append(val)

            xvals.append(np.double(par))

    inds = np.argsort(xvals)
    retVals['xs'] = []
    retVals['ys'] = []

    if mult:
        retVals['mx'] = []
        retVals['lb'] = []
        retVals['ub'] = []
        
    for i in range(0, np.size(inds)):
        retVals['xs'].append(xvals[inds[i]])

        if mult:
            retVals['mx'].append(yvals[inds[i]])   
            retVals['lb'].append(yvals1[inds[i]])   
            retVals['ub'].append(yvals2[inds[i]])   
        else:
            retVals['ys'].append(yvals[inds[i]])   


    return retVals

def saveGridAsLine(vals, fileName):
    f = open(fileName, 'wb')

    skeys1 = np.sort(vals.keys())

    for skey1 in skeys1:
        skeys2 = np.sort(vals[skey1].keys())
        for skey2 in skeys2:
            f.write("%5.2f %5.2f %5.2f\n" % (skey1, skey2, vals[skey1][skey2]))
    f.close()

stat = etu.getOptArgs(sys.argv, 'stat', 'mean')
path = etu.getOptArgs(sys.argv, 'path', '/tmp/kvlatency_perc')
val  = etu.getOptArgs(sys.argv, 'val', '10')

opts = {}
opts['nsig']   = etu.getOptArgs(sys.argv, 'nsig', None)
opts['bins']   = etu.getOptArgs(sys.argv, 'bins', 50)
opts['logx']   = etu.str2bool(etu.getOptArgs(sys.argv, 'logx', 'true'))
opts['logy']   = etu.str2bool(etu.getOptArgs(sys.argv, 'logy', 'true'))
opts['title']  = etu.getOptArgs(sys.argv, 'title', '')
opts['xlabel'] = etu.getOptArgs(sys.argv, 'xlabel', '')
opts['ylabel'] = etu.getOptArgs(sys.argv, 'ylabel', '')
opts['loc']    = etu.getOptArgs(sys.argv, 'loc', 'upper left')

print 'here 0'
retMn   = getLine(path, 'mean', 'nbyte', opts)
print 'here 1'
ret1Sig = getLine(path, '68conf', 'nbyte', opts)
ret2Sig = getLine(path, '95conf', 'nbyte', opts)
ret3Sig = getLine(path, '99conf', 'nbyte', opts)
ret99Perc = getLine(path, '99perc', 'nbyte', opts)

fig = plt.figure(figsize=(10,10))
fig.set_facecolor('white');

ax = fig.add_subplot(1,1,1)

if opts['logx']:
    ax.set_xscale('log')
if opts['logy']:
    ax.set_yscale('log')

#plt.plot(ret99Perc['xs'], ret99Perc['ys'], color='red')
plt.hold(True)

ax.fill_between(ret3Sig['xs'], ret3Sig['lb'], ret3Sig['ub'], facecolor='pink', edgecolor='pink')
h3sig = getPatchHandle('pink', '99% Confidence Interval')

ax.fill_between(ret2Sig['xs'], ret2Sig['lb'], ret2Sig['ub'], facecolor='lightblue', edgecolor='lightblue')
h2sig = getPatchHandle('lightblue', '95% Confidence Interval')

plt.plot(retMn['xs'], retMn['ys'], 'g--')
ax.fill_between(ret1Sig['xs'], ret1Sig['lb'], ret1Sig['ub'], facecolor='lightgreen', edgecolor='lightgreen')
plt.plot(ret1Sig['xs'], ret1Sig['mx'], color='darkgreen')
hmode = getLineHandle('green', '-', 'Mode')
hmean = getLineHandle('green', '--', 'Mean')
h1sig = getPatchHandle('lightgreen', '68% Confidence Interval')

#plt.plot(ret1Sig['xs'], ret1Sig['lb'], color='g')
#plt.plot(ret1Sig['xs'], ret1Sig['ub'], color='g')

plt.xlabel(opts['xlabel'])
plt.ylabel(opts['ylabel'])
plt.title(opts['title'])

plt.legend(handles=[hmode, hmean, h1sig, h2sig, h3sig], loc=opts['loc'])

plt.show()

