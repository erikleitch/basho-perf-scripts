import erltestutil as etu
import sys
import numpy as np
import matplotlib.pyplot as plt

def getGridAsLine(mypath, stat, param1, param2, param1match):
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
            val = etu.getStat(mypath, file, stat)
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

def saveGridAsLine(vals, fileName):
    f = open(fileName, 'wb')

    skeys1 = np.sort(vals.keys())

    for skey1 in skeys1:
        skeys2 = np.sort(vals[skey1].keys())
        for skey2 in skeys2:
            f.write("%5.2f %5.2f %5.2f\n" % (skey1, skey2, vals[skey1][skey2]))
    f.close()

stat = etu.getOptArgs(sys.argv, 'stat', 'mean')
val  = etu.getOptArgs(sys.argv, 'val', '10')

xmn, ymn = getGridAsLine('/tmp/tslatency_perc', stat, 'ncol', 'nbyte', val)
x95, y95 = getGridAsLine('/tmp/tslatency_perc', '95', 'ncol', 'nbyte', val)
x99, y99 = getGridAsLine('/tmp/tslatency_perc', '99', 'ncol', 'nbyte', val)

fig = plt.figure(figsize=(10,10))
fig.set_facecolor('white');

ax = fig.add_subplot(1,1,1)
ax.set_xscale('log')
ax.set_yscale('log')

plt.plot(xmn, ymn)
plt.hold(True)
plt.plot(x95, y95)
plt.plot(x99, y99)
plt.show()

