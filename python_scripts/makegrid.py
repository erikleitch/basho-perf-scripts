import erltestutil as etu
import sys
import numpy as np

def getGrid(mypath, stat, param1, param2):
    files = etu.getFiles(mypath)
    vals = {}
    for file in files:
        args = file.split('_')
        hasPar1 = False
        hasPar2 = False
        for arg in args:
            if param1 in arg:
                hasPar1 = True
                sargs = arg.split(param1)
                par1 = int(sargs[1])
            if param2 in arg:
                hasPar2 = True
                sargs = arg.split(param2)
                par2 = int(sargs[1])

        if hasPar1 and hasPar2:
            val = etu.getStat(mypath, file, stat)
            if par1 not in vals.keys():
                vals[par1] = {}
            vals[par1][par2] = val

    return vals

def saveGrid(vals, fileName):
    f = open(fileName, 'wb')

    skeys1 = np.sort(vals.keys())

    for skey1 in skeys1:
        skeys2 = np.sort(vals[skey1].keys())
        for skey2 in skeys2:
            f.write("%5.2f %5.2f %5.2f\n" % (skey1, skey2, vals[skey1][skey2]))
    f.close()

stat = etu.getOptArgs(sys.argv, 'stat', 'mean')

vals = getGrid('/tmp/tslatency_perc', stat, 'ncol', 'nbyte')
saveGrid(vals, '/tmp/tslatency_perc/' + stat + '_grid.txt')
