import erltestutil as etu
import sys
import numpy as np
import matplotlib.pyplot as plt

def getData(mypath, file, stat, opts):
    return etu.getStat(mypath, file, stat, opts)

print 'here 0'
paths   = etu.getOptArgs(sys.argv, 'path', '/tmp/kvlatency_perc').split(' ')
files   = etu.getOptArgs(sys.argv, 'file', '').split(' ')

print str(etu.getOptArgs(sys.argv, 'nsig', '0.0'))
print str(etu.getOptArgs(sys.argv, 'nsig', '0.0').split(' '))
                     
nsigs   = etu.listToFloat(etu.getOptArgs(sys.argv, 'nsig', '0.0').split(' '))
print 'here 1'
bins    = etu.listToInt(etu.getOptArgs(sys.argv, 'bins', '10').split(' '))
subplot = etu.listToInt(etu.getOptArgs(sys.argv, 'subplot', '1,1').split(','))
xlabels = etu.getOptArgs(sys.argv, 'xlabel', '').split(';')
ylabels = etu.getOptArgs(sys.argv, 'ylabel', '').split(';')
titles  = etu.getOptArgs(sys.argv, 'title', '').split(';')
figsize = etu.strToIntTuple(etu.getOptArgs(sys.argv, 'figsize', '10,10'))
xlims   = etu.getOptArgs(sys.argv, 'xlims', None)
output  = etu.getOptArgs(sys.argv, 'output', None)

if xlims != None:
    xlims = etu.strToFloatTuple(xlims)
    
print 'here 2 files = ' + str(files)

opts = {}

fig = plt.figure(figsize=figsize)
fig.set_facecolor('white');

nFile = np.size(files)
nFile = max(nFile, np.size(paths))
nFile = max(nFile, np.size(nsigs))
nFile = max(nFile, np.size(bins))

for iFile in range(0, nFile):
    ax = fig.add_subplot(subplot[0], subplot[1], iFile+1)

    opts['nsig'] = etu.indOrVal(nsigs, iFile)
    path = etu.indOrVal(paths, iFile)
    file = etu.indOrVal(files, iFile)

    print 'path = ' + str(path) + ' file = ' + str(file)
    
    dat = getData(path, file, 'data', opts)

    plt.hist(dat, bins=etu.indOrVal(bins, iFile))
    plt.xlabel(etu.indOrVal(xlabels, iFile))
    plt.ylabel(etu.indOrVal(ylabels, iFile))
    plt.title(etu.indOrVal(titles, iFile))

    if xlims != None:
        ax.set_xlim(xlims[0], xlims[1])

if output != None:
    plt.tight_layout(w_pad=2,pad=5)
    fig.savefig(output)
else:
    plt.show()


