import sys
import numpy as np

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

file1  = sys.argv[1]
file2  = sys.argv[2]

dat1, uparam1, uparam2 = getSortedDat(file1)
dat2, uparam1, uparam2 = getSortedDat(file2)

ncol = np.shape(dat1)[0]

for iCol in range(0, ncol):
    dc1 = dat1[iCol]
    dc2 = dat2[iCol]
    r = dc1/dc2
    print 'Ratio = ' + str(r)
    print 'iCol = ' + str(iCol) + ' Mean ratio = ' + str(np.mean(r)) + ' +- ' + str(np.std(r, ddof=1))
    inds = np.unravel_index(np.argmax(r), np.shape(r))
    p1 = uparam1[inds[0]]
    p2 = uparam2[inds[1]]
    print 'iCol = ' + str(iCol) + ' Max of ratio = ' + str(np.max(r)) + ' at index ' + str(np.unravel_index(np.argmax(r), np.shape(r))) + ' val = ' + str(p1) + ', ' + str(p2)
    print 'iCol = ' + str(iCol) + ' Ratio of max = ' + str(np.max(dc1) / np.max(dc2))




