import sys
import os
from os import listdir
from os.path import isfile, join
import numpy as np
import subprocess

def str2bool(v):
  return v.lower() in ("yes", "true", "t", "1")

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

def getFiles(mypath):
    onlyFiles = [f for f in listdir(mypath) if isfile(join(mypath, f))]
    return onlyFiles

def getStat(path, file, stat):
    dat = np.loadtxt(path + '/' + file);
    nline = np.shape(dat)[0];
    if stat == 'mean':
        return np.mean(dat)
    elif stat == 'median':
        return np.median(dat)
    else:
        return np.percentile(dat, np.double(stat))

def loadDataFromFile(file):
    dat = np.loadtxt(file);
    nline = np.shape(dat)[0];
    ncol  = np.shape(dat)[1];
    x = dat[:,0]
    y = dat[:,1]
    return x, y

def getSystemHosts():
  return "ip-10-1-3-1 ip-10-1-3-2 ip-10-1-3-3 ip-10-1-3-4 ip-10-1-3-5"

def getHarnessHosts():
  return "ip-10-1-2-1 ip-10-1-2-2 ip-10-1-2-3 ip-10-1-2-4 ip-10-1-2-5"

#print str(getSystemHosts())
