"""
to run this script call

$ python pvalue.py histogram mcmcfile yodafile

where the "histogram" parameter indicates which histogram should be read in
from the yoda files, "mcmcfile" is a suitable data file of toys, and
yodafiles is a list of input yoda files of the form "path/to/file.yoda:Title".
"""

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.mlab as mlab
import matplotlib.lines as lines
import numpy as np
import yoda as y
from sys import stdout, argv

def parse_infile(str):
    xs = str.split(":")

    path = xs[0]
    title = xs[1] if len(xs) > 1 else xs[0]

    return (title, path)


files = map(parse_infile, argv[3:])


def llh(modes, cov):
    norm = 1.0/np.sqrt(2*np.pi*np.linalg.det(cov))
    covinv = np.linalg.inv(cov)
    def f(ins):
        diff = ins - modes
        return norm*np.exp(-0.5*np.matmul(np.matmul(diff.T, covinv), diff))

    return f

infile = open(argv[2])

names = np.array(map(str.strip, infile.readline().split(",")))
binidxs = []
for (i, n) in enumerate(names):
    if "normtruthbin" in n:
        binidxs.append((int(n[12:]), i))

binidxs.sort()
idxs = np.array([0] + map(lambda (x, y): y, binidxs))

# read in the toys
xs_unsorted = np.loadtxt(infile, delimiter=',')

# only keep the llh and the observable
xs_unsorted = xs_unsorted[:,idxs]

xs_sorted = np.flipud(xs_unsorted[xs_unsorted[:,0].argsort()])

xs = xs_sorted.transpose()

# if we only have one param we need to add a dimension.
if len(xs.shape) == 1:
    xs.shape = (1, xs.shape[0])

# we remove the llh and the first bin since these are normalized
xs = xs[2:]

modes = xs[:,0]

print("modes of each bin of this observable:")
print(modes)

cov = np.cov(xs)


print("absolute uncertainty on each observable bin")
print(np.sqrt(np.diag(cov)))

print("absolute covariance between bins:")
print(cov)

modes = xs[:,0]
cov = np.cov(xs)


thisllh = llh(modes, cov)
llhs = np.array([thisllh(x) for x in xs.T])

tsts = -2*np.log(llhs)
meantsts = np.mean(tsts)
stddevtst = np.sqrt(np.var(tsts))

nllhs = len(llhs)


def pval(xs):
    return float(np.sum(thisllh(xs) > llhs)) / nllhs

histkey = argv[1]

hs = []
for (k, f) in files:
    h = y.readYODA(f)[histkey]
    hs.append((k, [b.area for b in h.bins[1:]]))


print("")
print("pvalues:")

for (k, h) in hs:
    print("%s distribution:" % k)
    print(h)

    print("")
    print("pvalue of %s:" % k)
    print(pval(h))
    print("Z of %s:" % k)
    print((-2*np.log(thisllh(h)) - meantsts)/stddevtst)
