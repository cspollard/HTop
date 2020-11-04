"""
to run this script call

$ python pvalue.py histogram mcmcfile yodafile

where the "histogram" parameter indicates which histogram should be read in
from the yoda files, "mcmcfile" is a suitable data file of toys, and
yodafiles is a list of input yoda files of the form "path/to/file.yoda:Title".
"""

debug=False

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
means = np.mean(xs, axis=1)

centers = means

cov = np.cov(xs)
covinv = np.linalg.inv(cov)


if debug:
    print("modes of each bin of this observable:")
    print(modes)
    print("")

    print("means of each bin of this observable:")
    print(means)
    print("")

    print("absolute uncertainty on each observable bin")
    print(np.sqrt(np.diag(cov)))

    print("absolute covariance between bins:")
    print(cov)


thisllh = llh(centers, cov)
llhs = np.array([thisllh(x) for x in xs.T])


tsts = -2*np.log(llhs)
meantsts = np.mean(tsts)
stddevtst = np.sqrt(np.var(tsts))

nllhs = float(len(llhs))


def pval(llh):
    x = float(np.sum(llh > llhs))
    return x / nllhs

histkey = argv[1]

hs = []
for (k, f) in files:
    h = y.readYODA(f)[histkey]
    hs.append((k, [b.area() for b in h.bins()[1:]]))


# print("")
# print("pvalues:")

fig, ax = plt.subplots()
hist, bins = np.histogram(tsts, bins=50)
width = 0.7 * (bins[1] - bins[0])
center = (bins[:-1] + bins[1:]) / 2
plt.bar(center, hist, align='center', width=width, color='green')
yint = ax.get_yaxis().get_data_interval()
ymin = yint[0]
ymax = yint[1]*1.2

thesepvals = {}
thesellhs = {}

for (k, h) in hs:
    if debug:
        print("")
        print("%s distribution:" % k)
        print(h)

    hdiff = h - centers

    llh = thisllh(h)
    thistst = -2*np.log(llh)

    thesellhs[k] = llh
    thesepvals[k] = pval(llh)

    # print("")
    # print("-2*log(llh) of %s:" % k)
    if debug:
        print(k)
        print("teststat:")
        print(thistst)
        print("chi2 of %s:" % k)
        print(np.matmul(np.matmul(hdiff.T, covinv), hdiff))
        print("pvalue:")
        print(pval(llh))
        print("Z of %s:" % k)
        print((thistst - meantsts)/stddevtst)


print("")
print("llhs:")
print(thesellhs.items())

print("")
print("pvals:")
print(thesepvals.items())


plt.savefig("teststats.pdf")
