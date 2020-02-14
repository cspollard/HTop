import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.mlab as mlab
import matplotlib.lines as lines
import numpy as np
import yoda as y
from sys import stdin, stdout, argv



def llh(modes, cov):
    norm = 1.0/np.sqrt(2*np.pi*np.linalg.det(cov))
    covinv = np.linalg.inv(cov)
    def f(ins):
        diff = ins - modes
        return norm*np.exp(-0.5*np.matmul(np.matmul(diff.T, covinv), diff))

    return f


names = np.array(map(str.strip, stdin.readline().split(",")))
binidxs = []
for (i, n) in enumerate(names):
    if "normtruthbin" in n:
        binidxs.append((int(n[12:]), i))

binidxs.sort()
idxs = np.array([0] + map(lambda (x, y): y, binidxs))

print("idxs: %s" % idxs)
print("names[idxs]: %s" % names[idxs])

# read in the toys
xs_unsorted = np.loadtxt(stdin, delimiter=',')

# only keep the llh and the observable
xs_unsorted = xs_unsorted[:,idxs]

xs_sorted = np.flipud(xs_unsorted[xs_unsorted[:,0].argsort()])

xs = xs_sorted.transpose()

# if we only have one param we need to add a dimension.
if len(xs.shape) == 1:
    xs.shape = (1, xs.shape[0])

# we remove the llh and the first bin since these are normalized
xs = xs[2:]

print("xs:")
print(xs)

modes = xs[:,0]

print("modes:")
print(modes)

cov = np.cov(xs)


print("absolute uncertainties")
print(np.sqrt(np.diag(cov)))

print("absolute covariance:")
print(cov)

modes = xs[:,0]
cov = np.cov(xs)


thisllh = llh(modes, cov)
llhs = np.array([thisllh(x) for x in xs.T])

print("llhs:")
print(llhs)

tsts = -2*np.log(llhs)
meantsts = np.mean(tsts)
stddevtst = np.sqrt(np.var(tsts))

nllhs = len(llhs)


def pval(xs):
    return float(np.sum(thisllh(xs) > llhs)) / nllhs

observable = argv[1]

histkey = "/htop/elmujjtrue/truejets/" + observable

files = \
    { "PowPy8" : "yoda.nochi2/PowPy8FS.yoda" \
    , "Sherpa221" : "yoda.nochi2/Sherpa221AFII.yoda" \
    , "Powheg+Herwig7": "yoda.nochi2/PowH7AFII.yoda" \
    , "Powheg+Pythia6": "yoda.nochi2/PowPy6FS.yoda" \
    , "Powheg+Pythia8 (FSR down)": "yoda.nochi2/PowPy8FSRDownAFII.yoda" \
    , "Powheg+Pythia8 (FSR up)": "yoda.nochi2/PowPy8FSRUpAFII.yoda" \
    , "Powheg+Pythia8 (ISR down)": "yoda.nochi2/PowPy8RadDownAFII.yoda" \
    , "Powheg+Pythia8 (ISR up)": "yoda.nochi2/PowPy8RadUpAFII.yoda" \
    , "aMC@NLO+Pythia8": "yoda.nochi2/aMCPy8AFII.yoda" \
    }


hs = {}

for (k, f) in files.iteritems():
    h = y.readYODA(f)[histkey]
    hs[k] = [b.area for b in h.bins[1:]]


print("pvalues:")

for (k, h) in hs.iteritems():
    # print("%s distribution:" % k)
    # print(h)

    print("")
    print("pvalue of %s:" % k)
    print(pval(h))
    print("Z of %s:" % k)
    print((-2*np.log(thisllh(h)) - meantsts)/stddevtst)
