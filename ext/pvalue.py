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


names = map(str.strip, stdin.readline().split(","))
xs_unsorted = np.loadtxt(stdin, delimiter=',')
print xs_unsorted
stdout.flush()

xs_sorted = np.flipud(xs_unsorted[xs_unsorted[:,0].argsort()])
print xs_sorted
stdout.flush()

xs = xs_sorted.transpose()

# if we only have one param we need to add a dimension.
if len(xs.shape) == 1:
    xs.shape = (1, xs.shape[0])

idxs = []
ns = []

i = 0
for n in names:
    if "normtruthbin" in n:
        idxs.append(i)
        ns.append(int(n[12:]))
    i += 1

names = ns
idxs = np.array(idxs)

print("names: %s" % names)
print("idxs: %s" % idxs)

# we throw out the last bin since this is a normalized distribution.
xs = xs[idxs[names]][:-1]

modes = xs[:,0]

print("modes:")
print(modes)

cov = np.cov(xs)

print("covariance:")
print(cov)



thisllh = llh(modes, cov)
llhs = np.array([thisllh(x) for x in xs.T])

tsts = -2*np.log(llhs)
meantsts = np.mean(tsts)
stddevtst = np.sqrt(np.var(tsts))

nllhs = len(llhs)


def pval(xs):
    return float(np.sum(thisllh(xs) > llhs)) / nllhs

observable = argv[1]

histkey = "/htop/elmujjtrue/truejets/" + observable

files = \
    { "PowPy8" : "yoda/PowPy8FS.yoda" \
    , "Sherpa221" : "yoda/Sherpa221AFII.yoda" \
    , "Powheg+Herwig7": "yoda/PowH7AFII.yoda" \
    , "Powheg+Pythia6": "yoda/PowPy6FS.yoda" \
    , "Powheg+Pythia8 (FSR down)": "yoda/PowPy8FSRDownAFII.yoda" \
    , "Powheg+Pythia8 (FSR up)": "yoda/PowPy8FSRUpAFII.yoda" \
    , "Powheg+Pythia8 (ISR down)": "yoda/PowPy8RadDownAFII.yoda" \
    , "Powheg+Pythia8 (ISR up)": "yoda/PowPy8RadUpAFII.yoda" \
    , "aMC@NLO+Pythia8": "yoda/aMCPy8AFII.yoda" \
    }


hs = {}

for (k, f) in files.iteritems():
    h = y.readYODA(f)[histkey]
    hs[k] = [b.area for b in h.bins[:-1]]

print hs

print("pvalues:")

for (k, h) in hs.iteritems():
    print("%s: %0.3f" % (k, pval(h)))
    # print("")
    # print("pvalue of %s:" % k)
    # print(pval(h))
    # print("")
    #
    #
    # print("Z of %s:" % k)
    # print((-2*np.log(thisllh(h)) - meantsts)/stddevtst)
    # print("")
