import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.mlab as mlab
import matplotlib.lines as lines
import numpy as np
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


sherpa_zbtcnorm = np.array([8.821322591083589e-2, 9.92282660527188e-2,
    0.13408325588342646, 0.22980462411586663, 0.2969697671086123,
    0.15170086092854013])[:-1]

powpy8_zbtcnorm = np.array([0.10692002743535663, 0.11786083140764149,
    0.13805726633020934, 0.21121689589116674, 0.2661283347730284,
    0.1598166441625974])[:-1]

powpy8fsrup_zbtcnorm = np.array([0.13126816922541598,
    0.14073369308799036, 0.15470149089941687, 0.21376640537445457,
    0.23584990396089672, 0.12368033745182536])[:-1]

powpy8fsrdown_zbtcnorm = np.array([8.390230920096885e-2,
    9.781717085183853e-2, 0.12280302301171915, 0.20589233865970744,
    0.29089597695885894, 0.1986891813169071])[:-1]

powpy6_zbtcnorm = np.array([0.11866906079624263, 0.13300122224205888,
    0.15251761264539876, 0.22261971792301252, 0.2510316958951893,
    0.12216069049809797])[:-1]

powh7_zbtcnorm = np.array([0.14685035881988126, 0.13486016361005285,
    0.14209857948672514, 0.20211157378695255, 0.23410708484687398,
    0.1399722394495142])[:-1]


thisllh = llh(modes, cov)
llhs = np.array([thisllh(x) for x in xs.T])

tsts = -2*np.log(llhs)
meantsts = np.mean(tsts)
stddevtst = np.sqrt(np.var(tsts))

nllhs = len(llhs)


def pval(xs):
    return float(np.sum(thisllh(xs) > llhs)) / nllhs



for (i, h) in [("powpy8", powpy8_zbtcnorm), ("sherpa", sherpa_zbtcnorm),
    ("py8fsrup", powpy8fsrup_zbtcnorm), ("py8fsrdown", powpy8fsrdown_zbtcnorm),
    ("powpy6", powpy6_zbtcnorm), ("powh7", powh7_zbtcnorm)]:

        print("pvalue of %s:" % i)
        print(pval(h))
        print("")

	print("Z of %s:" %i)
	print((-2*np.log(thisllh(h)) - meantsts)/stddevtst)
	print("")
