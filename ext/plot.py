import matplotlib.pyplot as plt
import matplotlib.mlab as mlab
import numpy as np
from sys import stdin

def plotParam(param, n):
    hist, bins = np.histogram(param, bins=50)
    width = 0.7 * (bins[1] - bins[0])
    center = (bins[:-1] + bins[1:]) / 2

    plt.bar(center, hist, align='center', width=width)
    plt.savefig("param%03i.png" % n)
    plt.clf()

xs = np.loadtxt(stdin).transpose()
print xs.shape

n = 1
if len(xs.shape) > 1:
    for param in xs:
        plotParam(param, n)
        n += 1

else:
    plotParam(xs, 0)
