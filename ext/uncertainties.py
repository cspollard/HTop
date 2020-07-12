"""
to run this script call

$ python uncertainties.py mcmcfile outfile.pdf obsname

"mcmcfile" is a suitable data file of toys, "outfile.pdf" is the filename of
the output figure, and "obsname" is the latex observable name used for
plotting.
"""

debug=False

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.mlab as mlab
import matplotlib.lines as lines
import numpy as np
from sys import stdout, argv


outfile = argv[2]
obsname = argv[3]
infile = open(argv[1])

names = np.array(map(str.strip, infile.readline().split(",")))
poiidxs = []
npidxs = []
npnames = []
for (i, n) in enumerate(names):
    if "normtruthbin" in n:
        poiidxs.append((int(n[12:]), i))
    elif "truthbin" in n or "recobin" in n or n == "llh":
        continue
    else:
        npidxs.append(i)


poiidxs.sort()
_, poiidxs = zip(*poiidxs)
poiidxs = np.array(poiidxs, dtype=int)
npidxs = np.array(npidxs, dtype=int)

print("poi idxs:")
print(poiidxs)

# read in the toys
xs = np.loadtxt(infile, delimiter=',').transpose()

# if we only have one param we need to add a dimension.
if len(xs.shape) == 1:
    xs.shape = (1, xs.shape[0])


cov = np.cov(xs)
var = np.diag(cov)
means = np.mean(xs, axis=1)

print("pois:")
print(names[poiidxs])
print("")

print("nps:")
print(names[npidxs])
print("")

print("poi means:")
print(means[poiidxs])
print("")

print("np means:")
print(means[npidxs])
print("")

print("covariances:")
print(cov)
print("")


print("poi total uncertainties:")
print(np.sqrt(var)[poiidxs])
print("")

print("np total uncertainties:")
print(np.sqrt(var)[npidxs])
print("")


def uncert(c, poi, nup):
  return c[poi, nup] / np.sqrt(c[nup, nup])



# divide uncertainties into 5 categories:
# tracking uncertainties
# other detector uncertainties
# signal modeling uncertainties
# bkg modeling uncertainties
# pileup uncertainty

catdict = {}
catlist = \
  [ "pileup", "tracking", "other detector", "signal modeling"
  , "background modeling"
  ]

for c in catlist:
  catdict[c] = [0]*len(poiidxs)

def uncert_cat(n):
  if n == "puwgt":
    return "pileup"

  elif "trk" in n:
    return "tracking"

  elif "btag" in n or "jet" in n or "jvt" in n:
    return "other detector"

  elif n in ["nsvtrksf", "fsr", "rad", "ps", "ptcsf", "ttbarnorm", "lumi"]:
    return "signal modeling"

  elif "stop" in n:
    return "background modeling"

  else:
    print("error --- uncategorized uncertainty:", n)
    exit(-1)



for i in range(len(poiidxs)):
    poiidx = poiidxs[i]
    for nup in npidxs:
        catdict[uncert_cat(names[nup])][i] += uncert(cov, poiidx, nup)**2

for k in catdict.keys():
  catdict[k] = map(np.sqrt, catdict[k])

catdict["total"] = list(np.sqrt(var[poiidxs]))

colors = \
  { "total" : "black"
  , "pileup" : "red"
  , "tracking" : "green"
  , "other detector" : "gray"
  , "signal modeling" : "blue"
  , "background modeling" : "orange"
  }


fig = plt.figure()

xpts = map(lambda x: x - 0.5, range(len(poiidxs)+1) + [len(poiidxs)])

for n, u in catdict.iteritems():
    print(n, ":", u)
    plt.plot( \
        xpts
      , [0] + u + [0]
      , color=colors[n]
      , ls="steps"
      , label=n
      )

plt.legend()

fig.axes[0].set_title("uncertainty sources for " + obsname)
fig.axes[0].set_xlabel(obsname + " bin")
fig.axes[0].set_ylabel("differential cross section uncertainty")

plt.savefig(outfile)
