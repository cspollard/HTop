"""
to run this script call

$ python uncertainties.py mcmcfile outfile.pdf obsname logfile.txt

"mcmcfile" is a suitable data file of toys, "outfile.pdf" is the filename of
the output figure, logfile is where logs will be written, and "obsname" is the
latex observable name used for plotting.
"""

from matplotlib import rc, rcParams
rcParams['text.usetex'] = True
rcParams['font.family'] = "Nimbus Sans"
rc('font',**{'family': 'sans-serif', 'sans-serif': 'Nimbus Sans' })

from matplotlib.texmanager import TexManager

#  TexManager.font_info['Nimbus Sans'] = ('Nimbus Sans', r"\usepackage{helvet}")
#  TexManager.font_family = 'Nimbus Sans'
#  TexManager.serif = False
#  TexManager.sans_serif = True

texinfo = \
    r"""\renewcommand{\familydefault}{\sfdefault}
    \usepackage{sfmath}
    \usepackage{helvet}
    \usepackage[symbolgreek]{mathastext}
    \usepackage{sansmath}
    \sansmath"""

rcParams['text.latex.preamble']=[texinfo]




debug=False

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.mlab as mlab
import matplotlib.lines as lines
import numpy as np
from sys import stdout, argv


zbtclab = "$z_{\\mathrm{T}, b}^\\mathrm{ch}$"
zblclab = "$z_{\\mathrm{L}, b}^\\mathrm{ch}$"
nsvtrklab = "$n_b^\\mathrm{ch}$"
rholab = "$\\rho$"

obsdict = \
    { "zbtc" : zbtclab
    , "zblc" : zblclab
    , "nsvtrk" : nsvtrklab
    , "rho" : rholab
    }

axdict = \
    { "zbtc" : 0.04
    , "zblc" : 0.04
    , "nsvtrk" : 0.065
    , "rho" : 0.03
    }

outfile = argv[2]
obsname = argv[3]
logfile = argv[4]
infile = open(argv[1])

obslab = obsdict[obsname]


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

# read in the toys
xs = np.loadtxt(infile, delimiter=',').transpose()

# if we only have one param we need to add a dimension.
if len(xs.shape) == 1:
    xs.shape = (1, xs.shape[0])


cov = np.cov(xs)
var = np.diag(cov)
means = np.mean(xs, axis=1)

if debug:
    print("poi idxs:")
    print(poiidxs)

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

s = ["\\begin{tabular}{ l " + "| r "*len(poiidxs) + "}"]
s += ["bin & " + " & ".join(map(str, range(len(poiidxs)))) + " \\\\"]
s += ["\\hline"]


for n, u in catdict.iteritems():
    
    s += [" & ".join([n] + map(lambda x: "%0.3f" % x, u)) + " \\\\"]

    plt.plot( \
        xpts
      , [0] + u + [0]
      , color=colors[n]
      , ls="steps"
      , label=n
      , lw=1
      )

s += ["\\hline"]

n = "total"
u = list(np.sqrt(var[poiidxs]))

plt.plot( \
    xpts
    , [0] + u + [0]
    , color=colors[n]
    , ls="steps"
    , label=n
    , lw=1
    )

s += [" & ".join([n] + map(lambda x: "%0.3f" % x, u)) + " \\\\"]

s += ["\\end{tabular}"]

plt.tick_params(axis='y', which=u'both', length=5)
plt.tick_params(axis='x', which=u'both', length=0)
plt.tick_params(axis='both', which=u'both', direction="in", width=0.2,
        bottom=True, top=True, left=True, right=True)

plt.plot([], [], ' ', label="\\textbf{ATLAS} \\emph{Preliminary}")
plt.plot([], [], ' ', label="$\\sqrt{s} = 13\\,\\mathrm{TeV}, 36\\,\\mathrm{fb}^{-1}$")

plt.legend(frameon=False)

fig.axes[0].set_title("uncertainty sources for " + obslab)
fig.axes[0].set_xlabel(obslab + " bin")
fig.axes[0].set_ylabel("$b$-jet fraction uncertainty")
plt.ylim((0, axdict[obsname]))

plt.savefig(outfile)


with open(logfile, 'w') as f:
    f.write('\n'.join(s))
