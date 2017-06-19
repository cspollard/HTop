# BEGIN PLOT /*
LogX=0
LogY=0
Legend=0
RatioPlotYMin=0.95
RatioPlotYMax=1.05
# END PLOT

# BEGIN PLOT /.*jets/pt$
Rebin=4
# END PLOT

# BEGIN PLOT .*mu$
Legend=1
# END PLOT

# TODO
# this is a hack to get the Z-axis scales correct but will break for a
# different set of bin sizes!
# BEGIN PLOT .*mig$
ZMin=0
ZMax=20
# END PLOT
