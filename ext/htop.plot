# BEGIN PLOT /*
LogX=0
LogY=0
Legend=1
RatioPlotYMin=0.50
RatioPlotYMax=1.50
# END PLOT

# BEGIN PLOT /.*zbtc?(norm)?$
LegendXPos=0.1
# END PLOT

# BEGIN PLOT /.*zblc?(norm)?$
LegendXPos=0.1
# END PLOT

# BEGIN PLOT /.*ptc?$
LogY=1
# END PLOT


# BEGIN PLOT /.*migreldiff$
LogZ=0
Legend=0
ZMin=-0.25
ZMax=0.25
ZLabel=relative probability difference
# END PLOT

# BEGIN PLOT /.*migdiff$
LogZ=0
Legend=0
ZMin=-0.01
ZMax=0.01
ZLabel=probability difference
# END PLOT

# BEGIN PLOT /.*migeff$
LogZ=0
Legend=0
ZMin=0
ZMax=0.20
ZLabel=probability
# END PLOT

# BEGIN PLOT /.*vs.*
Legend=0
PolyMarker=
# END PLOT

# BEGIN PLOT /.*data\.yoda.*
PolyMarker=*
color=black
# END PLOT
