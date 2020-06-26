# BEGIN PLOT /*
LogX=0
LogY=0
Legend=1
RatioPlotYMin=0.50
RatioPlotYMax=1.50
RatioPlotDrawReferenceFirst=1
LegendXPos=0.1
# END PLOT

# BEGIN PLOT /.*rho(norm)?$
RatioPlotYMin=0.75
RatioPlotYMax=1.25
RatioPlot1YMin=0.75
RatioPlot1YMax=1.25
RatioPlot2YMin=0.75
RatioPlot2YMax=1.25
RatioPlot3YMin=0.75
RatioPlot3YMax=1.25
LegendXPos=0.4
# END PLOT

# BEGIN PLOT /.*nsvtrk(norm)?$
LegendXPos=0.4
# END PLOT

# BEGIN PLOT /htop/elmujj/.*rho(norm)?$
XLabel=detector jet \ensuremath{\rho}
# END PLOT

# BEGIN PLOT /htop/elmujj/.*rho$
YLabel=\ensuremath{\frac{dn}{d\rho}}
# END PLOT

# BEGIN PLOT /htop/elmujj/.*rhonorm$
YLabel=\ensuremath{\frac{1}{n}\frac{dn}{d\rho}}
# END PLOT


# BEGIN PLOT /htop/elmujj/.*zbtc(norm)?$
XLabel=detector jet \ensuremath{z_{\mathrm{T,b}}^\mathrm{ch}}
# END PLOT

# BEGIN PLOT /htop/elmujj/.*zbtc$
YLabel=\ensuremath{\frac{dn}{z_{\mathrm{T,b}}^\mathrm{ch}}}
# END PLOT

# BEGIN PLOT /htop/elmujj/.*zbtcnorm$
YLabel=\ensuremath{\frac{1}{n}\frac{dn}{dz_{\mathrm{T,b}}^\mathrm{ch}}}
# END PLOT


# BEGIN PLOT /htop/elmujj/.*zblc(norm)?$
XLabel=detector jet \ensuremath{z_{\mathrm{L,b}}^\mathrm{ch}}
# END PLOT

# BEGIN PLOT /htop/elmujj/.*zblc$
YLabel=\ensuremath{\frac{dn}{z_{\mathrm{L,b}}^\mathrm{ch}}}
# END PLOT

# BEGIN PLOT /htop/elmujj/.*zblcnorm$
YLabel=\ensuremath{\frac{1}{n}\frac{dn}{dz_{\mathrm{L,b}}^\mathrm{ch}}}
# END PLOT


# BEGIN PLOT /htop/elmujj/.*nsvtrk(norm)?$
XLabel=detector jet \ensuremath{n_\mathrm{B}^\mathrm{ch}}
# END PLOT

# BEGIN PLOT /htop/elmujj/.*nsvtrk$
YLabel=\ensuremath{\frac{dn}{dn_\mathrm{B}^\mathrm{ch}}}
# END PLOT

# BEGIN PLOT /htop/elmujj/.*nsvtrknorm$
YLabel=\ensuremath{\frac{1}{n}\frac{dn}{dn_\mathrm{B}^\mathrm{ch}}}
# END PLOT



# BEGIN PLOT /htop/elmujjtrue/.*rho(norm)?$
XLabel=particle jet \ensuremath{\rho}
# END PLOT

# BEGIN PLOT /htop/elmujjtrue/.*rho$
YLabel=\ensuremath{\frac{d\sigma}{\rho}}
# END PLOT

# BEGIN PLOT /htop/elmujjtrue/.*rhonorm$
YLabel=\ensuremath{\frac{1}{\sigma}\frac{d\sigma}{\rho}}
# END PLOT


# BEGIN PLOT /htop/elmujjtrue/.*zbtc(norm)?$
XLabel=particle jet \ensuremath{z_{\mathrm{T,b}}^\mathrm{ch}}
# END PLOT

# BEGIN PLOT /htop/elmujjtrue/.*zbtc$
YLabel=\ensuremath{\frac{d\sigma}{z_{\mathrm{T,b}}^\mathrm{ch}}}
# END PLOT

# BEGIN PLOT /htop/elmujjtrue/.*zbtcnorm$
YLabel=\ensuremath{\frac{1}{\sigma}\frac{d\sigma}{dz_{\mathrm{T,b}}^\mathrm{ch}}}
# END PLOT


# BEGIN PLOT /htop/elmujjtrue/.*zblc(norm)?$
XLabel=particle jet \ensuremath{z_{\mathrm{L,b}}^\mathrm{ch}}
# END PLOT

# BEGIN PLOT /htop/elmujjtrue/.*zblc$
YLabel=\ensuremath{\frac{d\sigma}{z_{\mathrm{L,b}}^\mathrm{ch}}}
# END PLOT

# BEGIN PLOT /htop/elmujjtrue/.*zblcnorm$
YLabel=\ensuremath{\frac{1}{\sigma}\frac{d\sigma}{dz_{\mathrm{L,b}}^\mathrm{ch}}}
# END PLOT


# BEGIN PLOT /BFRAG/.*nsvtrk(norm)?$
XLabel=particle jet \ensuremath{n_\mathrm{B}^\mathrm{ch}}
# END PLOT

# BEGIN PLOT /BFRAG/.*nsvtrk$
YLabel=\ensuremath{\frac{d\sigma}{dn_\mathrm{B}^\mathrm{ch}}}
# END PLOT

# BEGIN PLOT /BFRAG/.*nsvtrknorm$
YLabel=\ensuremath{\frac{1}{\sigma}\frac{d\sigma}{dn_\mathrm{B}^\mathrm{ch}}}
# END PLOT



# BEGIN PLOT /htop/elmujjtrue/.*nsvtrk(norm)?$
XLabel=particle jet \ensuremath{n_\mathrm{B}^\mathrm{ch}}
# END PLOT

# BEGIN PLOT /htop/elmujjtrue/.*nsvtrk$
YLabel=\ensuremath{\frac{d\sigma}{dn_\mathrm{B}^\mathrm{ch}}}
# END PLOT

# BEGIN PLOT /htop/elmujjtrue/.*nsvtrknorm$
YLabel=\ensuremath{\frac{1}{\sigma}\frac{d\sigma}{dn_\mathrm{B}^\mathrm{ch}}}
# END PLOT



# BEGIN PLOT /.*ptc?$
LogY=1
# END PLOT

# BEGIN PLOT /.*migeff$
LogZ=0
Legend=0
ZMin=0
ZMax=0.1
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

# BEGIN PLOT /.*zblcmigeffY.*
YLabel=probability
XLabel=detector \ensuremath{z_{\mathrm{L,b}}^\mathrm{ch}}
# END PLOT

# BEGIN PLOT /.*zbtcmigeffY.*
YLabel=probability
XLabel=detector \ensuremath{z_{\mathrm{T,b}}^\mathrm{ch}}
# END PLOT

# BEGIN PLOT /.*nsvtrkmigeffY.*
YLabel=probability
XLabel=detector \ensuremath{n_\mathrm{B}^\mathrm{ch}}
# END PLOT
 
# BEGIN PLOT /.*rhomigeffY.*
YLabel=probability
XLabel=detector \ensuremath{\rho}
# END PLOT

# BEGIN PLOT /BFRAG/.*
CustomLegend=\textit{\textbf{ATLAS} Internal} \\ $\sqrt{s} = 13\,\text{TeV}, 36\,\text{fb}^{-1}$
RatioPlotErrorBandColor={[cmyk]{0,0,0,0.15}}
RatioPlotErrorBandStyle=solid
RatioPlotSameStyle=0
DrawReferenceFirst=1
# END PLOT

# BEGIN PLOT /BFRAG/zbtc
Title=$z_{T,b}^\text{ch} = p_\text{T}^\text{ch}(b) \ / \ p_\text{T}^\text{ch}(\text{jet})$
XLabel=$z_{T,b}^{\mathrm{ch}}$
YLabel=$1/\sigma \, \mathrm{d}\sigma/\mathrm{d}{z_{T,b}^\text{ch}}$
# END PLOT

# BEGIN PLOT /BFRAG/zblc
Title=$z_{L,b}^\text{ch} = p_\text{T}^\text{ch}(b) \cdot p_\text{T}^\text{ch}(\text{jet}) \ / \ | p_\text{T}^\text{ch}(\text{jet}) |^2$
XLabel=$z_{L,b}^{\mathrm{ch}}$
YLabel=$1/\sigma \, \mathrm{d}\sigma/\mathrm{d}{z_{L,b}^\text{ch}}$
# END PLOT

# BEGIN PLOT /BFRAG/nsvtrk
Title=Number of charged $b$-hadron children with $p_\mathrm{T} > 500$ MeV
XLabel=$n_{b}^{\mathrm{ch}}$
YLabel=$1/\sigma \, \mathrm{d}\sigma/\mathrm{d}{n_b^{\mathrm{ch}}}$
LegendXPos=0.4
YMax=0.5
# END PLOT

# BEGIN PLOT /BFRAG/rho
Title=$\rho = 2 p_\mathrm{T}^\mathrm{ch}(b) / (p_\mathrm{T}(e) + p_\mathrm{T}(\mu))$
XLabel=$\rho$
YLabel=$1/\sigma \, \mathrm{d}\sigma/\mathrm{d}{\rho}$
YMax=1.4
# END PLOT
