yodamerge --s2dmode first \
	unfold/particlelevel/closure/unfoldnsvtrk/total.yoda \
	unfold/particlelevel/closure/unfoldrho/total.yoda \
	unfold/particlelevel/closure/unfoldzblc/total.yoda \
	unfold/particlelevel/closure/unfoldzbtc/total.yoda \
	yoda/total.yoda \
	-o total.yoda


# remove normalized hists from fiducial, background
yoda2yoda -M "norm" yoda/fiducial.yoda ./fiducial.yoda
yoda2yoda -M "norm" yoda/background.yoda ./background.yoda


rivet-mkhtml -m ".*/elmujj/.*" -M ".*mu_.*" --mc-errs --single \
  --font helvetica -c ext/htop.plot \
  total.yoda:'Title=Total prediction \\ with uncertainty':"LineColor=Black":"ErrorBandColor={[cmyk]{0,0,0,0.15}}":"ErrorBands=1":"ErrorBars=0":"LineWidth=0.03"  \
  fiducial.yoda:'Title=Fiducial $e \mu bb$':"LineColor=Black":"ErrorBars=0":"LineStyle=dashdotted":"LineWidth=0.03" \
  background.yoda:'Non-$t\bar{t}$ background':"LineColor=Black":"ErrorBars=0":"LineStyle=dashed":"LineWidth=0.03" \
  yoda/data.yoda:"Title=Data":"LineColor=Black":"ConnectBins=0":"HorizLine=0" \
  -o detectorlevel/total


rivet-mkhtml -m ".*/elmujj/.*" -M ".*mu_.*" --mc-errs --single \
  --font helvetica -c ext/htop.chi2.plot \
  yoda/total.yoda:'Title=Total prediction \\ with uncertainty':"LineColor=Black":"ErrorBandColor={[cmyk]{0,0,0,0.15}}":"ErrorBands=1":"ErrorBars=0":"LineWidth=0.03" \
  fiducial.yoda:'Title=Fiducial $e \mu bb$':"LineColor=Black":"ErrorBars=0":"LineStyle=dashdotted":"LineWidth=0.03"  \
  background.yoda:'Non-$t\bar{t}$ background':"LineColor=Black":"ErrorBars=0":"LineStyle=dashed":"LineWidth=0.03"  \
  yoda/data.yoda:"Title=Data":"LineColor=Black":"ConnectBins=0":"HorizLine=0" \
  -o detectorlevel/totalchi2


yodamerge -o htop.yoda unfold/particlelevel/data/unfold*/htop.yoda
mkdir -p detectorlevel/recoclosure/data

rivet-mkhtml --mc-errs --single \
  -m "/htop/elmujj/probejets/(rho|zbtc|zblc|nsvtrk)$" \
  --font helvetica -c ext/htop.plot \
  total.yoda:'Title=Prior prediction \\ with uncertainty':"LineColor=Black":"ErrorBandColor={[cmyk]{0,0,0,0.15}}":"ErrorBands=1":"ErrorBars=0" \
  htop.yoda:'Title=Posterior prediction \\ with uncertainty':"LineColor=green":"ErrorBandColor={[cmyk]{1,0,1,0.10}}":"ErrorBands=1":"ErrorBars=0" \
  yoda/data.yoda:"Title=Data":"LineColor=Black":"ConnectBins=0":"HorizLine=0" \
  -o detectorlevel/recoclosure/data


yodamerge -o htop.yoda unfold/particlelevel/ps/unfold*/htop.yoda
mkdir -p detectorlevel/recoclosure/ps


rivet-mkhtml --mc-errs --single \
  -m "/htop/elmujj/probejets/(rho|zbtc|zblc|nsvtrk)$" \
  --font helvetica -c ext/htop.plot \
  total.yoda:'Title=Prior prediction with uncertainty':"LineColor=Black":"ErrorBandColor={[cmyk]{0,0,0,0.15}}":"ErrorBands=1":"ErrorBars=0" \
  htop.yoda:'Title=Posterior prediction with uncertainty':"LineColor=green":"ErrorBandColor={[cmyk]{1,0,1,0.10}}":"ErrorBands=1":"ErrorBars=0" \
  yoda/ps.yoda:"Title=Data":"LineColor=black":"ConnectBins=0":"HorizLine=0" \
  -o detectorlevel/recoclosure/ps


rivet-mkhtml -m ".*/elmujj/.*" -M ".*mu_.*" --mc-errs --single \
  -c ext/htop.breakdown.plot \
  yoda/nominal.yoda \
  yoda/ps.yoda:"parton shower" \
  yoda/fsr.yoda:"FSR" \
  yoda/puwgt.yoda:"pileup" \
  yoda/ptcsf.yoda:"jet \$p_\\mathrm{T}^\\mathrm{ch}\$" \
  yoda/data.yoda:"Title=Data":"LineColor=Black":"ConnectBins=0":"HorizLine=0" \
  -o detectorlevel/modeling


rivet-mkhtml -m ".*/elmujj/.*" -M ".*mu_.*" --mc-errs --single \
  -c ext/htop.breakdown.plot \
  yoda/nominal.yoda \
  yoda/nsvtrksf.yoda:"SV track multiplicity" \
  yoda/v2trk_fake_rate_tight.yoda:"track fake rate" \
  yoda/v2trk_eff_tight_global.yoda:"track efficiency" \
  yoda/v2trk_res_d0_meas.yoda:"track \$d_0\$" \
  yoda/v2trk_res_z0_meas.yoda:"track \$z_0\$" \
  yoda/data.yoda:"Title=Data":"LineColor=Black":"ConnectBins=0":"HorizLine=0" \
  -o detectorlevel/tracking




# rivet-mkhtml -m ".*/elmujj/.*norm" --single --mc-errs --pwd -c ext/htop.plot \
#   yoda/PowPy8FS.yoda:'Title=nominal $t \bar{t}$' \
#   yoda/background.yoda:"backgrounds" \
#   -o detectorlevel/sigbkg

rm -f fiducial.yoda background.yoda total.yoda htop.yoda
