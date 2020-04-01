rivet-mkhtml -m ".*/elmujj/.*" --mc-errs --pwd --linear -c ext/htop.plot \
  yoda/total.yoda:"LineColor=Black":"ErrorBandColor={[cmyk]{0,0,0,0.15}}":"ErrorBands=1":"ErrorBars=0" \
  yoda/fiducial.yoda:'Title=fiducial $e \mu bb$':"LineColor=Black":"ErrorBars=0":"LineStyle=dashed" \
  yoda/background.yoda:"backgrounds":"LineColor=Black":"ErrorBars=0":"LineStyle=dotted" \
  yoda/data.yoda:"Title=data":"LineColor=Black":"ConnectBins=0" \
  -o detectorlevel/total


rivet-mkhtml -m ".*/elmujj/.*" --mc-errs --linear --pwd -c ext/htop.plot \
  yoda/nominal.yoda \
  yoda/ps.yoda:"parton shower" \
  yoda/fsr.yoda:"FSR" \
  yoda/puwgt.yoda:"pileup" \
  yoda/ptcsf.yoda:"jet \$p_\\mathrm{T}^\\mathrm{ch}\$" \
  yoda/background.yoda:"backgrounds" \
  yoda/data.yoda:"Title=data":"LineColor=Black":"ConnectBins=0" \
  -o detectorlevel/modeling


rivet-mkhtml -m ".*/elmujj/.*" --mc-errs --linear --pwd -c ext/htop.plot \
  yoda/nominal.yoda \
  yoda/nsvtrksf.yoda:"SV track multiplicity" \
  yoda/v2trk_fake_rate_tight.yoda:"track fake rate" \
  yoda/v2trk_eff_tight_global.yoda:"track efficiency" \
  yoda/v2trk_res_d0_meas.yoda:"track \$d_0\$" \
  yoda/v2trk_res_z0_meas.yoda:"track \$z_0\$" \
  yoda/background.yoda:"backgrounds" \
  yoda/data.yoda:"Title=data":"LineColor=Black":"ConnectBins=0" \
  -o detectorlevel/tracking


rivet-mkhtml -m ".*/elmujj/.*norm" --mc-errs --linear --pwd -c ext/htop.plot \
  yoda/PowPy8FS.yoda:'Title=nominal $t \bar{t}$' \
  yoda/background.yoda:"backgrounds" \
  -o detectorlevel/sigbkg
