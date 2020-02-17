rivet-mkhtml -m ".*/elmujj/.*" --mc-errs --pwd -c ext/htop.plot \
  yoda/total.yoda \
  yoda/background.yoda:"backgrounds" \
  yoda/data.yoda:"LineColor=black" \
  -o detectorlevel/total

rivet-mkhtml -m ".*/elmujj/.*" --mc-errs --pwd -c ext/htop.plot \
  yoda/nominal.yoda \
  yoda/ps.yoda:"parton shower" \
  yoda/fsr.yoda:"FSR" \
  yoda/rad.yoda:"ISR" \
  yoda/puwgt.yoda:"pileup" \
  yoda/ptcsf.yoda:"jet $p_T^c$" \
  yoda/background.yoda:"backgrounds" \
  yoda/data.yoda:"LineColor=black" \
  -o detectorlevel/modeling

rivet-mkhtml -m ".*/elmujj/.*" --mc-errs --pwd -c ext/htop.plot \
  yoda/nominal.yoda yoda/nsvtrksf.yoda:"SV track multiplicity" \
  yoda/v2trk_fake_rate_tight.yoda:"track fake rate" \
  yoda/v2trk_eff_tight_global.yoda:"track efficiency" \
  yoda/v2trk_res_d0_meas.yoda:"track \$d_0\$" \
  yoda/v2trk_res_z0_meas.yoda:"track \$z_0\$" \
  yoda/background.yoda:"backgrounds" \
  yoda/data.yoda:"LineColor=black" \
  -o detectorlevel/tracking
