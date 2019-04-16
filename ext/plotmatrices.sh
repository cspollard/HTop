mkdir -p unfold/particlelevel/closure
mkdir -p unfold/particlelevel/closure_statonly
mkdir -p unfold/particlelevel/stress_herwig
mkdir -p unfold/particlelevel/data

rivet-mkhtml -c ext/htop.plot \
  -m ".*migeffY?[0-9]*$" \
  unfold/particlelevel/closure/unfoldzblc/nominal.yoda \
  unfold/particlelevel/closure/unfoldzblc/fsr.yoda \
  unfold/particlelevel/closure/unfoldzblc/rad.yoda \
  unfold/particlelevel/closure/unfoldzblc/ps.yoda \
  unfold/particlelevel/closure/unfoldzblc/puwgt.yoda \
  unfold/particlelevel/closure/unfoldzblc/v2trk_fake_rate_tight.yoda \
  unfold/particlelevel/closure/unfoldzblc/v2trk_res_d0_meas.yoda \
  unfold/particlelevel/closure/unfoldzblc/v2trk_res_z0_meas.yoda \
  -o unfold/particlelevel/closure/zblcplots

make-plots --pdf unfold/particlelevel/closure/zblcplots/*/*dat


rivet-mkhtml -c ext/htop.plot \
  -m ".*migeffY?[0-9]*$" \
  unfold/particlelevel/closure/unfoldzbtc/nominal.yoda \
  unfold/particlelevel/closure/unfoldzbtc/fsr.yoda \
  unfold/particlelevel/closure/unfoldzbtc/rad.yoda \
  unfold/particlelevel/closure/unfoldzbtc/ps.yoda \
  unfold/particlelevel/closure/unfoldzbtc/puwgt.yoda \
  unfold/particlelevel/closure/unfoldzbtc/v2trk_fake_rate_tight.yoda \
  unfold/particlelevel/closure/unfoldzbtc/v2trk_res_d0_meas.yoda \
  unfold/particlelevel/closure/unfoldzbtc/v2trk_res_z0_meas.yoda \
  -o unfold/particlelevel/closure/zbtcplots

make-plots --pdf unfold/particlelevel/closure/zbtcplots/*/*dat


rivet-mkhtml -c ext/htop.plot \
  -m ".*migeffY?[0-9]*$" \
  unfold/particlelevel/closure/unfoldzbrelc/nominal.yoda \
  unfold/particlelevel/closure/unfoldzbrelc/fsr.yoda \
  unfold/particlelevel/closure/unfoldzbrelc/rad.yoda \
  unfold/particlelevel/closure/unfoldzbrelc/ps.yoda \
  unfold/particlelevel/closure/unfoldzbrelc/puwgt.yoda \
  unfold/particlelevel/closure/unfoldzbrelc/v2trk_fake_rate_tight.yoda \
  unfold/particlelevel/closure/unfoldzbrelc/v2trk_res_d0_meas.yoda \
  unfold/particlelevel/closure/unfoldzbrelc/v2trk_res_z0_meas.yoda \
  -o unfold/particlelevel/closure/zbrelcplots


