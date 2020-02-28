mkdir -p unfold/particlelevel/closure
mkdir -p unfold/particlelevel/closure_statonly
mkdir -p unfold/particlelevel/stress_herwig
mkdir -p unfold/particlelevel/data


rivet-mkhtml -c ext/htop.plot \
  -m ".*migeffY?[0-9]*$" \
  unfold/particlelevel/closure/unfoldzblc/nominal.yoda \
  unfold/particlelevel/closure/unfoldzblc/fsr.yoda \
  unfold/particlelevel/closure/unfoldzblc/rad.yoda \
  unfold/particlelevel/closure/unfoldzblc/ptcsf.yoda \
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
  unfold/particlelevel/closure/unfoldzbtc/ptcsf.yoda \
  unfold/particlelevel/closure/unfoldzbtc/ps.yoda \
  unfold/particlelevel/closure/unfoldzbtc/puwgt.yoda \
  unfold/particlelevel/closure/unfoldzbtc/v2trk_fake_rate_tight.yoda \
  unfold/particlelevel/closure/unfoldzbtc/v2trk_res_d0_meas.yoda \
  unfold/particlelevel/closure/unfoldzbtc/v2trk_res_z0_meas.yoda \
  -o unfold/particlelevel/closure/zbtcplots

make-plots --pdf unfold/particlelevel/closure/zbtcplots/*/*dat


rivet-mkhtml -c ext/htop.plot \
  -m ".*migeffY?[0-9]*$" \
  unfold/particlelevel/closure/unfoldnsvtrk/nominal.yoda \
  unfold/particlelevel/closure/unfoldnsvtrk/fsr.yoda \
  unfold/particlelevel/closure/unfoldnsvtrk/rad.yoda \
  unfold/particlelevel/closure/unfoldnsvtrk/ptcsf.yoda \
  unfold/particlelevel/closure/unfoldnsvtrk/ps.yoda \
  unfold/particlelevel/closure/unfoldnsvtrk/puwgt.yoda \
  unfold/particlelevel/closure/unfoldnsvtrk/v2trk_fake_rate_tight.yoda \
  unfold/particlelevel/closure/unfoldnsvtrk/v2trk_res_d0_meas.yoda \
  unfold/particlelevel/closure/unfoldnsvtrk/v2trk_res_z0_meas.yoda \
  -o unfold/particlelevel/closure/nsvtrkplots


make-plots --pdf unfold/particlelevel/closure/nsvtrkplots/*/*dat


rivet-mkhtml -c ext/htop.plot \
  -m ".*migeffY?[0-9]*$" \
  unfold/particlelevel/closure/unfoldrho/nominal.yoda \
  unfold/particlelevel/closure/unfoldrho/fsr.yoda \
  unfold/particlelevel/closure/unfoldrho/rad.yoda \
  unfold/particlelevel/closure/unfoldrho/ptcsf.yoda \
  unfold/particlelevel/closure/unfoldrho/ps.yoda \
  unfold/particlelevel/closure/unfoldrho/puwgt.yoda \
  unfold/particlelevel/closure/unfoldrho/v2trk_fake_rate_tight.yoda \
  unfold/particlelevel/closure/unfoldrho/v2trk_res_d0_meas.yoda \
  unfold/particlelevel/closure/unfoldrho/v2trk_res_z0_meas.yoda \
  -o unfold/particlelevel/closure/rhoplots

make-plots --pdf unfold/particlelevel/closure/rhoplots/*/*dat


