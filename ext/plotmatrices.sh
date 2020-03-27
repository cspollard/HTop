plotmatrices() {

  if [[ -a unfold/particlelevel/closure/unfold$1/ptcsf.yoda ]]
  then
    FILE=unfold/particlelevel/closure/unfold$1/ptcsf.yoda:"\$p_\\mathrm{T}^\\mathrm{ch}\$"
  else
    FILE=
  fi
  

  rivet-mkhtml -c ext/htop.plot \
    -m ".*migeffY?[0-9]*$" \
    unfold/particlelevel/closure/unfold$1/nominal.yoda:"nominal" \
    unfold/particlelevel/closure/unfold$1/fsr.yoda:"fsr" \
    $FILE \
    unfold/particlelevel/closure/unfold$1/ps.yoda:"ps" \
    unfold/particlelevel/closure/unfold$1/puwgt.yoda:"puwgt" \
    unfold/particlelevel/closure/unfold$1/v2trk_fake_rate_tight.yoda:"track fake rate"\
    unfold/particlelevel/closure/unfold$1/v2trk_res_d0_meas.yoda:"track d0 resolution" \
    unfold/particlelevel/closure/unfold$1/v2trk_res_z0_meas.yoda:"track z0 resolution" \
    -o unfold/particlelevel/closure/$1plots

  make-plots --pdf unfold/particlelevel/closure/$1plots/*/*dat

}


for obs in zblc nsvtrk rho zbtc
do
  plotmatrices $obs
done


# mkdir -p unfold/particlelevel/closure
# 
# 
# rivet-mkhtml -c ext/htop.plot \
#   -m ".*migeffY?[0-9]*$" \
#   unfold/particlelevel/closure/unfoldzbtc/nominal.yoda \
#   unfold/particlelevel/closure/unfoldzbtc/fsr.yoda \
#   unfold/particlelevel/closure/unfoldzbtc/rad.yoda \
#   unfold/particlelevel/closure/unfoldzbtc/ptcsf.yoda \
#   unfold/particlelevel/closure/unfoldzbtc/ps.yoda \
#   unfold/particlelevel/closure/unfoldzbtc/puwgt.yoda \
#   unfold/particlelevel/closure/unfoldzbtc/v2trk_fake_rate_tight.yoda \
#   unfold/particlelevel/closure/unfoldzbtc/v2trk_res_d0_meas.yoda \
#   unfold/particlelevel/closure/unfoldzbtc/v2trk_res_z0_meas.yoda \
#   -o unfold/particlelevel/closure/zbtcplots
# 
# make-plots --pdf unfold/particlelevel/closure/zbtcplots/*/*dat
# 
# 
# rivet-mkhtml -c ext/htop.plot \
#   -m ".*migeffY?[0-9]*$" \
#   unfold/particlelevel/closure/unfoldnsvtrk/nominal.yoda \
#   unfold/particlelevel/closure/unfoldnsvtrk/fsr.yoda \
#   unfold/particlelevel/closure/unfoldnsvtrk/rad.yoda \
#   unfold/particlelevel/closure/unfoldnsvtrk/ptcsf.yoda \
#   unfold/particlelevel/closure/unfoldnsvtrk/ps.yoda \
#   unfold/particlelevel/closure/unfoldnsvtrk/puwgt.yoda \
#   unfold/particlelevel/closure/unfoldnsvtrk/v2trk_fake_rate_tight.yoda \
#   unfold/particlelevel/closure/unfoldnsvtrk/v2trk_res_d0_meas.yoda \
#   unfold/particlelevel/closure/unfoldnsvtrk/v2trk_res_z0_meas.yoda \
#   -o unfold/particlelevel/closure/nsvtrkplots
# 
# 
# make-plots --pdf unfold/particlelevel/closure/nsvtrkplots/*/*dat
# 
# 
# rivet-mkhtml -c ext/htop.plot \
#   -m ".*migeffY?[0-9]*$" \
#   unfold/particlelevel/closure/unfoldrho/nominal.yoda \
#   unfold/particlelevel/closure/unfoldrho/fsr.yoda \
#   unfold/particlelevel/closure/unfoldrho/rad.yoda \
#   unfold/particlelevel/closure/unfoldrho/ps.yoda \
#   unfold/particlelevel/closure/unfoldrho/puwgt.yoda \
#   unfold/particlelevel/closure/unfoldrho/v2trk_fake_rate_tight.yoda \
#   unfold/particlelevel/closure/unfoldrho/v2trk_res_d0_meas.yoda \
#   unfold/particlelevel/closure/unfoldrho/v2trk_res_z0_meas.yoda \
#   -o unfold/particlelevel/closure/rhoplots
# 
# make-plots --pdf unfold/particlelevel/closure/rhoplots/*/*dat
