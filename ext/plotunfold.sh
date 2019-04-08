TWOD= #--2d
DIR=`pwd -P`

mkdir -p unfold/particlelevel/data/zblcposteriors
cd unfold/particlelevel/data/zblcposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/data/zblcmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR


mkdir -p unfold/particlelevel/closure/zblcposteriors
cd unfold/particlelevel/closure/zblcposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/closure/zblcmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR

rivet-mkhtml -c ext/htop.plot \
  unfold/particlelevel/closure/unfoldzblc/nominal.yoda \
  unfold/particlelevel/closure/unfoldzblc/fsr.yoda \
  unfold/particlelevel/closure/unfoldzblc/ps.yoda \
  unfold/particlelevel/closure/unfoldzblc/puwgt.yoda \
  unfold/particlelevel/closure/unfoldzblc/v2trk_fake_rate_tight.yoda \
  unfold/particlelevel/closure/unfoldzblc/v2trk_res_d0_meas.yoda \
  unfold/particlelevel/closure/unfoldzblc/v2trk_res_z0_meas.yoda \
  -o unfold/particlelevel/closure/zblcplots

make-plots --pdf unfold/particlelevel/closure/zblcplots/*/*dat


mkdir -p unfold/particlelevel/data/zbtcposteriors
cd unfold/particlelevel/data/zbtcposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/data/zbtcmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR


mkdir -p unfold/particlelevel/closure/zbtcposteriors
cd unfold/particlelevel/closure/zbtcposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/closure/zbtcmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR

rivet-mkhtml -c ext/htop.plot \
  unfold/particlelevel/closure/unfoldzbtc/nominal.yoda \
  unfold/particlelevel/closure/unfoldzbtc/fsr.yoda \
  unfold/particlelevel/closure/unfoldzbtc/ps.yoda \
  unfold/particlelevel/closure/unfoldzbtc/puwgt.yoda \
  unfold/particlelevel/closure/unfoldzbtc/v2trk_fake_rate_tight.yoda \
  unfold/particlelevel/closure/unfoldzbtc/v2trk_res_d0_meas.yoda \
  unfold/particlelevel/closure/unfoldzbtc/v2trk_res_z0_meas.yoda \
  -o unfold/particlelevel/closure/zbtcplots

make-plots --pdf unfold/particlelevel/closure/zbtcplots/*/*dat


mkdir -p unfold/particlelevel/data/zbtrelcposteriors
cd unfold/particlelevel/data/zbtrelcposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/data/zbtrelcmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR


mkdir -p unfold/particlelevel/closure/zbtrelcposteriors
cd unfold/particlelevel/closure/zbtrelcposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/closure/zbtrelcmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR

rivet-mkhtml -c ext/htop.plot \
  unfold/particlelevel/closure/unfoldzbtrelc/nominal.yoda \
  unfold/particlelevel/closure/unfoldzbtrelc/fsr.yoda \
  unfold/particlelevel/closure/unfoldzbtrelc/puwgt.yoda \
  unfold/particlelevel/closure/unfoldzbtrelc/v2trk_fake_rate_tight.yoda \
  unfold/particlelevel/closure/unfoldzbtrelc/v2trk_res_d0_meas.yoda \
  unfold/particlelevel/closure/unfoldzbtrelc/v2trk_res_z0_meas.yoda \
  -o unfold/particlelevel/closure/zbtrelcplots

make-plots --pdf unfold/particlelevel/closure/zbtrelcplots/*/*dat
