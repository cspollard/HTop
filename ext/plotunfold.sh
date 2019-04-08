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


mkdir -p unfold/particlelevel/stress_herwig/zblcposteriors
cd unfold/particlelevel/stress_herwig/zblcposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/stress_herwig/zblcmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR



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


mkdir -p unfold/particlelevel/stress_herwig/zbtcposteriors
cd unfold/particlelevel/stress_herwig/zbtcposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/stress_herwig/zbtcmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR


mkdir -p unfold/particlelevel/data/zbrelcposteriors
cd unfold/particlelevel/data/zbrelcposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/data/zbrelcmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR


mkdir -p unfold/particlelevel/closure/zbrelcposteriors
cd unfold/particlelevel/closure/zbrelcposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/closure/zbrelcmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR

rivet-mkhtml -c ext/htop.plot \
  unfold/particlelevel/closure/unfoldzbrelc/nominal.yoda \
  unfold/particlelevel/closure/unfoldzbrelc/fsr.yoda \
  unfold/particlelevel/closure/unfoldzbrelc/puwgt.yoda \
  unfold/particlelevel/closure/unfoldzbrelc/v2trk_fake_rate_tight.yoda \
  unfold/particlelevel/closure/unfoldzbrelc/v2trk_res_d0_meas.yoda \
  unfold/particlelevel/closure/unfoldzbrelc/v2trk_res_z0_meas.yoda \
  -o unfold/particlelevel/closure/zbrelcplots

make-plots --pdf unfold/particlelevel/closure/zbrelcplots/*/*dat


mkdir -p unfold/particlelevel/stress_herwig/zbrelcposteriors
cd unfold/particlelevel/stress_herwig/zbrelcposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/stress_herwig/zbrelcmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR
