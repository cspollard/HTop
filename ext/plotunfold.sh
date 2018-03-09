DIR=`pwd -P`

mkdir -p unfold/particlelevel/data/zblcposteriors
cd unfold/particlelevel/data/zblcposteriors
python $DIR/../hunfold.git/ext/plot.py < $DIR/unfold/mcmc/data/zblcmcmc.dat
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/data/unfoldzblc/*yoda -o unfold/particlelevel/data/zblcplots
make-plots --pdf unfold/particlelevel/zblcplots/data/Users/*dat

mkdir -p unfold/particlelevel/closure/zblcposteriors
cd unfold/particlelevel/closure/zblcposteriors
python $DIR/../hunfold.git/ext/plot.py < $DIR/unfold/mcmc/closure/zblcmcmc.dat
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/closure/unfoldzblc/*yoda -o unfold/particlelevel/closure/zblcplots
make-plots --pdf unfold/particlelevel/closure/zblcplots/Users/*dat

mkdir -p unfold/particlelevel/data/zblposteriors
cd unfold/particlelevel/data/zblposteriors
python $DIR/../hunfold.git/ext/plot.py < $DIR/unfold/mcmc/data/zblmcmc.dat
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/data/unfoldzbl/*yoda -o unfold/particlelevel/data/zblcplots
make-plots --pdf unfold/particlelevel/zblplots/data/Users/*dat

mkdir -p unfold/particlelevel/closure/zblposteriors
cd unfold/particlelevel/closure/zblposteriors
python $DIR/../hunfold.git/ext/plot.py < $DIR/unfold/mcmc/closure/zblmcmc.dat
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/closure/unfoldzbl/*yoda -o unfold/particlelevel/closure/zblcplots
make-plots --pdf unfold/particlelevel/closure/zblplots/Users/*dat

mkdir -p unfold/particlelevel/data/zbtcposteriors
cd unfold/particlelevel/data/zbtcposteriors
python $DIR/../hunfold.git/ext/plot.py < $DIR/unfold/mcmc/data/zbtcmcmc.dat
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/data/unfoldzbtc/*yoda -o unfold/particlelevel/data/zbtcplots
make-plots --pdf unfold/particlelevel/zbtcplots/data/Users/*dat

mkdir -p unfold/particlelevel/closure/zbtcposteriors
cd unfold/particlelevel/closure/zbtcposteriors
python $DIR/../hunfold.git/ext/plot.py < $DIR/unfold/mcmc/closure/zbtcmcmc.dat
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/closure/unfoldzbtc/*yoda -o unfold/particlelevel/closure/zbtcplots
make-plots --pdf unfold/particlelevel/closure/zbtcplots/Users/*dat

mkdir -p unfold/particlelevel/data/zbtposteriors
cd unfold/particlelevel/data/zbtposteriors
python $DIR/../hunfold.git/ext/plot.py < $DIR/unfold/mcmc/data/zbtmcmc.dat
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/data/unfoldzbt/*yoda -o unfold/particlelevel/data/zbtplots
make-plots --pdf unfold/particlelevel/zbtplots/data/Users/*dat

mkdir -p unfold/particlelevel/closure/zbtposteriors
cd unfold/particlelevel/closure/zbtposteriors
python $DIR/../hunfold.git/ext/plot.py < $DIR/unfold/mcmc/closure/zbtmcmc.dat
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/closure/unfoldzbtc/*yoda -o unfold/particlelevel/closure/zbtcplots
make-plots --pdf unfold/particlelevel/closure/zbtcplots/Users/*dat

mkdir -p unfold/particlelevel/data/zbtrelcposteriors
cd unfold/particlelevel/data/zbtrelcposteriors
python $DIR/../hunfold.git/ext/plot.py < $DIR/unfold/mcmc/data/zbtrelcmcmc.dat
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/data/unfoldzbtrelc/*yoda -o unfold/particlelevel/data/zbtrelcplots
make-plots --pdf unfold/particlelevel/zbtrelcplots/data/Users/*dat

mkdir -p unfold/particlelevel/closure/zbtrelcposteriors
cd unfold/particlelevel/closure/zbtrelcposteriors
python $DIR/../hunfold.git/ext/plot.py < $DIR/unfold/mcmc/closure/zbtrelcmcmc.dat
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/closure/unfoldzbtrelc/*yoda -o unfold/particlelevel/closure/zbtrelcplots
make-plots --pdf unfold/particlelevel/closure/zbtrelcplots/Users/*dat

mkdir -p unfold/particlelevel/data/zbtrelposteriors
cd unfold/particlelevel/data/zbtrelposteriors
python $DIR/../hunfold.git/ext/plot.py < $DIR/unfold/mcmc/data/zbtrelmcmc.dat
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/data/unfoldzbtrel/*yoda -o unfold/particlelevel/data/zbtrelplots
make-plots --pdf unfold/particlelevel/zbtrelplots/data/Users/*dat

mkdir -p unfold/particlelevel/closure/zbtrelposteriors
cd unfold/particlelevel/closure/zbtrelposteriors
python $DIR/../hunfold.git/ext/plot.py < $DIR/unfold/mcmc/closure/zbtrelmcmc.dat
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/closure/unfoldzbtrel/*yoda -o unfold/particlelevel/closure/zbtrelplots
make-plots --pdf unfold/particlelevel/closure/zbtrelplots/Users/*dat
