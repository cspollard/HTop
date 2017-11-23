mkdir -p particlelevel/data/zblcposteriors
cd particlelevel/data/zblcposteriors
python ../../../../hunfold.git/ext/plot.py < ../../../mcmc/data/zblcmcmc.dat
cd ../../../
rivet-mkhtml -c ext/htop.plot particlelevel/data/unfoldzblc/*yoda -o particlelevel/data/zblcplots
make-plots --pdf particlelevel/zblcplots/data/Users/*dat

mkdir -p particlelevel/closure/zblcposteriors
cd particlelevel/closure/zblcposteriors
python ../../../../hunfold.git/ext/plot.py < ../../../mcmc/closure/zblcmcmc.dat
cd ../../../
rivet-mkhtml -c ext/htop.plot particlelevel/closure/unfoldzblc/*yoda -o particlelevel/closure/zblcplots
make-plots --pdf particlelevel/closure/zblcplots/Users/*dat

mkdir -p particlelevel/data/zbtcposteriors
cd particlelevel/data/zbtcposteriors
python ../../../../hunfold.git/ext/plot.py < ../../../mcmc/data/zbtcmcmc.dat
cd ../../../
rivet-mkhtml -c ext/htop.plot particlelevel/data/unfoldzbtc/*yoda -o particlelevel/data/zbtcplots
make-plots --pdf particlelevel/zbtcplots/data/Users/*dat

mkdir -p particlelevel/closure/zbtcposteriors
cd particlelevel/closure/zbtcposteriors
python ../../../../hunfold.git/ext/plot.py < ../../../mcmc/closure/zbtcmcmc.dat
cd ../../../
rivet-mkhtml -c ext/htop.plot particlelevel/closure/unfoldzbtc/*yoda -o particlelevel/closure/zbtcplots
make-plots --pdf particlelevel/closure/zbtcplots/Users/*dat

mkdir -p particlelevel/data/zbtrelcposteriors
cd particlelevel/data/zbtrelcposteriors
python ../../../../hunfold.git/ext/plot.py < ../../../mcmc/data/zbtrelcmcmc.dat
cd ../../../
rivet-mkhtml -c ext/htop.plot particlelevel/data/unfoldzbtrelc/*yoda -o particlelevel/data/zbtrelcplots
make-plots --pdf particlelevel/zbtrelcplots/data/Users/*dat

mkdir -p particlelevel/closure/zbtrelcposteriors
cd particlelevel/closure/zbtrelcposteriors
python ../../../../hunfold.git/ext/plot.py < ../../../mcmc/closure/zbtrelcmcmc.dat
cd ../../../
rivet-mkhtml -c ext/htop.plot particlelevel/closure/unfoldzbtrelc/*yoda -o particlelevel/closure/zbtrelcplots
make-plots --pdf particlelevel/closure/zbtrelcplots/Users/*dat

