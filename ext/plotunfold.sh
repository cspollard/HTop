mkdir -p particlelevel/zblcposteriors
cd particlelevel/zblcposteriors
python ../../../hunfold.git/ext/plot.py < ../zblcmcmc.dat
cd ../../
rivet-mkhtml -c ext/htop.plot particlelevel/unfoldzblc/*yoda -o particlelevel/zblcplots
make-plots --pdf particlelevel/zblcplots/Users/*dat

mkdir -p particlelevel/zbtcposteriors
cd particlelevel/zbtcposteriors
python ../../../hunfold.git/ext/plot.py < ../zbtcmcmc.dat
cd ../../
rivet-mkhtml -c ext/htop.plot particlelevel/unfoldzbtc/*yoda -o particlelevel/zbtcplots
make-plots --pdf particlelevel/zbtcplots/Users/*dat

mkdir -p particlelevel/zbtrelcposteriors
cd particlelevel/zbtrelcposteriors
python ../../../hunfold.git/ext/plot.py < ../zbtrelcmcmc.dat
cd ../../
rivet-mkhtml -c ext/htop.plot particlelevel/unfoldzbtrelc/*yoda -o particlelevel/zbtrelcplots
make-plots --pdf particlelevel/zbtrelcplots/Users/*dat


mkdir -p detectorlevel/zblcposteriors
cd detectorlevel/zblcposteriors
python ../../../hunfold.git/ext/plot.py < ../zblcmcmc.dat
cd ../../

mkdir -p detectorlevel/zbtcposteriors
cd detectorlevel/zbtcposteriors
python ../../../hunfold.git/ext/plot.py < ../zbtcmcmc.dat
cd ../../

mkdir -p detectorlevel/zbtrelcposteriors
cd detectorlevel/zbtrelcposteriors
python ../../../hunfold.git/ext/plot.py < ../zbtrelcmcmc.dat
cd ../../
