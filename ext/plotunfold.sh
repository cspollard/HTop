mkdir -p closuredata/zblcposteriors
cd closuredata/zblcposteriors
python ../../../hunfold.git/ext/plot.py < ../zblcmcmc.dat
cd ../../
rivet-mkhtml -c ext/htop.plot closuredata/unfoldzblc/*yoda -o closuredata/zblcplots
make-plots --pdfpng closuredata/zblcplots/Users/*dat

mkdir -p closuredata/zbtcposteriors
cd closuredata/zbtcposteriors
python ../../../hunfold.git/ext/plot.py < ../zbtcmcmc.dat
cd ../../
rivet-mkhtml -c ext/htop.plot closuredata/unfoldzbtc/*yoda -o closuredata/zbtcplots
make-plots --pdfpng closuredata/zbtcplots/Users/*dat

mkdir -p closuredata/zbtrelcposteriors
cd closuredata/zbtrelcposteriors
python ../../../hunfold.git/ext/plot.py < ../zbtrelcmcmc.dat
cd ../../
rivet-mkhtml -c ext/htop.plot closuredata/unfoldzbtrelc/*yoda -o closuredata/zbtrelcplots
make-plots --pdfpng closuredata/zbtrelcplots/Users/*dat


mkdir -p realdata/zblcposteriors
cd realdata/zblcposteriors
python ../../../hunfold.git/ext/plot.py < ../zblcmcmc.dat
cd ../../
rivet-mkhtml -c ext/htop.plot realdata/unfoldzblc/*yoda -o realdata/zblcplots
make-plots --pdfpng realdata/zblcplots/Users/*dat

mkdir -p realdata/zbtcposteriors
cd realdata/zbtcposteriors
python ../../../hunfold.git/ext/plot.py < ../zbtcmcmc.dat
cd ../../
rivet-mkhtml -c ext/htop.plot realdata/unfoldzbtc/*yoda -o realdata/zbtcplots
make-plots --pdfpng realdata/zbtcplots/Users/*dat

mkdir -p realdata/zbtrelcposteriors
cd realdata/zbtrelcposteriors
python ../../../hunfold.git/ext/plot.py < ../zbtrelcmcmc.dat
cd ../../
rivet-mkhtml -c ext/htop.plot realdata/unfoldzbtrelc/*yoda -o realdata/zbtrelcplots
make-plots --pdfpng realdata/zbtrelcplots/Users/*dat

