mkdir -p closuredata/posteriors
cd closuredata/posteriors
python ../../../hunfold.git/ext/plot.py < ../data.dat

cd ../../

mkdir -p realdata/posteriors
cd realdata/posteriors
python ../../../hunfold.git/ext/plot.py < ../data.dat

cd ../../

rivet-mkhtml -c ext/htop.plot closuredata/unfoldyoda/*yoda -o closuredata/unfoldplots
make-plots --pdfpng closuredata/unfoldplots/Users/*dat
