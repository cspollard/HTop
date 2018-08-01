# TODO
# there is a problem here: certain migration matrices are not being
# produced.

TWOD= #--2d
DIR=`pwd -P`

mkdir -p unfold/particlelevel/data/zblcposteriors
cd unfold/particlelevel/data/zblcposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/data/zblcmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/data/unfoldzblc/*yoda -o unfold/particlelevel/data/zblcplots

mkdir -p unfold/particlelevel/closure/zblcposteriors
cd unfold/particlelevel/closure/zblcposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/closure/zblcmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/closure/unfoldzblc/*yoda -o unfold/particlelevel/closure/zblcplots
make-plots --pdf unfold/particlelevel/closure/zblcplots/Users/*dat

mkdir -p unfold/particlelevel/data/zblposteriors
cd unfold/particlelevel/data/zblposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/data/zblmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/data/unfoldzbl/*yoda -o unfold/particlelevel/data/zblplots

mkdir -p unfold/particlelevel/closure/zblposteriors
cd unfold/particlelevel/closure/zblposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/closure/zblmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/closure/unfoldzbl/*yoda -o unfold/particlelevel/closure/zblplots
make-plots --pdf unfold/particlelevel/closure/zblplots/Users/*dat

mkdir -p unfold/particlelevel/data/zbtcposteriors
cd unfold/particlelevel/data/zbtcposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/data/zbtcmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/data/unfoldzbtc/*yoda -o unfold/particlelevel/data/zbtcplots

mkdir -p unfold/particlelevel/closure/zbtcposteriors
cd unfold/particlelevel/closure/zbtcposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/closure/zbtcmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/closure/unfoldzbtc/*yoda -o unfold/particlelevel/closure/zbtcplots
make-plots --pdf unfold/particlelevel/closure/zbtcplots/Users/*dat

mkdir -p unfold/particlelevel/data/zbtposteriors
cd unfold/particlelevel/data/zbtposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/data/zbtmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/data/unfoldzbt/*yoda -o unfold/particlelevel/data/zbtplots

mkdir -p unfold/particlelevel/closure/zbtposteriors
cd unfold/particlelevel/closure/zbtposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/closure/zbtmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/closure/unfoldzbt/*yoda -o unfold/particlelevel/closure/zbtplots
make-plots --pdf unfold/particlelevel/closure/zbtplots/Users/*dat

mkdir -p unfold/particlelevel/data/zbrelcposteriors
cd unfold/particlelevel/data/zbrelcposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/data/zbrelcmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/data/unfoldzbrelc/*yoda -o unfold/particlelevel/data/zbrelcplots

mkdir -p unfold/particlelevel/closure/zbrelcposteriors
cd unfold/particlelevel/closure/zbrelcposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/closure/zbrelcmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/closure/unfoldzbrelc/*yoda -o unfold/particlelevel/closure/zbrelcplots
make-plots --pdf unfold/particlelevel/closure/zbrelcplots/Users/*dat

mkdir -p unfold/particlelevel/data/zbrelposteriors
cd unfold/particlelevel/data/zbrelposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/data/zbrelmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/data/unfoldzbrel/*yoda -o unfold/particlelevel/data/zbrelplots

mkdir -p unfold/particlelevel/closure/zbrelposteriors
cd unfold/particlelevel/closure/zbrelposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/closure/zbrelmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR
rivet-mkhtml -c ext/htop.plot unfold/particlelevel/closure/unfoldzbrel/*yoda -o unfold/particlelevel/closure/zbrelplots
make-plots --pdf unfold/particlelevel/closure/zbrelplots/Users/*dat
