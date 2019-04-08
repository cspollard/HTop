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
make-plots --pdf unfold/particlelevel/closure/zblcplots/*/*dat


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
make-plots --pdf unfold/particlelevel/closure/zbtcplots/*/*dat


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
make-plots --pdf unfold/particlelevel/closure/zbrelcplots/*/*dat
