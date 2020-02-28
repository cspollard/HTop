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


mkdir -p unfold/particlelevel/stress_herwig/zblcposteriors
cd unfold/particlelevel/stress_herwig/zblcposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/stress_herwig/zblcmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR


mkdir -p unfold/particlelevel/data/rhoposteriors
cd unfold/particlelevel/data/rhoposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/data/rhomcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR


mkdir -p unfold/particlelevel/closure/rhoposteriors
cd unfold/particlelevel/closure/rhoposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/closure/rhomcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR


mkdir -p unfold/particlelevel/stress_herwig/rhoposteriors
cd unfold/particlelevel/stress_herwig/rhoposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/stress_herwig/rhomcmc.dat
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


mkdir -p unfold/particlelevel/stress_herwig/zbtcposteriors
cd unfold/particlelevel/stress_herwig/zbtcposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/stress_herwig/zbtcmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR


mkdir -p unfold/particlelevel/data/nsvtrkposteriors
cd unfold/particlelevel/data/nsvtrkposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/data/nsvtrkmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR


mkdir -p unfold/particlelevel/closure/nsvtrkposteriors
cd unfold/particlelevel/closure/nsvtrkposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/closure/nsvtrkmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR


mkdir -p unfold/particlelevel/stress_herwig/nsvtrkposteriors
cd unfold/particlelevel/stress_herwig/nsvtrkposteriors
python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/stress_herwig/nsvtrkmcmc.dat
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf
cd $DIR
