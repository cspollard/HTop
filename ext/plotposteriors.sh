TWOD= #--2d

plotposteriors () {

  DIR=`pwd -P`
  MCMC=$DIR/unfold/mcmc/$1/$2mcmc.dat

  mkdir -p unfold/particlelevel/$1/$2posteriors

  cd unfold/particlelevel/$1/$2posteriors

  python $DIR/../hunfold.git/ext/plot.py $TWOD < $MCMC

  gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf

  python $DIR/ext/uncertainties.py $MCMC $2uncerts.pdf $2

  cd $DIR

}


for vers in data closure closure_statonly ps
do
  for obs in zblc rho nsvtrk zbtc
  do
    plotposteriors $vers $obs
  done
done
