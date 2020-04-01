TWOD= #--2d

plotposteriors () {

  DIR=`pwd -P`

  mkdir -p unfold/particlelevel/$1/$2posteriors

  cd unfold/particlelevel/$1/$2posteriors

  python $DIR/../hunfold.git/ext/plot.py $TWOD < $DIR/unfold/mcmc/$1/$2mcmc.dat

  gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=posteriors.pdf *pdf

  cd $DIR

}


for vers in data closure closure_statonly ps
do
  for obs in zblc rho nsvtrk zbtc
  do
    plotposteriors $vers $obs
  done
done
