unfold () {
  mkdir -p unfold/mcmc/$1

  mkdir -p unfold/particlelevel/$1/unfold$2

  stack exec run-htop-unfold -- \
    --mcmcfile unfold/mcmc/$1/$2mcmc.dat \
    --nsamples $3 \
    --yodafolder unfold/particlelevel/$1/unfold$2 \
    --xsecfile data/XSection-MC15-13TeV.data \
    --observable $2 hist/*hist.gz \
    --test $1 \
    > unfold/mcmc/$1/$2.log 2>&1

}


for vers in data closure closure_statonly ps # mugt22 mule22
do
  for obs in zblc rho nsvtrk zbtc
  do
    unfold $vers $obs 20000
  done
done
