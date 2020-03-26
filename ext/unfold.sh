unfold () {
  mkdir -p unfold/mcmc/$1

  mkdir -p unfold/particlelevel/$1/unfold$2

  stack exec run-htop-unfold -- \
    --mcmcfile unfold/mcmc/$1/$2mcmc.dat \
    --nsamples $3 \
    --yodafolder unfold/particlelevel/$1/unfold$2 \
    --xsecfile data/XSection-MC15-13TeV.data \
    --observable $2 hist/*hist.gz \
    > unfold/mcmc/$1/$2.log 2>&1

}


for vers in data closure closure_statonly stress_herwig stress_mugt22 stress_mule22
do
  for obs in zblc rho nsvtrk zbtc
  do
    unfold $obs $vers 20000
  done
done


# mkdir -p unfold/mcmc/closure
# mkdir -p unfold/mcmc/closure_statonly
# mkdir -p unfold/mcmc/stress_herwig
# mkdir -p unfold/mcmc/data
# 
# 
# mkdir -p unfold/particlelevel/data/unfoldzblc
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/data/zblcmcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/data/unfoldzblc --xsecfile data/XSection-MC15-13TeV.data --observable zblc hist/*hist.gz > unfold/mcmc/data/zblc.log 2>&1
# 
# mkdir -p unfold/particlelevel/data/unfoldnsvtrk
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/data/nsvtrkmcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/data/unfoldnsvtrk --xsecfile data/XSection-MC15-13TeV.data --observable nsvtrk hist/*hist.gz > unfold/mcmc/data/nsvtrk.log 2>&1
# 
# 
# mkdir -p unfold/particlelevel/closure/unfoldzblc
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/closure/zblcmcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/closure/unfoldzblc --xsecfile data/XSection-MC15-13TeV.data --observable zblc --stresstest closure hist/{3,4}*hist.gz > unfold/mcmc/closure/zblc.log 2>&1
# 
# mkdir -p unfold/particlelevel/closure/unfoldnsvtrk
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/closure/nsvtrkmcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/closure/unfoldnsvtrk --xsecfile data/XSection-MC15-13TeV.data --observable nsvtrk --stresstest closure hist/{3,4}*hist.gz > unfold/mcmc/closure/nsvtrk.log 2>&1
# 
# 
# mkdir -p unfold/particlelevel/data/unfoldrho
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/data/rhomcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/data/unfoldrho --xsecfile data/XSection-MC15-13TeV.data --observable rho hist/*hist.gz > unfold/mcmc/data/rho.log 2>&1
# 
# mkdir -p unfold/particlelevel/closure/unfoldrho
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/closure/rhomcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/closure/unfoldrho --xsecfile data/XSection-MC15-13TeV.data --observable rho --stresstest closure hist/{3,4}*hist.gz > unfold/mcmc/closure/rho.log 2>&1
# 
# 
# 
# mkdir -p unfold/particlelevel/closure_statonly/unfoldzblc
# stack exec run-htop-unfold -- --stat-only --mcmcfile unfold/mcmc/closure_statonly/zblcmcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/closure_statonly/unfoldzblc --xsecfile data/XSection-MC15-13TeV.data --observable zblc --stresstest closure hist/{3,4}*hist.gz > unfold/mcmc/closure_statonly/zblc.log 2>&1
# 
# mkdir -p unfold/particlelevel/stress_herwig/unfoldzblc
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/stress_herwig/zblcmcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/stress_herwig/unfoldzblc --xsecfile data/XSection-MC15-13TeV.data --observable zblc --stresstest ps hist/{3,4}*hist.gz > unfold/mcmc/stress_herwig/zblc.log 2>&1
# 
# 
# mkdir -p unfold/particlelevel/closure_statonly/unfoldnsvtrk
# stack exec run-htop-unfold -- --stat-only --mcmcfile unfold/mcmc/closure_statonly/nsvtrkmcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/closure_statonly/unfoldnsvtrk --xsecfile data/XSection-MC15-13TeV.data --observable nsvtrk --stresstest closure hist/{3,4}*hist.gz > unfold/mcmc/closure_statonly/nsvtrk.log 2>&1
# 
# mkdir -p unfold/particlelevel/stress_herwig/unfoldnsvtrk
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/stress_herwig/nsvtrkmcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/stress_herwig/unfoldnsvtrk --xsecfile data/XSection-MC15-13TeV.data --observable nsvtrk --stresstest ps hist/{3,4}*hist.gz > unfold/mcmc/stress_herwig/nsvtrk.log 2>&1
# 
# 
# mkdir -p unfold/particlelevel/closure_statonly/unfoldrho
# stack exec run-htop-unfold -- --stat-only --mcmcfile unfold/mcmc/closure_statonly/rhomcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/closure_statonly/unfoldrho --xsecfile data/XSection-MC15-13TeV.data --observable rho --stresstest closure hist/{3,4}*hist.gz > unfold/mcmc/closure_statonly/rho.log 2>&1
# 
# mkdir -p unfold/particlelevel/stress_herwig/unfoldrho
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/stress_herwig/rhomcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/stress_herwig/unfoldrho --xsecfile data/XSection-MC15-13TeV.data --observable rho --stresstest ps hist/{3,4}*hist.gz > unfold/mcmc/stress_herwig/rho.log 2>&1
# 
# 
# mkdir -p unfold/particlelevel/data/unfoldzbtc
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/data/zbtcmcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/data/unfoldzbtc --xsecfile data/XSection-MC15-13TeV.data --observable zbtc hist/*hist.gz > unfold/mcmc/data/zbtc.log 2>&1
# 
# mkdir -p unfold/particlelevel/closure/unfoldzbtc
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/closure/zbtcmcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/closure/unfoldzbtc --xsecfile data/XSection-MC15-13TeV.data --observable zbtc --stresstest closure hist/{3,4}*hist.gz > unfold/mcmc/closure/zbtc.log 2>&1
# 
# mkdir -p unfold/particlelevel/closure_statonly/unfoldzbtc
# stack exec run-htop-unfold -- --stat-only --mcmcfile unfold/mcmc/closure_statonly/zbtcmcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/closure_statonly/unfoldzbtc --xsecfile data/XSection-MC15-13TeV.data --observable zbtc --stresstest closure hist/{3,4}*hist.gz > unfold/mcmc/closure_statonly/zbtc.log 2>&1
# 
# mkdir -p unfold/particlelevel/stress_herwig/unfoldzbtc
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/stress_herwig/zbtcmcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/stress_herwig/unfoldzbtc --xsecfile data/XSection-MC15-13TeV.data --observable zbtc --stresstest ps hist/{3,4}*hist.gz > unfold/mcmc/stress_herwig/zbtc.log 2>&1


# mkdir -p unfold/particlelevel/stress_mu/unfoldzblc
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/stress_mu/zblcmcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/stress_mu/unfoldzblc --xsecfile data/XSection-MC15-13TeV.data --observable zblc --stresstest puwgt hist/{3,4}*hist.gz > unfold/mcmc/stress_mu/zblc.log 2>&1

# mkdir -p unfold/particlelevel/stress_mugt22/unfoldzblc
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/stress_mugt22/zblcmcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/stress_mugt22/unfoldzblc --xsecfile data/XSection-MC15-13TeV.data --observable zblc --stresstest mugt22 hist/{3,4}*hist.gz > unfold/mcmc/stress_mugt22/zblc.log 2>&1


# mkdir -p unfold/particlelevel/stress_mu/unfoldzbtc
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/stress_mu/zbtcmcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/stress_mu/unfoldzbtc --xsecfile data/XSection-MC15-13TeV.data --observable zbtc --stresstest puwgt hist/{3,4}*hist.gz > unfold/mcmc/stress_mu/zbtc.log 2>&1


# mkdir -p unfold/particlelevel/stress_mu/unfoldnsvtrk
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/stress_mu/nsvtrkmcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/stress_mu/unfoldnsvtrk --xsecfile data/XSection-MC15-13TeV.data --observable nsvtrk --stresstest puwgt hist/{3,4}*hist.gz > unfold/mcmc/stress_mu/nsvtrk.log 2>&1


# mkdir -p unfold/particlelevel/closure/unfoldzbrelc
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/closure/zbrelcmcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/closure/unfoldzbrelc --xsecfile data/XSection-MC15-13TeV.data --observable zbrelc --stresstest closure hist/{3,4}*hist.gz > unfold/mcmc/closure/zbrelc.log 2>&1
# 
# mkdir -p unfold/particlelevel/closure_statonly/unfoldzbrelc
# stack exec run-htop-unfold -- --stat-only --mcmcfile unfold/mcmc/closure_statonly/zbrelcmcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/closure_statonly/unfoldzbrelc --xsecfile data/XSection-MC15-13TeV.data --observable zbrelc --stresstest closure hist/{3,4}*hist.gz > unfold/mcmc/closure_statonly/zbrelc.log 2>&1
# 
# mkdir -p unfold/particlelevel/stress_herwig/unfoldzbrelc
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/stress_herwig/zbrelcmcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/stress_herwig/unfoldzbrelc --xsecfile data/XSection-MC15-13TeV.data --observable zbrelc --stresstest ps hist/{3,4}*hist.gz > unfold/mcmc/stress_herwig/zbrelc.log 2>&1
# 
# mkdir -p unfold/particlelevel/stress_mu/unfoldzbrelc
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/stress_mu/zbrelcmcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/stress_mu/unfoldzbrelc --xsecfile data/XSection-MC15-13TeV.data --observable zbrelc --stresstest puwgt hist/{3,4}*hist.gz > unfold/mcmc/stress_mu/zbrelc.log 2>&1
# 
# mkdir -p unfold/particlelevel/data/unfoldzbrelc
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/data/zbrelcmcmc.dat --nsamples 20000 --yodafolder unfold/particlelevel/data/unfoldzbrelc --xsecfile data/XSection-MC15-13TeV.data --observable zbrelc hist/*hist.gz > unfold/mcmc/data/zbrelc.log 2>&1
