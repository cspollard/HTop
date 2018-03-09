mkdir -p unfold/mcmc/closure
mkdir -p unfold/mcmc/data

mkdir -p unfold/particlelevel/closure/unfoldzbtc
mkdir -p unfold/particlelevel/data/unfoldzbtc
stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/data/zbtcmcmc.dat --nsamples 100000 --yodafolder unfold/particlelevel/data/unfoldzbtc --xsecfile ext/XSection-MC15-13TeV.data --observable zbtc hist/*hist.gz > unfold/mcmc/data/zbtc.log 2>&1 &
stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/closure/zbtcmcmc.dat --nsamples 100000 --yodafolder unfold/particlelevel/closure/unfoldzbtc --xsecfile ext/XSection-MC15-13TeV.data --observable zbtc hist/{3,4}*hist.gz > unfold/mcmc/closure/zbtc.log 2>&1

mkdir -p unfold/particlelevel/closure/unfoldzbt
mkdir -p unfold/particlelevel/data/unfoldzbt
stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/data/zbtmcmc.dat --nsamples 100000 --yodafolder unfold/particlelevel/data/unfoldzbt --xsecfile ext/XSection-MC15-13TeV.data --observable zbt hist/*hist.gz > unfold/mcmc/data/zbt.log 2>&1 &
stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/closure/zbtmcmc.dat --nsamples 100000 --yodafolder unfold/particlelevel/closure/unfoldzbt --xsecfile ext/XSection-MC15-13TeV.data --observable zbt hist/{3,4}*hist.gz > unfold/mcmc/closure/zbt.log 2>&1

mkdir -p unfold/particlelevel/closure/unfoldzbl
mkdir -p unfold/particlelevel/data/unfoldzbl
stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/data/zblmcmc.dat --nsamples 100000 --yodafolder unfold/particlelevel/data/unfoldzbl --xsecfile ext/XSection-MC15-13TeV.data --observable zbl hist/*hist.gz > unfold/mcmc/data/zbl.log 2>&1 &
stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/closure/zblmcmc.dat --nsamples 100000 --yodafolder unfold/particlelevel/closure/unfoldzbl --xsecfile ext/XSection-MC15-13TeV.data --observable zbl hist/{3,4}*hist.gz > unfold/mcmc/closure/zbl.log 2>&1

mkdir -p unfold/particlelevel/closure/unfoldzblc
mkdir -p unfold/particlelevel/data/unfoldzblc
stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/data/zblcmcmc.dat --nsamples 100000 --yodafolder unfold/particlelevel/data/unfoldzblc --xsecfile ext/XSection-MC15-13TeV.data --observable zblc hist/*hist.gz > unfold/mcmc/data/zblc.log 2>&1 &
stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/closure/zblcmcmc.dat --nsamples 100000 --yodafolder unfold/particlelevel/closure/unfoldzblc --xsecfile ext/XSection-MC15-13TeV.data --observable zblc hist/{3,4}*hist.gz > unfold/mcmc/closure/zblc.log 2>&1

mkdir -p unfold/particlelevel/closure/unfoldzbtrelc
mkdir -p unfold/particlelevel/data/unfoldzbtrelc
stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/data/zbtrelcmcmc.dat --nsamples 100000 --yodafolder unfold/particlelevel/data/unfoldzbtrelc --xsecfile ext/XSection-MC15-13TeV.data --observable zbtrelc hist/*hist.gz > unfold/mcmc/data/zbtrelc.log 2>&1 &
stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/closure/zbtrelcmcmc.dat --nsamples 100000 --yodafolder unfold/particlelevel/closure/unfoldzbtrelc --xsecfile ext/XSection-MC15-13TeV.data --observable zbtrelc hist/{3,4}*hist.gz > unfold/mcmc/closure/zbtrelc.log 2>&1

mkdir -p unfold/particlelevel/closure/unfoldzbtrel
mkdir -p unfold/particlelevel/data/unfoldzbtrel
stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/data/zbtrelmcmc.dat --nsamples 100000 --yodafolder unfold/particlelevel/data/unfoldzbtrel --xsecfile ext/XSection-MC15-13TeV.data --observable zbtrel hist/*hist.gz > unfold/mcmc/data/zbtrel.log 2>&1 &
stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/closure/zbtrelmcmc.dat --nsamples 100000 --yodafolder unfold/particlelevel/closure/unfoldzbtrel --xsecfile ext/XSection-MC15-13TeV.data --observable zbtrel hist/{3,4}*hist.gz > unfold/mcmc/closure/zbtrel.log 2>&1
