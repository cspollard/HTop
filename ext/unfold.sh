mkdir -p mcmc/closure
mkdir -p mcmc/data

mkdir -p particlelevel/closure/unfoldzbtc
mkdir -p particlelevel/data/unfoldzbtc
stack exec run-htop-unfold -- --mcmcfile mcmc/data/zbtcmcmc.dat --nsamples 1000000 --yodafolder particlelevel/data/unfoldzbtc --xsecfile ext/XSection-MC15-13TeV.data --observable zbtc hist/*hist.gz > mcmc/data/zbtc.log 2>&1 &
stack exec run-htop-unfold -- --mcmcfile mcmc/closure/zbtcmcmc.dat --nsamples 1000000 --yodafolder particlelevel/closure/unfoldzbtc --xsecfile ext/XSection-MC15-13TeV.data --observable zbtc hist/{3,4}*hist.gz > mcmc/closure/zbtc.log 2>&1

mkdir -p particlelevel/closure/unfoldzbt
mkdir -p particlelevel/data/unfoldzbt
stack exec run-htop-unfold -- --mcmcfile mcmc/data/zbtmcmc.dat --nsamples 1000000 --yodafolder particlelevel/data/unfoldzbt --xsecfile ext/XSection-MC15-13TeV.data --observable zbt hist/*hist.gz > mcmc/data/zbt.log 2>&1 &
stack exec run-htop-unfold -- --mcmcfile mcmc/closure/zbtmcmc.dat --nsamples 1000000 --yodafolder particlelevel/closure/unfoldzbt --xsecfile ext/XSection-MC15-13TeV.data --observable zbt hist/{3,4}*hist.gz > mcmc/closure/zbt.log 2>&1

mkdir -p particlelevel/closure/unfoldzbl
mkdir -p particlelevel/data/unfoldzbl
stack exec run-htop-unfold -- --mcmcfile mcmc/data/zblmcmc.dat --nsamples 1000000 --yodafolder particlelevel/data/unfoldzbl --xsecfile ext/XSection-MC15-13TeV.data --observable zbl hist/*hist.gz > mcmc/data/zbl.log 2>&1 &
stack exec run-htop-unfold -- --mcmcfile mcmc/closure/zblmcmc.dat --nsamples 1000000 --yodafolder particlelevel/closure/unfoldzbl --xsecfile ext/XSection-MC15-13TeV.data --observable zbl hist/{3,4}*hist.gz > mcmc/closure/zbl.log 2>&1

mkdir -p particlelevel/closure/unfoldzblc
mkdir -p particlelevel/data/unfoldzblc
stack exec run-htop-unfold -- --mcmcfile mcmc/data/zblcmcmc.dat --nsamples 1000000 --yodafolder particlelevel/data/unfoldzblc --xsecfile ext/XSection-MC15-13TeV.data --observable zblc hist/*hist.gz > mcmc/data/zblc.log 2>&1 &
stack exec run-htop-unfold -- --mcmcfile mcmc/closure/zblcmcmc.dat --nsamples 1000000 --yodafolder particlelevel/closure/unfoldzblc --xsecfile ext/XSection-MC15-13TeV.data --observable zblc hist/{3,4}*hist.gz > mcmc/closure/zblc.log 2>&1

mkdir -p particlelevel/closure/unfoldzbtrelc
mkdir -p particlelevel/data/unfoldzbtrelc
stack exec run-htop-unfold -- --mcmcfile mcmc/data/zbtrelcmcmc.dat --nsamples 1000000 --yodafolder particlelevel/data/unfoldzbtrelc --xsecfile ext/XSection-MC15-13TeV.data --observable zbtrelc hist/*hist.gz > mcmc/data/zbtrelc.log 2>&1 &
stack exec run-htop-unfold -- --mcmcfile mcmc/closure/zbtrelcmcmc.dat --nsamples 1000000 --yodafolder particlelevel/closure/unfoldzbtrelc --xsecfile ext/XSection-MC15-13TeV.data --observable zbtrelc hist/{3,4}*hist.gz > mcmc/closure/zbtrelc.log 2>&1

mkdir -p particlelevel/closure/unfoldzbtrel
mkdir -p particlelevel/data/unfoldzbtrel
stack exec run-htop-unfold -- --mcmcfile mcmc/data/zbtrelmcmc.dat --nsamples 1000000 --yodafolder particlelevel/data/unfoldzbtrel --xsecfile ext/XSection-MC15-13TeV.data --observable zbtrel hist/*hist.gz > mcmc/data/zbtrel.log 2>&1 &
stack exec run-htop-unfold -- --mcmcfile mcmc/closure/zbtrelmcmc.dat --nsamples 1000000 --yodafolder particlelevel/closure/unfoldzbtrel --xsecfile ext/XSection-MC15-13TeV.data --observable zbtrel hist/{3,4}*hist.gz > mcmc/closure/zbtrel.log 2>&1
