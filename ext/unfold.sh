mkdir -p mcmc/closure
mkdir -p mcmc/data

mkdir -p particlelevel/closure/unfoldzbtc
mkdir -p particlelevel/data/unfoldzbtc
stack exec run-htop-unfold -- --mcmcfile mcmc/data/zbtcmcmc.dat --yodafolder particlelevel/data/unfoldzbtc --xsecfile ../atlas.git/data/XSection-MC15-13TeV.data --observable zbtc hist/*hist.gz > detectorlevel/zbtc.log 2>&1
stack exec run-htop-unfold -- --mcmcfile mcmc/closure/zbtcmcmc.dat --yodafolder particlelevel/closure/unfoldzbtc --xsecfile ../atlas.git/data/XSection-MC15-13TeV.data --observable zbtc hist/{3,4}*hist.gz > particlelevel/zbtc.log 2>&1

mkdir -p particlelevel/closure/unfoldzblc
mkdir -p particlelevel/data/unfoldzblc
stack exec run-htop-unfold -- --mcmcfile mcmc/data/zblcmcmc.dat --yodafolder particlelevel/data/unfoldzblc --xsecfile ../atlas.git/data/XSection-MC15-13TeV.data --observable zblc hist/*hist.gz > detectorlevel/zblc.log 2>&1
stack exec run-htop-unfold -- --mcmcfile mcmc/closure/zblcmcmc.dat --yodafolder particlelevel/closure/unfoldzblc --xsecfile ../atlas.git/data/XSection-MC15-13TeV.data --observable zblc hist/{3,4}*hist.gz > particlelevel/zblc.log 2>&1

mkdir -p particlelevel/closure/unfoldzbtrelc
mkdir -p particlelevel/data/unfoldzbtrelc
stack exec run-htop-unfold -- --mcmcfile mcmc/data/zbtrelcmcmc.dat --yodafolder particlelevel/data/unfoldzbtrelc --xsecfile ../atlas.git/data/XSection-MC15-13TeV.data --observable zbtrelc hist/*hist.gz > detectorlevel/zbtrelc.log 2>&1
stack exec run-htop-unfold -- --mcmcfile mcmc/closure/zbtrelcmcmc.dat --yodafolder particlelevel/closure/unfoldzbtrelc --xsecfile ../atlas.git/data/XSection-MC15-13TeV.data --observable zbtrelc hist/{3,4}*hist.gz > particlelevel/zbtrelc.log 2>&1
