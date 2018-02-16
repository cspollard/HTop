mkdir -p mcmc/closure
mkdir -p mcmc/data

mkdir -p particlelevel/closure/unfoldzbtc
mkdir -p particlelevel/data/unfoldzbtc
stack exec run-htop-unfold -- --mcmcfile mcmc/data/zbtcmcmc.dat --yodafolder particlelevel/data/unfoldzbtc --xsecfile ext/XSection-MC15-13TeV.data --observable zbtc hist/*hist.gz > mcmc/data/zbtc.log 2>&1 &
stack exec run-htop-unfold -- --mcmcfile mcmc/closure/zbtcmcmc.dat --yodafolder particlelevel/closure/unfoldzbtc --xsecfile ext/XSection-MC15-13TeV.data --observable zbtc hist/{3,4}*hist.gz > mcmc/closure/zbtc.log 2>&1

mkdir -p particlelevel/closure/unfoldzblc
mkdir -p particlelevel/data/unfoldzblc
stack exec run-htop-unfold -- --mcmcfile mcmc/data/zblcmcmc.dat --yodafolder particlelevel/data/unfoldzblc --xsecfile ext/XSection-MC15-13TeV.data --observable zblc hist/*hist.gz > mcmc/data/zblc.log 2>&1
stack exec run-htop-unfold -- --mcmcfile mcmc/closure/zblcmcmc.dat --yodafolder particlelevel/closure/unfoldzblc --xsecfile ext/XSection-MC15-13TeV.data --observable zblc hist/{3,4}*hist.gz > mcmc/closure/zblc.log 2>&1 &

mkdir -p particlelevel/closure/unfoldzbtrelc
mkdir -p particlelevel/data/unfoldzbtrelc
stack exec run-htop-unfold -- --mcmcfile mcmc/data/zbtrelcmcmc.dat --yodafolder particlelevel/data/unfoldzbtrelc --xsecfile ext/XSection-MC15-13TeV.data --observable zbtrelc hist/*hist.gz > mcmc/data/zbtrelc.log 2>&1 &
stack exec run-htop-unfold -- --mcmcfile mcmc/closure/zbtrelcmcmc.dat --yodafolder particlelevel/closure/unfoldzbtrelc --xsecfile ext/XSection-MC15-13TeV.data --observable zbtrelc hist/{3,4}*hist.gz > mcmc/closure/zbtrelc.log 2>&1 &
