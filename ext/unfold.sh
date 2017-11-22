mkdir -p detectorlevel/unfoldzbtc
stack exec run-htop-unfold -- --mcmcfile detectorlevel/zbtcmcmc.dat --yodafolder detectorlevel/unfoldzbtc --xsecfile ../atlas.git/data/XSection-MC15-13TeV.data --observable zbtc hist/*hist.gz > detectorlevel/zbtc.log 2>&1
mkdir -p particlelevel/unfoldzbtc
stack exec run-htop-unfold -- --mcmcfile particlelevel/zbtcmcmc.dat --yodafolder particlelevel/unfoldzbtc --xsecfile ../atlas.git/data/XSection-MC15-13TeV.data --observable zbtc hist/{3,4}*hist.gz > particlelevel/zbtc.log 2>&1

mkdir -p detectorlevel/unfoldzblc
stack exec run-htop-unfold -- --mcmcfile detectorlevel/zblcmcmc.dat --yodafolder detectorlevel/unfoldzblc --xsecfile ../atlas.git/data/XSection-MC15-13TeV.data --observable zblc hist/*hist.gz > detectorlevel/zblc.log 2>&1
mkdir -p particlelevel/unfoldzblc
stack exec run-htop-unfold -- --mcmcfile particlelevel/zblcmcmc.dat --yodafolder particlelevel/unfoldzblc --xsecfile ../atlas.git/data/XSection-MC15-13TeV.data --observable zblc hist/{3,4}*hist.gz > particlelevel/zblc.log 2>&1

mkdir -p detectorlevel/unfoldzbtrelc
stack exec run-htop-unfold -- --mcmcfile detectorlevel/zbtrelcmcmc.dat --yodafolder detectorlevel/unfoldzbtrelc --xsecfile ../atlas.git/data/XSection-MC15-13TeV.data --observable zbtrelc hist/*hist.gz > detectorlevel/zbtrelc.log 2>&1
mkdir -p particlelevel/unfoldzbtrelc
stack exec run-htop-unfold -- --mcmcfile particlelevel/zbtrelcmcmc.dat --yodafolder particlelevel/unfoldzbtrelc --xsecfile ../atlas.git/data/XSection-MC15-13TeV.data --observable zbtrelc hist/{3,4}*hist.gz > particlelevel/zbtrelc.log 2>&1
