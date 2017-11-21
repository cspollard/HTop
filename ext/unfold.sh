mkdir -p realdata/unfoldzbtc
stack exec run-htop-unfold -- --mcmcfile realdata/zbtcmcmc.dat --yodafolder realdata/unfoldzbtc --xsecfile ../atlas.git/data/XSection-MC15-13TeV.data --observable zbtc hist/*hist.gz > realdata/zbtc.log 2>&1 &
mkdir -p closuredata/unfoldzbtc
stack exec run-htop-unfold -- --mcmcfile closuredata/zbtcmcmc.dat --yodafolder closuredata/unfoldzbtc --xsecfile ../atlas.git/data/XSection-MC15-13TeV.data --observable zbtc hist/{3,4}*hist.gz > closuredata/zbtc.log 2>&1 &

mkdir -p realdata/unfoldzblc
stack exec run-htop-unfold -- --mcmcfile realdata/zblcmcmc.dat --yodafolder realdata/unfoldzblc --xsecfile ../atlas.git/data/XSection-MC15-13TeV.data --observable zblc hist/*hist.gz > realdata/zblc.log 2>&1 &
mkdir -p closuredata/unfoldzblc
stack exec run-htop-unfold -- --mcmcfile closuredata/zblcmcmc.dat --yodafolder closuredata/unfoldzblc --xsecfile ../atlas.git/data/XSection-MC15-13TeV.data --observable zblc hist/{3,4}*hist.gz > closuredata/zblc.log 2>&1 &

mkdir -p realdata/unfoldzbtrelc
stack exec run-htop-unfold -- --mcmcfile realdata/zbtrelcmcmc.dat --yodafolder realdata/unfoldzbtrelc --xsecfile ../atlas.git/data/XSection-MC15-13TeV.data --observable zbtrelc hist/*hist.gz > realdata/zbtrelc.log 2>&1 &
mkdir -p closuredata/unfoldzbtrelc
stack exec run-htop-unfold -- --mcmcfile closuredata/zbtrelcmcmc.dat --yodafolder closuredata/unfoldzbtrelc --xsecfile ../atlas.git/data/XSection-MC15-13TeV.data --observable zbtrelc hist/{3,4}*hist.gz > closuredata/zbtrelc.log 2>&1 &
