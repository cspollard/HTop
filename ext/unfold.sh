mkdir -p unfold/mcmc/closure
mkdir -p unfold/mcmc/closure_statonly
mkdir -p unfold/mcmc/data

mkdir -p unfold/particlelevel/closure/unfoldzbtc
mkdir -p unfold/particlelevel/closure_statonly/unfoldzbtc
mkdir -p unfold/particlelevel/data/unfoldzbtc
stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/data/zbtcmcmc.dat --nsamples 500000 --yodafolder unfold/particlelevel/data/unfoldzbtc --xsecfile data/XSection-MC15-13TeV.data --observable zbtc hist/*hist.gz > unfold/mcmc/data/zbtc.log 2>&1
stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/closure/zbtcmcmc.dat --nsamples 500000 --yodafolder unfold/particlelevel/closure/unfoldzbtc --xsecfile data/XSection-MC15-13TeV.data --observable zbtc hist/{3,4}*hist.gz > unfold/mcmc/closure/zbtc.log 2>&1
stack exec run-htop-unfold -- --stat-only --mcmcfile unfold/mcmc/closure_statonly/zbtcmcmc.dat --nsamples 500000 --yodafolder unfold/particlelevel/closure_statonly/unfoldzbtc --xsecfile data/XSection-MC15-13TeV.data --observable zbtc hist/{3,4}*hist.gz > unfold/mcmc/closure_statonly/zbtc.log 2>&1

# mkdir -p unfold/particlelevel/closure/unfoldzbt
# mkdir -p unfold/particlelevel/closure_statonly/unfoldzbt
# mkdir -p unfold/particlelevel/data/unfoldzbt
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/data/zbtmcmc.dat --nsamples 500000 --yodafolder unfold/particlelevel/data/unfoldzbt --xsecfile data/XSection-MC15-13TeV.data --observable zbt hist/*hist.gz > unfold/mcmc/data/zbt.log 2>&1
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/closure/zbtmcmc.dat --nsamples 500000 --yodafolder unfold/particlelevel/closure/unfoldzbt --xsecfile data/XSection-MC15-13TeV.data --observable zbt hist/{3,4}*hist.gz > unfold/mcmc/closure/zbt.log 2>&1
# stack exec run-htop-unfold -- --stat-only --mcmcfile unfold/mcmc/closure_statonly/zbtmcmc.dat --nsamples 500000 --yodafolder unfold/particlelevel/closure_statonly/unfoldzbt --xsecfile data/XSection-MC15-13TeV.data --observable zbt hist/{3,4}*hist.gz > unfold/mcmc/closure_statonly/zbt.log 2>&1

mkdir -p unfold/particlelevel/closure/unfoldzblc
mkdir -p unfold/particlelevel/closure_statonly/unfoldzblc
mkdir -p unfold/particlelevel/data/unfoldzblc
stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/data/zblcmcmc.dat --nsamples 500000 --yodafolder unfold/particlelevel/data/unfoldzblc --xsecfile data/XSection-MC15-13TeV.data --observable zblc hist/*hist.gz > unfold/mcmc/data/zblc.log 2>&1
stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/closure/zblcmcmc.dat --nsamples 500000 --yodafolder unfold/particlelevel/closure/unfoldzblc --xsecfile data/XSection-MC15-13TeV.data --observable zblc hist/{3,4}*hist.gz > unfold/mcmc/closure/zblc.log 2>&1
stack exec run-htop-unfold -- --stat-only --mcmcfile unfold/mcmc/closure_statonly/zblcmcmc.dat --nsamples 500000 --yodafolder unfold/particlelevel/closure_statonly/unfoldzblc --xsecfile data/XSection-MC15-13TeV.data --observable zblc hist/{3,4}*hist.gz > unfold/mcmc/closure_statonly/zblc.log 2>&1

# mkdir -p unfold/particlelevel/closure/unfoldzbl
# mkdir -p unfold/particlelevel/closure_statonly/unfoldzbl
# mkdir -p unfold/particlelevel/data/unfoldzbl
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/data/zblmcmc.dat --nsamples 500000 --yodafolder unfold/particlelevel/data/unfoldzbl --xsecfile data/XSection-MC15-13TeV.data --observable zbl hist/*hist.gz > unfold/mcmc/data/zbl.log 2>&1
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/closure/zblmcmc.dat --nsamples 500000 --yodafolder unfold/particlelevel/closure/unfoldzbl --xsecfile data/XSection-MC15-13TeV.data --observable zbl hist/{3,4}*hist.gz > unfold/mcmc/closure/zbl.log 2>&1
# stack exec run-htop-unfold -- --stat-only --mcmcfile unfold/mcmc/closure_statonly/zblmcmc.dat --nsamples 500000 --yodafolder unfold/particlelevel/closure_statonly/unfoldzbl --xsecfile data/XSection-MC15-13TeV.data --observable zbl hist/{3,4}*hist.gz > unfold/mcmc/closure_statonly/zbl.log 2>&1

mkdir -p unfold/particlelevel/closure/unfoldzbrelc
mkdir -p unfold/particlelevel/closure_statonly/unfoldzbrelc
mkdir -p unfold/particlelevel/data/unfoldzbrelc
stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/data/zbrelcmcmc.dat --nsamples 500000 --yodafolder unfold/particlelevel/data/unfoldzbrelc --xsecfile data/XSection-MC15-13TeV.data --observable zbrelc hist/*hist.gz > unfold/mcmc/data/zbrelc.log 2>&1
stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/closure/zbrelcmcmc.dat --nsamples 500000 --yodafolder unfold/particlelevel/closure/unfoldzbrelc --xsecfile data/XSection-MC15-13TeV.data --observable zbrelc hist/{3,4}*hist.gz > unfold/mcmc/closure/zbrelc.log 2>&1
stack exec run-htop-unfold -- --stat-only --mcmcfile unfold/mcmc/closure_statonly/zbrelcmcmc.dat --nsamples 500000 --yodafolder unfold/particlelevel/closure_statonly/unfoldzbrelc --xsecfile data/XSection-MC15-13TeV.data --observable zbrelc hist/{3,4}*hist.gz > unfold/mcmc/closure_statonly/zbrelc.log 2>&1

# mkdir -p unfold/particlelevel/closure/unfoldzbrel
# mkdir -p unfold/particlelevel/closure_statonly/unfoldzbrel
# mkdir -p unfold/particlelevel/data/unfoldzbrel
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/data/zbrelmcmc.dat --nsamples 500000 --yodafolder unfold/particlelevel/data/unfoldzbrel --xsecfile data/XSection-MC15-13TeV.data --observable zbrel hist/*hist.gz > unfold/mcmc/data/zbrel.log 2>&1
# stack exec run-htop-unfold -- --mcmcfile unfold/mcmc/closure/zbrelmcmc.dat --nsamples 500000 --yodafolder unfold/particlelevel/closure/unfoldzbrel --xsecfile data/XSection-MC15-13TeV.data --observable zbrel hist/{3,4}*hist.gz > unfold/mcmc/closure/zbrel.log 2>&1
# stack exec run-htop-unfold -- --stat-only --mcmcfile unfold/mcmc/closure_statonly/zbrelmcmc.dat --nsamples 500000 --yodafolder unfold/particlelevel/closure_statonly/unfoldzbrel --xsecfile data/XSection-MC15-13TeV.data --observable zbrel hist/{3,4}*hist.gz > unfold/mcmc/closure_statonly/zbrel.log 2>&1
