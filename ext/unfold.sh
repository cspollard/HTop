mkdir -p realdata/unfoldyoda
stack exec run-htop-unfold -- ../atlas.git/data/XSection-MC15-13TeV.data realdata/data.dat realdata/unfoldyoda hist/*hist.gz > realdata/log 2>&1 &
mkdir -p closuredata/unfoldyoda
stack exec run-htop-unfold -- ../atlas.git/data/XSection-MC15-13TeV.data closuredata/data.dat closuredata/unfoldyoda hist/{3,4}*hist.gz > closuredata/log 2>&1
