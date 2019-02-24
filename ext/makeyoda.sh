mkdir -p yoda
stack exec run-htop-toyoda -- \
  -o yoda \
  --xsecfile data/XSection-MC15-13TeV.data hist/*hist.gz
