plotunfold() {
  yodamerge -o htop.yoda unfold/particlelevel/$1/unfold*/htop.yoda

  rivet-mkhtml --mc-errs --pwd \
    -m "/htop/elmujjtrue/truejets/(rho|zbtc|zblc|zbrelc|nsvtrk)(|norm)$" \
    -c ext/htop.plot \
    yoda/PowPy8FS.yoda:"PowPy8" \
    yoda/PowH7AFII.yoda:"PowH7" \
    yoda/aMCPy8AFII.yoda:"aMCPy8" \
    yoda/Sherpa221AFII.yoda:"Sherpa221" \
    -o unfold/particlelevel/$1/genplots

  rivet-mkhtml --mc-errs --pwd \
    -m "/htop/elmujjtrue/truejets/(rho|zbtc|zblc|zbrelc|nsvtrk)(|norm)$" \
    -c ext/htop.plot \
    yoda/PowPy8FS.yoda:"PowPy8" \
    yoda/PowPy8FSRUpAFII.yoda:"PowPy8FSRUp" \
    yoda/PowPy8FSRDownAFII.yoda:"PowPy8FSRDown" \
    yoda/PowPy6FS.yoda:"PowPy6" \
    -o unfold/particlelevel/$1/powpygenplots


  rivet-mkhtml --mc-errs --pwd \
    -m "/htop/elmujjtrue/truejets/(rho|zbtc|zblc|zbrelc|nsvtrk)(|norm)$" \
    -c ext/htop.plot \
    yoda/PowPy8FS.yoda:"PowPy8" \
    yoda/PowPy8RadUpAFII.yoda:"PowPy8ISRUp" \
    yoda/PowPy8RadDownAFII.yoda:"PowPy8ISRDown" \
    -o unfold/particlelevel/$1/powpyisrgenplots

  perl -p -i -e "s/\/REF\/htop\/elmujj\//\/htop\/elmujj\//g" htop.yoda

  if [ -e yoda/$1.yoda ]
  then
    mkdir -p detectorlevel/recoclosure/$1
    rivet-mkhtml --mc-errs \
      -m "/htop/elmujj/probejets/(rho|zbtc|zblc|zbrelc|nsvtrk)$" \
      -c ext/htop.plot \
      yoda/total.yoda:"Title=nominal prediction":"LineColor=Black":"ErrorBandColor={[cmyk]{0,0,0,0.15}}":"ErrorBands=1":"ErrorBars=0" \
      yoda/background.yoda:"backgrounds":"LineColor=Black":"ErrorBars=0":"LineStyle=dotted" \
      htop.yoda:"Title=posterior prediction":"LineColor=green":"ErrorBandColor={[cmyk]{1,0,1,0.10}}":"ErrorBands=1":"ErrorBars=0" \
      yoda/$1.yoda:"Title=data":"LineColor=black":"ConnectBins=0" \
      -o detectorlevel/recoclosure/$1
  fi

  rm -f htop.yoda

}


plotunfold_closure() {
  yodamerge -o htop.yoda unfold/particlelevel/$1/unfold*/htop.yoda

  if [[ $1 == "ps" ]]
  then 
    rivet-mkhtml --mc-errs --pwd \
      -m "/htop/elmujjtrue/truejets/(rho|zbtc|zblc|zbrelc|nsvtrk)(|norm)$" \
      -c ext/htop.plot \
      yoda/PowPy8FS.yoda:"PowPy8" \
      yoda/PowH7AFII.yoda:"PowH7" \
      -o unfold/particlelevel/$1/genplots
  else
    rivet-mkhtml --mc-errs --pwd \
      -m "/htop/elmujjtrue/truejets/(rho|zbtc|zblc|zbrelc|nsvtrk)(|norm)$" \
      -c ext/htop.plot \
      yoda/PowPy8FS.yoda:"PowPy8" \
      -o unfold/particlelevel/$1/genplots
  fi

  perl -p -i -e "s/\/REF\/htop\/elmujj\//\/htop\/elmujj\//g" htop.yoda

  if [ -e yoda/$1.yoda ]
  then
    mkdir -p detectorlevel/recoclosure/$1
    rivet-mkhtml --mc-errs \
      -m "/htop/elmujj/probejets/(rho|zbtc|zblc|zbrelc|nsvtrk)$" \
      -c ext/htop.plot \
      yoda/total.yoda:"Title=nominal prediction":"LineColor=Black":"ErrorBandColor={[cmyk]{0,0,0,0.15}}":"ErrorBands=1":"ErrorBars=0" \
      yoda/background.yoda:"backgrounds":"LineColor=Black":"ErrorBars=0":"LineStyle=dotted" \
      htop.yoda:"Title=posterior prediction":"LineColor=green":"ErrorBandColor={[cmyk]{1,0,1,0.10}}":"ErrorBands=1":"ErrorBars=0" \
      yoda/$1.yoda:"Title=data":"LineColor=black":"ConnectBins=0" \
      -o detectorlevel/recoclosure/$1
  fi

  rm -f htop.yoda

}


plotunfold data

for vers in closure closure_statonly ps mule22 mugt22
do
  plotunfold_closure $vers
done
