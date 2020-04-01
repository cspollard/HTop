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


for vers in data closure closure_statonly ps mule22 mugt22
do
  plotunfold $vers
done

# yodamerge -o htop.yoda unfold/particlelevel/data/unfold*/htop.yoda
# 
# rivet-mkhtml --mc-errs --pwd -m "/htop/elmujjtrue/truejets/(rho|zbtc|zblc|zbrelc|nsvtrk)(|norm)$" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowH7AFII.yoda:"PowH7" yoda/aMCPy8AFII.yoda:"aMCPy8" yoda/Sherpa221AFII.yoda:"Sherpa221" -o unfold/particlelevel/data/genplots
# rivet-mkhtml --mc-errs --pwd -m "/htop/elmujjtrue/truejets/(rho|zbtc|zblc|zbrelc|nsvtrk)(|norm)$" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowPy8FSRUpAFII.yoda:"PowPy8FSRUp" yoda/PowPy8FSRDownAFII.yoda:"PowPy8FSRDown" yoda/PowPy6FS.yoda:"PowPy6" -o unfold/particlelevel/data/powpygenplots
# 
# perl -p -i -e "s/\/REF\/htop\/elmujj\//\/htop\/elmujj\//g" htop.yoda
# rivet-mkhtml -m "/htop/elmujj/probejets/(rho|zbtc|zblc|zbrelc|nsvtrk)$" --mc-errs -c ext/htop.plot yoda/nominal.yoda yoda/background.yoda:"backgrounds" htop.yoda:"Title=posterior":"LineColor=green" yoda/data.yoda:"LineColor=black" -o detectorlevel/recoclosure/data
# 
# rm -f htop.yoda
# 
# 
# yodamerge -o htop.yoda unfold/particlelevel/closure_statonly/unfold*/htop.yoda
# 
# rivet-mkhtml --mc-errs --pwd -m "/htop/elmujjtrue/truejets/(rho|zbtc|zblc|zbrelc|nsvtrk)(|norm)$" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowH7AFII.yoda:"PowH7" yoda/aMCPy8AFII.yoda:"aMCPy8" yoda/Sherpa221AFII.yoda:"Sherpa221" -o unfold/particlelevel/closure_statonly/genplots
# rivet-mkhtml --mc-errs --pwd -m "/htop/elmujjtrue/truejets/(rho|zbtc|zblc|zbrelc|nsvtrk)(|norm)$" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowPy8FSRUpAFII.yoda:"PowPy8FSRUp" yoda/PowPy8FSRDownAFII.yoda:"PowPy8FSRDown" yoda/PowPy6FS.yoda:"PowPy6" -o unfold/particlelevel/closure_statonly/powpygenplots
# 
# perl -p -i -e "s/\/REF\/htop\/elmujj\//\/htop\/elmujj\//g" htop.yoda
# rivet-mkhtml -m "/htop/elmujj/probejets/(rho|zbtc|zblc|zbrelc|nsvtrk)$" --mc-errs -c ext/htop.plot yoda/nominal.yoda yoda/background.yoda:"backgrounds" htop.yoda:"Title=posterior":"LineColor=green" yoda/nominal.yoda:"LineColor=black" -o detectorlevel/recoclosure/closure_statonly
# 
# rm -f htop.yoda
# 
# yodamerge -o htop.yoda unfold/particlelevel/stress_mugt22/unfold*/htop.yoda
# 
# rivet-mkhtml --mc-errs --pwd -m "/htop/elmujjtrue/truejets/(rho|zbtc|zblc|zbrelc|nsvtrk)(|norm)$" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowH7AFII.yoda:"PowH7" yoda/aMCPy8AFII.yoda:"aMCPy8" yoda/Sherpa221AFII.yoda:"Sherpa221" -o unfold/particlelevel/stress_mugt22/genplots
# rivet-mkhtml --mc-errs --pwd -m "/htop/elmujjtrue/truejets/(rho|zbtc|zblc|zbrelc|nsvtrk)(|norm)$" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowPy8FSRUpAFII.yoda:"PowPy8FSRUp" yoda/PowPy8FSRDownAFII.yoda:"PowPy8FSRDown" yoda/PowPy6FS.yoda:"PowPy6" -o unfold/particlelevel/stress_mugt22/powpygenplots
# 
# perl -p -i -e "s/\/REF\/htop\/elmujj\//\/htop\/elmujj\//g" htop.yoda
# rivet-mkhtml -m "/htop/elmujj/probejets/(rho|zbtc|zblc|zbrelc|nsvtrk)$" --mc-errs -c ext/htop.plot yoda/nominal.yoda yoda/background.yoda:"backgrounds" htop.yoda:"Title=posterior":"LineColor=green" yoda/ps.yoda:"Title=PowH7":"LineColor=black" -o detectorlevel/recoclosure/stress_mugt22
# 
# rm -f htop.yoda
# 
# yodamerge -o htop.yoda unfold/particlelevel/stress_mule22/unfold*/htop.yoda
# 
# rivet-mkhtml --mc-errs --pwd -m "/htop/elmujjtrue/truejets/(rho|zbtc|zblc|zbrelc|nsvtrk)(|norm)$" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowH7AFII.yoda:"PowH7" yoda/aMCPy8AFII.yoda:"aMCPy8" yoda/Sherpa221AFII.yoda:"Sherpa221" -o unfold/particlelevel/stress_mule22/genplots
# rivet-mkhtml --mc-errs --pwd -m "/htop/elmujjtrue/truejets/(rho|zbtc|zblc|zbrelc|nsvtrk)(|norm)$" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowPy8FSRUpAFII.yoda:"PowPy8FSRUp" yoda/PowPy8FSRDownAFII.yoda:"PowPy8FSRDown" yoda/PowPy6FS.yoda:"PowPy6" -o unfold/particlelevel/stress_mule22/powpygenplots
# 
# perl -p -i -e "s/\/REF\/htop\/elmujj\//\/htop\/elmujj\//g" htop.yoda
# rivet-mkhtml -m "/htop/elmujj/probejets/(rho|zbtc|zblc|zbrelc|nsvtrk)$" --mc-errs -c ext/htop.plot yoda/nominal.yoda yoda/background.yoda:"backgrounds" htop.yoda:"Title=posterior":"LineColor=green" yoda/ps.yoda:"Title=PowH7":"LineColor=black" -o detectorlevel/recoclosure/stress_mule22
# 
# rm -f htop.yoda
# 
# yodamerge -o htop.yoda unfold/particlelevel/stress_herwig/unfold*/htop.yoda
# 
# rivet-mkhtml --mc-errs --pwd -m "/htop/elmujjtrue/truejets/(rho|zbtc|zblc|zbrelc|nsvtrk)(|norm)$" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowH7AFII.yoda:"PowH7" yoda/aMCPy8AFII.yoda:"aMCPy8" yoda/Sherpa221AFII.yoda:"Sherpa221" -o unfold/particlelevel/stress_herwig/genplots
# rivet-mkhtml --mc-errs --pwd -m "/htop/elmujjtrue/truejets/(rho|zbtc|zblc|zbrelc|nsvtrk)(|norm)$" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowPy8FSRUpAFII.yoda:"PowPy8FSRUp" yoda/PowPy8FSRDownAFII.yoda:"PowPy8FSRDown" yoda/PowPy6FS.yoda:"PowPy6" -o unfold/particlelevel/stress_herwig/powpygenplots
# 
# perl -p -i -e "s/\/REF\/htop\/elmujj\//\/htop\/elmujj\//g" htop.yoda
# rivet-mkhtml -m "/htop/elmujj/probejets/(rho|zbtc|zblc|zbrelc|nsvtrk)$" --mc-errs -c ext/htop.plot yoda/nominal.yoda yoda/background.yoda:"backgrounds" htop.yoda:"Title=posterior":"LineColor=green" yoda/ps.yoda:"Title=PowH7":"LineColor=black" -o detectorlevel/recoclosure/stress_herwig
# 
# rm -f htop.yoda
# 
# rivet-mkhtml --mc-errs --pwd -m "/htop/elmujjtrue/truejets/(rho|zbtc|zblc|zbrelc|nsvtrk)(|norm)$" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowH7AFII.yoda:"PowH7" yoda/aMCPy8AFII.yoda:"aMCPy8" yoda/Sherpa221AFII.yoda:"Sherpa221" -o unfold/particlelevel/genplotsnodata
# rivet-mkhtml --mc-errs --pwd -m "/htop/elmujjtrue/truejets/(rho|zbtc|zblc|zbrelc|nsvtrk)(|norm)$" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowPy8FSRUpAFII.yoda:"PowPy8FSRUp" yoda/PowPy8FSRDownAFII.yoda:"PowPy8FSRDown" yoda/PowPy6FS.yoda:"PowPy6" -o unfold/particlelevel/powpygenplotsnodata
