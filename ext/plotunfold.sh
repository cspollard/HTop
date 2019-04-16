mkdir -p detectorlevel/recoclosure/

yodamerge -o htop.yoda unfold/particlelevel/data/unfold*/htop.yoda

rivet-mkhtml --mc-errs --pwd -m "/htop/elmujjtrue/truejets/(zbtc|zblc|zbrelc)(|norm)$" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowH7AFII.yoda:"PowH7" yoda/aMCPy8AFII.yoda:"aMCPy8" yoda/Sherpa221AFII.yoda:"Sherpa221" -o unfold/particlelevel/data/genplots
rivet-mkhtml --mc-errs --pwd -m "/htop/elmujjtrue/truejets/(zbtc|zblc|zbrelc)(|norm)$" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowPy8FSRUpAFII.yoda:"PowPy8FSRUp" yoda/PowPy8FSRDownAFII.yoda:"PowPy8FSRDown" yoda/PowPy6FS.yoda:"PowPy6" -o unfold/particlelevel/data/powpygenplots

perl -p -i -e "s/\/REF\/htop\/elmujj\//\/htop\/elmujj\//g" htop.yoda
rivet-mkhtml -m "/htop/elmujj/probejets/(zbtc|zblc|zbrelc)$" --mc-errs -c ext/htop.plot yoda/nominal.yoda yoda/background.yoda:"backgrounds" htop.yoda:"Title=posterior":"LineColor=green" yoda/data.yoda:"LineColor=black" -o detectorlevel/recoclosure/data

rm -f htop.yoda

yodamerge -o htop.yoda unfold/particlelevel/closure/unfold*/htop.yoda

rivet-mkhtml --mc-errs --pwd -m "/htop/elmujjtrue/truejets/(zbtc|zblc|zbrelc)(|norm)$" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowH7AFII.yoda:"PowH7" yoda/aMCPy8AFII.yoda:"aMCPy8" yoda/Sherpa221AFII.yoda:"Sherpa221" -o unfold/particlelevel/closure/genplots
rivet-mkhtml --mc-errs --pwd -m "/htop/elmujjtrue/truejets/(zbtc|zblc|zbrelc)(|norm)$" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowPy8FSRUpAFII.yoda:"PowPy8FSRUp" yoda/PowPy8FSRDownAFII.yoda:"PowPy8FSRDown" yoda/PowPy6FS.yoda:"PowPy6" -o unfold/particlelevel/closure/powpygenplots

perl -p -i -e "s/\/REF\/htop\/elmujj\//\/htop\/elmujj\//g" htop.yoda
rivet-mkhtml -m "/htop/elmujj/probejets/(zbtc|zblc|zbrelc)$" --mc-errs -c ext/htop.plot yoda/nominal.yoda yoda/background.yoda:"backgrounds" htop.yoda:"Title=posterior":"LineColor=green" yoda/nominal.yoda:"LineColor=black" -o detectorlevel/recoclosure/closure

rm -f htop.yoda

yodamerge -o htop.yoda unfold/particlelevel/closure_statonly/unfold*/htop.yoda

rivet-mkhtml --mc-errs --pwd -m "/htop/elmujjtrue/truejets/(zbtc|zblc|zbrelc)(|norm)$" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowH7AFII.yoda:"PowH7" yoda/aMCPy8AFII.yoda:"aMCPy8" yoda/Sherpa221AFII.yoda:"Sherpa221" -o unfold/particlelevel/closure_statonly/genplots
rivet-mkhtml --mc-errs --pwd -m "/htop/elmujjtrue/truejets/(zbtc|zblc|zbrelc)(|norm)$" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowPy8FSRUpAFII.yoda:"PowPy8FSRUp" yoda/PowPy8FSRDownAFII.yoda:"PowPy8FSRDown" yoda/PowPy6FS.yoda:"PowPy6" -o unfold/particlelevel/closure_statonly/powpygenplots

perl -p -i -e "s/\/REF\/htop\/elmujj\//\/htop\/elmujj\//g" htop.yoda
rivet-mkhtml -m "/htop/elmujj/probejets/(zbtc|zblc|zbrelc)$" --mc-errs -c ext/htop.plot yoda/nominal.yoda yoda/background.yoda:"backgrounds" htop.yoda:"Title=posterior":"LineColor=green" yoda/nominal.yoda:"LineColor=black" -o detectorlevel/recoclosure/closure_statonly

rm -f htop.yoda

yodamerge -o htop.yoda unfold/particlelevel/stress_herwig/unfold*/htop.yoda

rivet-mkhtml --mc-errs --pwd -m "/htop/elmujjtrue/truejets/(zbtc|zblc|zbrelc)(|norm)$" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowH7AFII.yoda:"PowH7" yoda/aMCPy8AFII.yoda:"aMCPy8" yoda/Sherpa221AFII.yoda:"Sherpa221" -o unfold/particlelevel/stress_herwig/genplots
rivet-mkhtml --mc-errs --pwd -m "/htop/elmujjtrue/truejets/(zbtc|zblc|zbrelc)(|norm)$" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowPy8FSRUpAFII.yoda:"PowPy8FSRUp" yoda/PowPy8FSRDownAFII.yoda:"PowPy8FSRDown" yoda/PowPy6FS.yoda:"PowPy6" -o unfold/particlelevel/stress_herwig/powpygenplots

perl -p -i -e "s/\/REF\/htop\/elmujj\//\/htop\/elmujj\//g" htop.yoda
rivet-mkhtml -m "/htop/elmujj/probejets/(zbtc|zblc|zbrelc)$" --mc-errs -c ext/htop.plot yoda/nominal.yoda yoda/background.yoda:"backgrounds" htop.yoda:"Title=posterior":"LineColor=green" yoda/ps.yoda:"Title=PowH7":"LineColor=black" -o detectorlevel/recoclosure/stress_herwig

rm -f htop.yoda

rivet-mkhtml --mc-errs --pwd -m "/htop/elmujjtrue/truejets/(zbtc|zblc|zbrelc)(|norm)$" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowH7AFII.yoda:"PowH7" yoda/aMCPy8AFII.yoda:"aMCPy8" yoda/Sherpa221AFII.yoda:"Sherpa221" -o unfold/particlelevel/genplotsnodata
rivet-mkhtml --mc-errs --pwd -m "/htop/elmujjtrue/truejets/(zbtc|zblc|zbrelc)(|norm)$" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowPy8FSRUpAFII.yoda:"PowPy8FSRUp" yoda/PowPy8FSRDownAFII.yoda:"PowPy8FSRDown" yoda/PowPy6FS.yoda:"PowPy6" -o unfold/particlelevel/powpygenplotsnodata
