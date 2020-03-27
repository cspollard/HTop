interpret () {

  mkdir -p unfold/interpret/$1

  python ext/pvalue.py /BFRAG/$1 unfold/mcmc/data/$1mcmc.dat \
    gridYoda/Merge_410470_PhPy8.yoda:PowPy8 \
    gridYoda/Merge_410465_aMcAtNloPy8.yoda:aMCNLOPy8 \
    gridYoda/Merge_410503_PhPy8.yoda:PowPy8_410503 \
    gridYoda/Merge_411360_PP8_MECoff_grec.yoda:PowPy8_MECoff_grec \
    > unfold/interpret/$1/pythia.log \
    2>&1


  python ext/pvalue.py /BFRAG/$1 unfold/mcmc/data/$1mcmc.dat \
    gridYoda/Merge_410470_PhPy8.yoda:PowPy8 \
    gridYoda/Merge_410028_V2Up.yoda:PowPy8FSRUp \
    gridYoda/Merge_410029_V2Down.yoda:PowPy8FSRDown \
    gridYoda/Merge_411291_PP8_rb1p05.yoda:PowPy8_A14rb \
    > unfold/interpret/$1/pythiaA14vars.log \
    2>&1


  python ext/pvalue.py /BFRAG/$1 unfold/mcmc/data/$1mcmc.dat \
    gridYoda/Merge_410470_PhPy8.yoda:PowPy8 \
    gridYoda/Merge_PH704.yoda:PowHer704 \
    gridYoda/Merge_411234_PH713.yoda:PowHer713 \
    gridYoda/Merge_PH16.yoda:PowHer716 \
    > unfold/interpret/$1/herwig.log \
    2>&1


  python ext/pvalue.py /BFRAG/$1 unfold/mcmc/data/$1mcmc.dat \
    gridYoda/Merge_410470_PhPy8.yoda:PowPy8 \
    gridYoda/Merge_410252_Sh221.yoda:Sherpa221 \
    gridYoda/Merge_950010_Sh228_HTprime.yoda:Sherpa228HTprime \
    gridYoda/Merge_950033_Sh228_CSSevol.yoda:Sherpa228CSSevol \
    > unfold/interpret/$1/sherpa.log \
    2>&1


  mv -f teststats.pdf unfold/interpret/$1/teststats.pdf

}



yodamerge -o htop.yoda unfold/particlelevel/data/unfold*/htop.yoda
yodacnv -m "(rho|zbtc|zblc|nsvtrk)norm$" htop.yoda htop.yoda

perl -p -i -e "s/htop.*jets/BFRAG/g" htop.yoda
perl -p -i -e "s/norm//g" htop.yoda


for obs in zblc rho nsvtrk zbtc
do
  interpret $obs
done


rivet-mkhtml --mc-errs --pwd \
  -m "/BFRAG/(rho|zbtc|zblc|nsvtrk)$" \
  -c ext/htop.plot \
  gridYoda/Merge_410470_PhPy8.yoda:"PowPy8" \
  gridYoda/Merge_410465_aMcAtNloPy8.yoda:"aMCNLOPy8" \
  gridYoda/Merge_410503_PhPy8.yoda:"PowPy8_410503" \
  gridYoda/Merge_411360_PP8_MECoff_grec.yoda:"PowPy8_MECoff_grec" \
  -o unfold/interpret/pythia.plots


rivet-mkhtml --mc-errs --pwd \
  -m "/BFRAG/(rho|zbtc|zblc|nsvtrk)$" \
  -c ext/htop.plot \
  gridYoda/Merge_410470_PhPy8.yoda:"PowPy8" \
  gridYoda/Merge_410028_V2Up.yoda:"PowPy8FSRUp" \
  gridYoda/Merge_410029_V2Down.yoda:"PowPy8FSRDown" \
  gridYoda/Merge_411291_PP8_rb1p05.yoda:"PowPy8_A14rb" \
  -o unfold/interpret/pythiaA14vars.plots \


rivet-mkhtml --mc-errs --pwd \
  -m "/BFRAG/(rho|zbtc|zblc|nsvtrk)$" \
  -c ext/htop.plot \
  gridYoda/Merge_PH704.yoda:"PowHer704" \
  gridYoda/Merge_411234_PH713.yoda:"PowHer713" \
  gridYoda/Merge_PH16.yoda:"PowHer716" \
  -o unfold/interpret/herwig.plots


rivet-mkhtml --mc-errs --pwd \
  -m "/BFRAG/(rho|zbtc|zblc|nsvtrk)$" \
  -c ext/htop.plot \
  gridYoda/Merge_410252_Sh221.yoda:"Sherpa221" \
  gridYoda/Merge_950010_Sh228_HTprime.yoda:"Sherpa228HTprime" \
  gridYoda/Merge_950033_Sh228_CSSevol.yoda:"Sherpa228CSSevol" \
  -o unfold/interpret/sherpa.plots


rm -f htop.yoda
