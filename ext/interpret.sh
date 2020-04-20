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
    gridYoda/Merge_*rb*aS*.yoda \
    > unfold/interpret/$1/pythiaA14rbscan.log \
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


plotrb () {

rivet-mkhtml --mc-errs --pwd \
  -m "/BFRAG/(rho|zbtc|zblc|nsvtrk)$" \
  -c ext/htop.plot \
  gridYoda/Merge_$1_aS0120.yoda:'Title=$\alpha_S = 0.120$' \
  gridYoda/Merge_$1_aS0124.yoda:'Title=$\alpha_S = 0.124$' \
  gridYoda/Merge_$1_aS0127.yoda:'Title=$\alpha_S = 0.127$' \
  gridYoda/Merge_$1_aS0130.yoda:'Title=$\alpha_S = 0.130$' \
  gridYoda/Merge_$1_aS0136.yoda:'Title=$\alpha_S = 0.136$' \
  -o unfold/interpret/pythia.$1.plots

}


plotaS () {

rivet-mkhtml --mc-errs --pwd \
  -m "/BFRAG/(rho|zbtc|zblc|nsvtrk)$" \
  -c ext/htop.plot \
  gridYoda/Merge_rb0855_$1.yoda:'Title=$r_B = 0.855$' \
  gridYoda/Merge_rb0920_$1.yoda:'Title=$r_B = 0.920$' \
  gridYoda/Merge_rb0970_$1.yoda:'Title=$r_B = 0.970$' \
  gridYoda/Merge_rb1050_$1.yoda:'Title=$r_B = 1.050$' \
  -o unfold/interpret/pythia.$1.plots

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


rivet-mkhtml --mc-errs --pwd \
  -m "/BFRAG/(rho|zbtc|zblc|nsvtrk)$" \
  -c ext/htop.plot \
  gridYoda/Merge_PP8_EvtGen.yoda:"PowPy8 + EvtGen" \
  gridYoda/Merge_PP8_noEvtGen.yoda:"PowPy8 - EvtGen" \
  -o unfold/interpret/evtgen.plots


for x in rb0855 rb0920 rb0970 rb1050
do
  plotrb $x
done

for x in aS0120 aS0124 aS0127 aS0130 aS0136
do
  plotaS $x
done

rm -f htop.yoda
