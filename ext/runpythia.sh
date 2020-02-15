# rb values to test
rbs="0.855 1.05 0.67"

# as values to test
ases="0.127 0.136 0.124"


NEVT=1000000

for rb in $rbs
do
  for as in $ases
  do
    BASE=A14_rb${rb}_as${as}
    mkfifo ${BASE}.hepmc
	  run-pythia -s -n $NEVT \
      -c Top:all=on -c 24:onMode=off -c "24:onIfAny=11 13 15" \
      -c Tune:pp=19 \
      -c StringZ:rFactB=${rb} \
      -c TimeShower:alphaSvalue=${as} \
      -c PDF:pSet=13 \
      -o ${BASE}.hepmc \
      2>&1 > ${BASE}.log & \
    rivet --pwd -a BFRAG -o ${BASE}.yoda ${BASE}.hepmc > ${BASE}.rivet.log 2>&1 &
  done
done
  

BASE=Monash
mkfifo ${BASE}.hepmc
run-pythia -s -n $NEVT \
  -c Top:all=on -c 24:onMode=off -c "24:onIfAny=11 13 15" \
  -c Tune:pp=14 \
  -c PDF:pSet=13 \
  -o ${BASE}.hepmc \
  2>&1 > ${BASE}.log & \
rivet --pwd -a BFRAG -o ${BASE}.yoda ${BASE}.hepmc > ${BASE}.rivet.log 2>&1 &
