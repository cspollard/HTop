run-pythia -n 10000 \
  -c Top:all=on -c 24:onMode=off -c "24:onIfAny=11 13 15" \
  -c Tune:pp=19 \
  -c StringZ:rFactB=1.05 \
  -c PDF:pSet=13 \
  -o A14-rb.hepmc \
  2>&1 > A14-rb.log &
# rivet --pwd -a BFRAG fifo -o A14-rb.yoda

run-pythia -n 10000 \
  -c Top:all=on -c 24:onMode=off -c "24:onIfAny=11 13 15" \
  -c Tune:pp=19 \
  -c PDF:pSet=13 \
  -o A14.hepmc \
  2>&1 > A14.log &
# rivet --pwd -a BFRAG fifo -o A14.yoda

run-pythia -n 10000 \
  -c Top:all=on -c 24:onMode=off -c "24:onIfAny=11 13 15" \
  -c Tune:pp=14 \
  -c PDF:pSet=13 \
  -o Monash.hepmc \
  2>&1 > Monash.log &
# rivet --pwd -a BFRAG fifo -o Monash.yoda

run-pythia -n 10000 \
  -c Top:all=on -c 24:onMode=off -c "24:onIfAny=11 13 15" \
  -c Tune:pp=0 \
  -c PDF:pSet=13 \
  -o Pythia.hepmc \
  2>&1 > Pythia.log &
# rivet --pwd -a BFRAG fifo -o Pythia.yoda

