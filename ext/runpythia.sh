run-pythia -n 10000 \
  -c Tune:pp=19 \
  -c StringZ:rFactB=1.05 \
  -c PDF:pSet=13 \
  -c Top:all=on -c 24:onMode=off -c "24:onIfAny=11 13" -o fifo &
rivet --pwd -a BFRAG fifo -o A14-rb.yoda

run-pythia -n 10000 \
  -c Tune:pp=19 \
  -c PDF:pSet=13 \
  -c Top:all=on -c 24:onMode=off -c "24:onIfAny=11 13" -o fifo &
rivet --pwd -a BFRAG fifo -o A14.yoda

run-pythia -n 10000 \
  -c Tune:pp=14 \
  -c PDF:pSet=13 \
  -c Top:all=on -c 24:onMode=off -c "24:onIfAny=11 13" -o fifo &
rivet --pwd -a BFRAG fifo -o Monash.yoda

run-pythia -n 10000 \
  -c Tune:pp=0 \
  -c PDF:pSet=13 \
  -c Top:all=on -c 24:onMode=off -c "24:onIfAny=11 13" -o fifo &
rivet --pwd -a BFRAG fifo -o Pythia.yoda

