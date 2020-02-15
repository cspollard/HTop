run-pythia -n 10000 -c Top:all=on -c 24:onMode=off -c "24:onIfAny=11 13" -o fifo &
rivet --pwd -a BFRAG fifo
