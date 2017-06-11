#!/bin/bash

# for instance
# for f in 0 1 2 3; do qsub -j oe -q medium6 -v F=run/$f ext/runPGS.sh; done

cd $PBS_O_WORKDIR
source /cvmfs/atlas.cern.ch/repo/ATLASLocalRootBase/user/atlasLocalSetup.sh
lsetup root
time run-htop --outfile $F.hist.gz --infiles $F.infiles > $F.log 2>&1
