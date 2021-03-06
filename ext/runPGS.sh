#!/bin/bash

# for instance
# for f in 0 1 2 3; do qsub -j oe -q medium6 -v F=run/$f ext/runPGS.sh; done

cd $PBS_O_WORKDIR

echo "files to be analyzed:"
for f in `cat $F.infiles`; do ls $f; done

source /cvmfs/atlas.cern.ch/repo/ATLASLocalRootBase/user/atlasLocalSetup.sh
lsetup "root 6.04.14-x86_64-slc6-gcc49-opt"

time stack exec run-htop -- --outfile $F.hist.gz --infiles $F.infiles > $F.log 2>&1
