#!/bin/bash

cd $PBS_O_WORKDIR
source /cvmfs/atlas.cern.ch/repo/ATLASLocalRootBase/user/atlasLocalSetup.sh
lsetup root
run-htop --outfile $1.hist.gz --infiles $1.infiles > $1.log 2>&1
