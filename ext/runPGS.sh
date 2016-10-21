#!/bin/bash

cd $PBS_O_WORKDIR
source /cvmfs/atlas.cern.ch/repo/ATLASLocalRootBase/user/atlasLocalSetup.sh
localSetupROOT
mkdir $1.run
cd $1.run
ls /nfs/atlas/chgray01/QualificationProject/NTuples_210916/user.chgray.$1*/*.root* > infiles
run-htop --outfile hist.gz --infiles infiles --xsecfile ../data/XSection-MC15-13TeV-fromSusyGrp.data > log 2>&1
