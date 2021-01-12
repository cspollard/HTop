#!/bin/bash

cd /home/ppe/c/cspollard/Programming/htop.git

echo "CWD:"
pwd -P

time stack exec run-htop -- --outfile ${1/infiles/hist.gz}.hist.gz --infiles $1
