for f in `ls *.hepmc`
do
  rivet --pwd -a BFRAG $f -o ${f/hepmc/yoda} > ${f/hepmc/rivet.log} 2>&1 &
done
