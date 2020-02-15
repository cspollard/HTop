for f in `ls *.hepmc`
do
  rivet --pwd -a BFRAG $f -o ${f/hepmc/yoda}
done
