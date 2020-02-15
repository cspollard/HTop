for f in A14_rb*yoda
do yodascale -i -c ".* 1" $f
done

yodascale -i -c ".* 1" Monash.yoda

for rb in 1.05 0.97 0.92 0.855
do rivet-mkhtml --mc-errs --pwd A14_rb${rb}*.yoda -c ../ext/htop.plot -o rb${rb}
done
