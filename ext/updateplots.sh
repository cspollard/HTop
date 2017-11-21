mkdir -p closuredata
cp realdata/unfoldyoda/htop.yoda ./
rivet-mkhtml --mc-errs --pwd -m ".*/truejets/.*" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowH7AFII.yoda:"PowH7" yoda/aMCPy8AFII.yoda:"aMCPy8" yoda/Sherpa221AFII.yoda:"Sherpa221" -o closuredata/genplots

rivet-mkhtml --mc-errs --pwd -m ".*/truejets/.*" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowPy8RadUpAFII.yoda:"PowPy8RadUp" yoda/PowPy8RadDownAFII.yoda:"PowPy8RadDown" yoda/PowPy6FS.yoda:"PowPy6" -o closuredata/powpygenplots

mkdir -p realdata
rivet-mkhtml -m ".*/elmujj/.*" --mc-errs --pwd -c ext/htop.plot yoda/data.yoda yoda/nominal.yoda yoda/ps.yoda yoda/me.yoda yoda/rad.yoda yoda/puwgt.yoda yoda/background.yoda:"backgrounds" -o realdata/plots
