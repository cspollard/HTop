mkdir -p unfold/particlelevel/closure
mkdir -p unfold/particlelevel/data

yodamerge -o htop.yoda unfold/particlelevel/data/unfold*/htop.yoda

rivet-mkhtml --mc-errs --pwd -m ".*/truejets/zb.*" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowH7AFII.yoda:"PowH7" yoda/aMCPy8AFII.yoda:"aMCPy8" yoda/Sherpa221AFII.yoda:"Sherpa221" -o particlelevel/data/genplots
rivet-mkhtml --mc-errs --pwd -m ".*/truejets/zb.*" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowPy8RadUpAFII.yoda:"PowPy8RadUp" yoda/PowPy8RadDownAFII.yoda:"PowPy8RadDown" yoda/PowPy6FS.yoda:"PowPy6" -o particlelevel/data/powpygenplots

rm htop.yoda

yodamerge -o htop.yoda unfold/particlelevel/closure/unfold*/htop.yoda

rivet-mkhtml --mc-errs --pwd -m ".*/truejets/zb.*" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowH7AFII.yoda:"PowH7" yoda/aMCPy8AFII.yoda:"aMCPy8" yoda/Sherpa221AFII.yoda:"Sherpa221" -o particlelevel/closure/genplots
rivet-mkhtml --mc-errs --pwd -m ".*/truejets/zb.*" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowPy8RadUpAFII.yoda:"PowPy8RadUp" yoda/PowPy8RadDownAFII.yoda:"PowPy8RadDown" yoda/PowPy6FS.yoda:"PowPy6" -o particlelevel/closure/powpygenplots

rm htop.yoda

rivet-mkhtml --mc-errs --pwd -m ".*/truejets/.*" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowH7AFII.yoda:"PowH7" yoda/aMCPy8AFII.yoda:"aMCPy8" yoda/Sherpa221AFII.yoda:"Sherpa221" -o particlelevel/genplotsnodata
rivet-mkhtml --mc-errs --pwd -m ".*/truejets/.*" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowPy8RadUpAFII.yoda:"PowPy8RadUp" yoda/PowPy8RadDownAFII.yoda:"PowPy8RadDown" yoda/PowPy6FS.yoda:"PowPy6" -o particlelevel/powpygenplotsnodata

mkdir -p detectorlevel
rivet-mkhtml -m ".*/elmujj/.*" --mc-errs --pwd -c ext/htop.plot yoda/data.yoda yoda/nominal.yoda yoda/ps.yoda yoda/rad.yoda yoda/fsr.yoda yoda/puwgt.yoda yoda/background.yoda:"backgrounds" -o detectorlevel/plots
