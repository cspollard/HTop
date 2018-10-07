mkdir -p unfold/particlelevel/closure
mkdir -p unfold/particlelevel/closure_statonly
mkdir -p unfold/particlelevel/data

yodamerge -o htop.yoda unfold/particlelevel/data/unfold*/htop.yoda

rivet-mkhtml --mc-errs --pwd -m ".*/truejets/zb.*" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowH7AFII.yoda:"PowH7" yoda/aMCPy8AFII.yoda:"aMCPy8" yoda/Sherpa221AFII.yoda:"Sherpa221" -o unfold/particlelevel/data/genplots
rivet-mkhtml --mc-errs --pwd -m ".*/truejets/zb.*" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowPy8FSRUpAFII.yoda:"PowPy8FSRUp" yoda/PowPy8FSRDownAFII.yoda:"PowPy8FSRDown" yoda/PowPy6FS.yoda:"PowPy6" -o unfold/particlelevel/data/powpygenplots

rm htop.yoda

yodamerge -o htop.yoda unfold/particlelevel/closure/unfold*/htop.yoda

rivet-mkhtml --mc-errs --pwd -m ".*/truejets/zb.*" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowH7AFII.yoda:"PowH7" yoda/aMCPy8AFII.yoda:"aMCPy8" yoda/Sherpa221AFII.yoda:"Sherpa221" -o unfold/particlelevel/closure/genplots
rivet-mkhtml --mc-errs --pwd -m ".*/truejets/zb.*" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowPy8FSRUpAFII.yoda:"PowPy8FSRUp" yoda/PowPy8FSRDownAFII.yoda:"PowPy8FSRDown" yoda/PowPy6FS.yoda:"PowPy6" -o unfold/particlelevel/closure/powpygenplots

rm htop.yoda

yodamerge -o htop.yoda unfold/particlelevel/closure_statonly/unfold*/htop.yoda

rivet-mkhtml --mc-errs --pwd -m ".*/truejets/zb.*" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowH7AFII.yoda:"PowH7" yoda/aMCPy8AFII.yoda:"aMCPy8" yoda/Sherpa221AFII.yoda:"Sherpa221" -o unfold/particlelevel/closure_statonly/genplots
rivet-mkhtml --mc-errs --pwd -m ".*/truejets/zb.*" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowPy8FSRUpAFII.yoda:"PowPy8FSRUp" yoda/PowPy8FSRDownAFII.yoda:"PowPy8FSRDown" yoda/PowPy6FS.yoda:"PowPy6" -o unfold/particlelevel/closure_statonly/powpygenplots

rm htop.yoda

rivet-mkhtml --mc-errs --pwd -m ".*/truejets/.*" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowH7AFII.yoda:"PowH7" yoda/aMCPy8AFII.yoda:"aMCPy8" yoda/Sherpa221AFII.yoda:"Sherpa221" -o unfold/particlelevel/genplotsnodata
rivet-mkhtml --mc-errs --pwd -m ".*/truejets/.*" -c ext/htop.plot yoda/PowPy8FS.yoda:"PowPy8" yoda/PowPy8FSRUpAFII.yoda:"PowPy8FSRUp" yoda/PowPy8FSRDownAFII.yoda:"PowPy8FSRDown" yoda/PowPy6FS.yoda:"PowPy6" -o unfold/particlelevel/powpygenplotsnodata

mkdir -p detectorlevel
rivet-mkhtml -m ".*/elmujj/.*" --mc-errs --pwd -c ext/htop.plot yoda/data.yoda yoda/nominal.yoda yoda/ps.yoda:"parton shower" yoda/rad.yoda:"ISR" yoda/fsr.yoda:"FSR" yoda/puwgt.yoda:"pileup" yoda/background.yoda:"backgrounds" -o detectorlevel/modeling
rivet-mkhtml -m ".*/elmujj/.*" --mc-errs --pwd -c ext/htop.plot yoda/data.yoda yoda/nominal.yoda yoda/npvtrksf.yoda:"PV track multiplicity" yoda/nsvtrksf.yoda:"SV track multiplicity" yoda/v2trk_fake_rate_tight.yoda:"track fake rate" yoda/v2trk_eff_loose_global.yoda:"track efficiency" yoda/v2trk_res_d0_meas.yoda:"track \$d_0\$" yoda/v2trk_res_z0_meas.yoda:"track \$z_0\$" yoda/background.yoda:"backgrounds" -o detectorlevel/tracking
