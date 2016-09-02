import ROOT as R
from sys import argv

c = R.TChain("nominal")
cw = R.TChain("sumWeights")
for f in argv[2:]:
    c.Add(f)
    cw.Add(f)

f = R.TFile(argv[1], "CREATE")
t = c.CopyTree("ElecN == 1 && MuonN == 1")
tw = cw.CopyTree("")

t.Write()
tw.Write()
f.Close()
