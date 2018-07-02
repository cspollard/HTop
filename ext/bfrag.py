import ROOT as R
R.TH1.SetDefaultSumw2()
R.gROOT.SetBatch(1)
R.gROOT.SetStyle("Plain")
R.gStyle.SetOptTitle(0)
R.gStyle.SetOptStat(0)
R.gROOT.LoadMacro("style/AtlasStyle.C")
R.SetAtlasStyle()
R.gStyle.SetTitleOffset(1.5, "y")
R.gStyle.SetTitleOffset(1.5, "z")

import array as A

def rebin(vals, binedges, (imin, imax), comb):

    n = len(vals) - 2
    if imin < 1:
        print "error: imin < 1"
        exit()
    elif imax > n:
        print "error: imax > n"
        print "imax: %d, n: %d" % (imax, n)
        exit()

    newvals = [vals[0]]
    newbinedges = []
    for (i, v) in enumerate(vals[1:], 1):
        if i > imin and i <= imax:
            newvals[-1] = comb(newvals[-1], v)
        else:
            newvals.append(v)
            newbinedges.append(binedges[i-1])

    return (newvals, newbinedges)


def rebinTH2FX(h, rebins):
    if len(rebins) == 0:
        return h

    def project(h, i):
        return h.ProjectionY(h.GetName() + "_px%d" % i, i, i)

    def hadd(h1, h2):
        h = h1.Clone(h1.GetName() + "_add")
        h.Add(h2)
        return h

    xaxis = h.GetXaxis()
    h1s = map(lambda i: project(h, i), range(0, h.GetNbinsX()+2))
    binedges = map(lambda i: xaxis.GetBinLowEdge(i), range(1, h.GetNbinsX()+2))

    while len(rebins) > 0:
        (imin, imax) = rebins[0]
        (h1s, binedges) = rebin(h1s, binedges, (imin, imax), hadd)
        n = imax - imin
        rebins = map(lambda (mn, mx): (mn-n, mx-n), rebins[1:])

    ybinedges = \
        map(lambda i: h.GetYaxis().GetBinLowEdge(i), range(1, h.GetNbinsY()+2))

    hrebin = \
        R.TH2F(h.GetName() + "_rebin", h.GetTitle(),
            len(binedges)-1, A.array('d', binedges),
            h.GetNbinsY(), A.array('d', ybinedges))


    for i in range(0, hrebin.GetNbinsX()+2):
        for j in range(0, hrebin.GetNbinsY()+2):
            hrebin.SetBinContent(i, j, h1s[j].GetBinContent(i))
            hrebin.SetBinError(i, j, h1s[j].GetBinError(i))

    return hrebin


def rebinTH2FY(h, rebins):
    return transposeTH2F(rebinTH2FX(transposeTH2F(h), rebins))


def transposeTH2F(h):
    nx = h.GetNbinsX()
    ny = h.GetNbinsY()

    xaxis = h.GetXaxis()
    yaxis = h.GetYaxis()

    xbinedges = \
        map(lambda i: xaxis.GetBinLowEdge(i), range(1, h.GetNbinsX()+2))
    ybinedges = \
        map(lambda i: yaxis.GetBinLowEdge(i), range(1, h.GetNbinsY()+2))

    ht = R.TH2F(h.GetName()+"_transposed", h.GetTitle(),
            ny, A.array('d', ybinedges),
            nx, A.array('d', xbinedges))

    for i in range(0, nx+2):
        for j in range(0, ny+2):
            ht.SetBinContent(j, i, h.GetBinContent(i, j))
            ht.SetBinError(j, i, h.GetBinError(i, j))

    ht.GetXaxis().SetTitle(yaxis.GetTitle())
    ht.GetYaxis().SetTitle(xaxis.GetTitle())
    return ht


def getSVEffHist(inf):
    hnum = inf.Get("SVeffNum").Clone()
    hden = inf.Get("SVeffDen").Clone()

    hnum.Rebin(10)
    hden.Rebin(10)

    hrat = hnum.Clone()
    hrat.Sumw2()
    hrat.Divide(hnum, hden, 1, 1, "B")

    hrat.GetXaxis().SetTitle("p_{T}^{B} [MeV]")
    hrat.GetYaxis().SetTitle("SV efficiency")

    return hrat


def getSVResAbsHist(inf):
    h = transposeTH2F(inf.Get("SVresabsvsbHpT"))
    h.Sumw2()
    h.RebinX(5)

    rebiny = \
        [ (1, 10), (11, 20), (21, 30), (31, 40), (41, 50)
        , (51, 55), (56, 60), (61, 65), (66, 70)
        , (71, 80), (81, 90), (91, 100)
        ]

    h1 = rebinTH2FY(h, rebiny)
    h1.SetTitle(h.GetTitle())
    h1.SetName("SVresabsvsbHpT")
    h = h1

    normalize(h)

    h.GetXaxis().SetTitle("p_{T}^{B} [MeV]")
    h.GetYaxis().SetTitle("p_{T}^{SV} - p_{T}^{B} [MeV]")

    return h


def getSVResRelHist(inf):
    h = transposeTH2F(inf.Get("SVresrelvsbHpT"))
    h.Sumw2()

    h.RebinX(5)
    h.RebinY(10)

    normalize(h)

    h.GetXaxis().SetTitle("p_{T}^{B} [MeV]")
    h.GetYaxis().SetTitle("(p_{T}^{SV} - p_{T}^{B}) / p_{T}^{B} [MeV]")

    return h


def getSumTrkPtHist(inf):
    h = inf.Get("jptvstrkpt").Clone()
    h.Sumw2()
    h.RebinX(10)
    h.RebinY(5)
    normalize(h)
    h.GetXaxis().SetTitle("jet p_{T} [MeV]")
    h.GetYaxis().SetTitle("track sum p_{T} [MeV]")
    return h


def getDEtaVsPtHist(inf):
    h = transposeTH2F(inf.Get("SVJdEtavsbHtpt"))
    h.Sumw2()
    h.RebinX(5)
    h.RebinY(10)
    normalize(h)
    h.GetXaxis().SetTitle("p_{T}^{B} [MeV]")
    h.GetYaxis().SetTitle("#Delta #eta(SV, B)")
    return h


def getDPhiVsPtHist(inf):
    h = transposeTH2F(inf.Get("SVJdPhivsbHtpt"))
    h.Sumw2()
    h.RebinX(5)
    h.RebinY(10)
    normalize(h)
    h.GetXaxis().SetTitle("p_{T}^{B} [MeV]")
    h.GetYaxis().SetTitle("#Delta #phi(SV, B)")
    return h


def getDRVsPtHist(inf):
    h = transposeTH2F(inf.Get("SVJdRvsbHtpt"))
    h.Sumw2()
    h.RebinX(5)
    h.RebinY(2)
    normalize(h)
    h.GetXaxis().SetTitle("p_{T}^{B} [MeV]")
    h.GetYaxis().SetTitle("#Delta R(SV, B)")
    return h


def getDEtaHist(inf):
    h = inf.Get("SVJdEta")
    h.Sumw2()
    rebinx = \
        [ -0.5, -0.4, -0.3, -0.2, -0.1
        , -0.05, -0.04, -0.03, -0.02, -0.01
        , 0.0, 0.01, 0.02, 0.03, 0.04, 0.05
        , 0.1, 0.2, 0.3, 0.4, 0.5
        ]

    h = h.Rebin(len(rebinx)-1, "SVJdEta", A.array('d', rebinx))

    normalize(h)
    h.GetXaxis().SetTitle("#Delta #eta(SV, B)")
    return h


def getDPhiHist(inf):
    h = inf.Get("SVJdPhi")
    h.Sumw2()
    rebinx = \
        [ -0.5, -0.4, -0.3, -0.2, -0.1
        , -0.05, -0.04, -0.03, -0.02, -0.01
        , 0.0, 0.01, 0.02, 0.03, 0.04, 0.05
        , 0.1, 0.2, 0.3, 0.4, 0.5
        ]

    h = h.Rebin(len(rebinx)-1, "SVJdPhi", A.array('d', rebinx))

    normalize(h)
    h.GetXaxis().SetTitle("#Delta #phi(SV, B)")
    return h


def normalize(h):
    if "TH1" in h.ClassName():
        h.Scale(1.0/h.Integral(0, h.GetNbinsX()+1))
        h.GetYaxis().SetTitle("normalized")

    elif "TH2" in h.ClassName():
        h.Scale(1.0/h.Integral(0, h.GetNbinsX()+1, 0, h.GetNbinsY()+1))
        h.GetZaxis().SetTitle("normalized")

    return


def getAbsDiff(hvar, hnom):
    hdiff = hvar.Clone(hvar.GetName() + "_diff")
    hdiff.Add(hnom, -1);

    if "TH1" in hdiff.ClassName():
        hdiff.GetYaxis().SetTitle("P(var) - P(nom)")

    elif "TH2" in hdiff.ClassName():
        hdiff.GetZaxis().SetTitle("P(var) - P(nom)")

    return hdiff


def getRelDiff(hdiff, hnom):
    hrat = hdiff.Clone(hdiff.GetName() + "rel")
    hrat.Divide(hrat, hnom)

    if "TH1" in diff.ClassName():
        hrat.GetYaxis().SetTitle("(P(var) - P(nom)) / P(nom)")

    elif "TH2" in diff.ClassName():
        hrat.GetZaxis().SetTitle("(P(var) - P(nom)) / P(nom)")

    return hrat


def histaccess(inf):
    return \
        [ ("sveff", getSVEffHist(inf))
        , ("svabsres", getSVResAbsHist(inf))
        , ("svrelres", getSVResRelHist(inf))
        , ("trkptsum", getSumTrkPtHist(inf))
        , ("svdeta", getDEtaHist(inf))
        , ("svdphi", getDPhiHist(inf))
        ]


def listToStr(l):
    lstrs = map(lambda x: "%.2e" % x, l)
    str = "[" + ", ".join(lstrs) + "]"
    return str


def lines(l):
    return "\n".join(l)


def toText1D(n, h):
    xaxis = h.GetXaxis()
    xbinedges = \
        map(lambda i: xaxis.GetBinLowEdge(i), range(1, h.GetNbinsX()+2))
    xbinstr = listToStr(xbinedges)

    values = \
        map(lambda i: h.GetBinContent(i), range(1, h.GetNbinsX()+1))
    valsstr = listToStr(values)

    histstr = lines(
        [ "%s :: Histogram Vector (ArbBin Double) Double" % n
        , "%s = histogram xbins vals" % n
        , "  where"
        , "    xbins = arbBin %s" % xbinstr
        , "    vals = %s" % valsstr
        , ""
        , ""
        ]
        )

    return histstr


def toText2D(n, h):
    xaxis = h.GetXaxis()
    xbinedges = \
        map(lambda i: xaxis.GetBinLowEdge(i), range(1, h.GetNbinsX()+2))
    xbinstr = listToStr(xbinedges)

    yaxis = h.GetYaxis()
    ybinedges = \
        map(lambda i: yaxis.GetBinLowEdge(i), range(1, h.GetNbinsY()+2))
    ybinstr = listToStr(ybinedges)

    values = \
        [ h.GetBinContent(j, i) \
            for i in range(1, h.GetNbinsX()+1) \
            for j in range(1, h.GetNbinsY()+1)
        ]
    valsstr = listToStr(values)

    histstr = lines(
        [ "%s :: Histogram Vector (Bin2D (ArbBin Double) (ArbBin Double)) Double" % n
        , "%s = histogram (Bin2D xbins ybins) vals" % n
        , "  where"
        , "    xbins = arbBin %s" % xbinstr
        , "    ybins = arbBin %s" % ybinstr
        , "    vals = %s" % valsstr
        , ""
        , ""
        ]
        )


    return histstr



from sys import argv
systname = ".".join(argv[1].split(".")[:-1])
sourceout = open(argv[2], 'a')

outf = R.TFile.Open(systname + ".tmp", "RECREATE")
nomf = R.TFile.Open("v2TRK_NOMINAL.root", "READ")
varf = R.TFile.Open(systname + ".root", "READ")

outf.cd()

nomHs = histaccess(nomf)
map(lambda (n, h): h.SetName(h.GetName() + "_nom"), nomHs)

varHs = histaccess(varf)
map(lambda (n, h): h.SetName(h.GetName() + "_var"), varHs)

c = R.TCanvas("c", "c", 800, 600)
c.SetRightMargin(0.2)
c.SetLeftMargin(0.15)



for ((n, hn), (m, hv)) in zip(nomHs, varHs):
    diff = getAbsDiff(hv, hn)
    reldiff = getRelDiff(diff, hn)

    name = n + "_" + systname

    def save(post):
        c.SaveAs(name + post + ".png")
        c.Clear()

    if "TH1" in diff.ClassName():
        hv.Draw("e")
        save("")
        hv.Write()

        diff.Draw("e")
        diff.GetYaxis().SetRangeUser(-0.02, 0.02)
        save("_diff")
        diff.Write()

        reldiff.Draw("e")
        reldiff.GetYaxis().SetRangeUser(-0.1, 0.1)
        save("_reldiff")
        reldiff.Write()

        sourceout.write(toText1D(name, reldiff))

    elif "TH2" in diff.ClassName():
        hv.Draw("colz")
        save("")
        hv.Write()

        diff.Draw("colz")
        diff.GetZaxis().SetRangeUser(-0.01, 0.01)
        save("_diff")
        diff.Write()

        reldiff.Draw("colz")
        reldiff.GetZaxis().SetRangeUser(-0.3, 0.3)
        save("_reldiff")
        reldiff.Write()

        sourceout.write(toText2D(name, reldiff))
