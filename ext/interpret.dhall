let Prelude = https://prelude.dhall-lang.org/package.dhall

let Style = { color : Text, line : Text, marker : Text }

let red
    : Style
    = { color = "red", line = "solid", marker = "asterisk" }

let blue
    : Style
    = { color = "blue", line = "dotted", marker = "diamond*" }

let green
    : Style
    = { color = "green", line = "dashed", marker = "triangle*" }

let orange
    : Style
    = { color = "orange", line = "dashdotted", marker = "square*" }

let MC = { path : Text, title : Text }

let powpy8 =
      { path = "gridYoda/Merge_410503_PhPy8.yoda"
      , title = "Pow+Py8 \$r_B = 0.855 \\, \\alpha_S = 0.127\$"
      }

let aMCNLOPy8 =
      { path = "gridYoda/Merge_410465_aMcAtNloPy8.yoda"
      , title = "aMC@NLO+Pythia8"
      }

let powPy8_410470 =
      { path = "gridYoda/Merge_410470_PhPy8.yoda"
      , title = "Pow+Py8 (410470)"
      }

let powPy8_MECoff_grec =
      { path = "gridYoda/Merge_411360_PP8_MECoff_grec.yoda"
      , title = "Pow+Py8 MECoff_grec"
      }

let powpy8_fsrup =
      { path = "gridYoda/Merge_410028_V2Up.yoda"
      , title = "Pow+Py8 \$r_B = 0.855 \\, \\alpha_s = 0.136\$"
      }

let powpy8_fsrdown =
      { path = "gridYoda/Merge_410029_V2Down.yoda"
      , title = "Pow+Py8 \$r_B = 0.855 \\, \\alpha_s = 0.124\$"
      }

let powpy8_a14rb =
      { path = "gridYoda/Merge_411291_PP8_rb1p05.yoda"
      , title = "Pow+Py8 \$r_B = 0.105 \\, \\alpha_s = 0.127\$"
      }

let powher704 =
      { path = "gridYoda/Merge_PH704.yoda", title = "Pow+Her 7.0.4" }

let powher713 =
      { path = "gridYoda/Merge_411234_PH713.yoda"
      , title = "Pow+Her 7.1.3"
      }

let powher716 =
      { path = "gridYoda/Merge_PH16.yoda", title = "Pow+Her 7.1.6" }

let sh221 =
      { path = "gridYoda/Merge_410252_Sh221.yoda", title = "Sherpa 2.2.1" }

let sh228 =
      { path = "gridYoda/Merge_950010_Sh228_HTprime.yoda"
      , title = "Sherpa 2.2.8"
      }

let sh228fix =
      { path = "gridYoda/Merge_950033_Sh228_CSSevol.yoda"
      , title = "Sherpa 2.2.8 (\$b \\to bg\$ fix)"
      }

let PlotRecord = MC ⩓ Style

let Plot = List PlotRecord

let pythia
    : Plot
    = [ powpy8 ∧ red
      , aMCNLOPy8 ∧ blue
      , powPy8_410470 ∧ green
      , powPy8_MECoff_grec ∧ orange
      ]

let pythiaa14s
    : Plot
    = [ powpy8 ∧ red
      , powpy8_fsrup ∧ blue
      , powpy8_fsrdown ∧ green
      , powpy8_a14rb ∧ orange
      ]

let herwigs
    : Plot
    = [ powpy8 ∧ red, powher704 ∧ blue, powher713 ∧ green, powher716 ∧ orange ]

let sherpas
    : Plot
    = [ powpy8 ∧ red, sh221 ∧ blue, sh228 ∧ green, sh228fix ∧ orange ]

let gens
    : Plot
    = [ powpy8 ∧ red, powher704 ∧ blue, sh221 ∧ green ]

let pathstyle =
      λ(p : PlotRecord) →
        "'${p.path}:Title=${p.title}:PlotOrder=0:LineColor=${p.color}:LineStyle=${p.line}:PolyMarker=${p.marker}:DotScale=1.5'"

let pathtitle = λ(p : PlotRecord) → "'${p.path}:${p.title}'"

let pvalue =
      λ(obs : Text) →
      λ(p : Text) →
      λ(ps : Plot) →
        let t =
              Prelude.Text.concatSep
                ''
                ${" "}\
                ${" "} ''
                (Prelude.List.map PlotRecord Text pathtitle ps)

        in  ''
            python ext/pvalue.py /BFRAG/${obs} unfold/mcmc/data/${obs}mcmc.dat \
              ${t} \
              > unfold/interpret/${obs}/${p}.log \
              2>&1
            ''

let plot =
      λ(p : Text) →
      λ(ps : Plot) →
        let t =
              Prelude.Text.concatSep
                ''
                ${" "}\
                ${" "} ''
                (Prelude.List.map PlotRecord Text pathstyle ps)

        in  ''
            rivet-mkhtml --pwd \
              -m "/BFRAG/(rho|zbtc|zblc|nsvtrk)$" \
              -c ext/htop.plot \
              ${t} \
              -o unfold/interpret/${p}.plots
            ''

in  Prelude.Text.concatSep
      "\n\n"
      [ pvalue "rho" "pythia" pythia
      , pvalue "rho" "pythiaA14" pythiaa14s
      , pvalue "rho" "herwig" herwigs
      , pvalue "rho" "sherpa" sherpas
      , plot "pythia" pythia
      , plot "pythiaA14" pythiaa14s
      , plot "herwig" herwigs
      , plot "sherpa" sherpas
      , plot "generators" gens
      ]
