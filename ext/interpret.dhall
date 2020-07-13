let Prelude = https://prelude.dhall-lang.org/package.dhall

let concatSep = Prelude.Text.concatSep
let lmap = Prelude.List.map
let default = Prelude.Optional.default

let Color = { color : Text }
let Line = { line : Text }
let Marker = { marker : Text }
let EBand = { band : Text }

let black = { color = "black" }
let red = { color = "red" }
let blue = { color = "blue" }
let green = { color = "green" }
let orange = { color = "orange" }

let noline = { line = "none" }
let solid = { line = "solid" }
let dotted = { line = "dotted" }
let dashed = { line = "dashed" }
let dashdotted = { line = "dashdotted" }

let dot = { marker = "*" }
let asterisk = { marker = "asterisk" }
let diamond = { marker = "diamond*" }
let triangle = { marker = "triangle*" }
let square = { marker = "square*" }

let extra = \(t : Text) -> { extra=t }
let nomore = extra ""

let data =
      { path = "BFRAG.yoda"
      , title = "data"
      , name = "data"
      } /\ black /\ solid /\ dot /\ extra ":ConnectBins=0:ErrorBars=1"

let powpy8 =
      { path = "gridYoda/Merge_410503_PhPy8.yoda"
      , title = "Pow+Py8 \$r_B = 0.855 \\, \\alpha_S = 0.127\$"
      , name = "powpy8"
      } /\ red /\ solid /\ asterisk /\ nomore

let aMCNLOPy8 =
      { path = "gridYoda/Merge_410465_aMcAtNloPy8.yoda"
      , title = "aMC@NLO+Pythia8"
      , name = "aMCNLOPy8"
      } /\ blue /\ dotted /\ square /\ nomore

let powPy8_410470 =
      { path = "gridYoda/Merge_410470_PhPy8.yoda"
      , title = "Pow+Py8 (410470)"
      , name = "powPy8_410470"
      } /\ green /\ dashed /\ triangle /\ nomore

let powPy8_MECoff_grec =
      { path = "gridYoda/Merge_411360_PP8_MECoff_grec.yoda"
      , title = "Pow+Py8 MECoff_grec"
      , name = "powPy8_MECoff_grec"
      } /\ orange /\ dashdotted /\ diamond /\ nomore

let powpy8_fsrup =
      { path = "gridYoda/Merge_410028_V2Up.yoda"
      , title = "Pow+Py8 \$r_B = 0.855 \\, \\alpha_s = 0.136\$"
      , name = "powpy8_fsrup"
      } /\ blue /\ dotted /\ square /\ nomore

let powpy8_fsrdown =
      { path = "gridYoda/Merge_410029_V2Down.yoda"
      , title = "Pow+Py8 \$r_B = 0.855 \\, \\alpha_s = 0.124\$"
      , name = "powpy8_fsrdown"
      } /\ green /\ dashed /\ triangle /\ nomore

let powpy8_a14rb =
      { path = "gridYoda/Merge_411291_PP8_rb1p05.yoda"
      , title = "Pow+Py8 \$r_B = 0.105 \\, \\alpha_s = 0.127\$"
      , name = "powpy8_a14rb"
      } /\ orange /\ dashdotted /\ diamond /\ nomore

let powher704 =
      { path = "gridYoda/Merge_PH704.yoda"
      , title = "Pow+Her 7.0.4"
      , name = "powher704"
      } /\ green /\ dotted /\ square /\ nomore

let powher713 =
      { path = "gridYoda/Merge_411234_PH713.yoda"
      , title = "Pow+Her 7.1.3"
      , name = "powher713"
      } /\ blue /\ dashdotted /\ diamond /\ nomore

let powher716 =
      { path = "gridYoda/Merge_PH16.yoda"
      , title = "Pow+Her 7.1.6"
      , name = "powher716"
      } /\ orange /\ dashed /\ triangle /\ nomore


let sh221 =
      { path = "gridYoda/Merge_410252_Sh221.yoda"
      , title = "Sherpa 2.2.1"
      , name = "sh221"
      } /\ blue /\ dashdotted /\ diamond /\ nomore

let sh228 =
      { path = "gridYoda/Merge_950010_Sh228_HTprime.yoda"
      , title = "Sherpa 2.2.8"
      , name = "sh228"
      } /\ green /\ dotted /\ square /\ nomore

let sh228fix =
      { path = "gridYoda/Merge_950033_Sh228_CSSevol.yoda"
      , title = "Sherpa 2.2.8 (\$b \\to bg\$ fix)"
      , name = "sh228fix"
      } /\ orange /\ dashed /\ triangle /\ nomore

let PlotRecord = { path : Text, title : Text, name : Text, color : Text, line : Text, marker : Text, extra : Text }

let Plot = List PlotRecord

let pythia
    : Plot
    = [ powpy8
      , aMCNLOPy8
      , powPy8_410470
      , powPy8_MECoff_grec
      ]

let pythiaa14s
    = [ powpy8
      , powpy8_fsrup
      , powpy8_fsrdown
      , powpy8_a14rb
      ]

let herwigs
    = [ powher704, powher713, powher716 ]

let sherpas
    = [ sh221, sh228, sh228fix ]

let gens
    = [ powpy8, powher704, sh221 ]

let pathstyle =
      λ(p : PlotRecord) →
        "'${p.path}:Name=${p.name}:Title=${p.title}:PlotOrder=0:LineColor=${p.color}:LineStyle=${p.line}:PolyMarker=${p.marker}:DotScale=1.5${p.extra}'"

let pathtitle = λ(p : PlotRecord) → "'${p.path}:${p.title}'"

let pvalue =
      λ(obs : Text) →
      λ(p : Text) →
      λ(ps : Plot) →
        let t =
              concatSep
                ''
                ${" "}\
                ${" "} ''
                (lmap PlotRecord Text pathtitle ps)

        in  ''
            python ext/pvalue.py /BFRAG/${obs} unfold/mcmc/data/${obs}mcmc.dat \
              ${t} \
              > unfold/interpret/${obs}/${p}.log \
              2>&1
            ''

let plot =
      λ(c : Optional Text) →
      λ(p : Text) →
      λ(ps : Plot) →
        let t =
              concatSep
                ''
                ${" "}\
                ${" "} ''
                (lmap PlotRecord Text pathstyle ps)

        in  ''
            rivet-mkhtml --pwd --font helvetica --single \
              -m "/BFRAG/(rho|zbtc|zblc|nsvtrk)$" \
              -c ext/htop.plot ${Prelude.Optional.default Text "" c} \
              ${t} \
              -o unfold/interpret/${p}.plots
            ''


let getName = \(x : PlotRecord) -> x.name

let config = \(t : Text) -> \(p : Plot) -> \(main : Bool) ->
        ''
        cat << EOF > plot/${t}.plot
        # BEGIN PLOT /BFRAG/.*

        ${if main then "MainPlot=1" else "MainPlot=0\nRatioPlot1Legend=1\nLegendXPos=0.1\nRatioPlot1YMax=2.5"}

        RatioPlot=0
        RatioPlot1=1
        RatioPlot1Reference=data
        RatioPlot1ErrorBandColor={[cmyk]{0,0,0,0.15}}

        # END PLOT
        EOF
        ''

let setup =
      ''
      yodamerge -o BFRAG.yoda unfold/particlelevel/data/unfold*/htop.yoda
      yodacnv -m "(rho|zbtc|zblc|nsvtrk)norm$" BFRAG.yoda BFRAG.yoda

      perl -p -i -e "s/htop.*jets/BFRAG/g" BFRAG.yoda
      perl -p -i -e "s/norm//g" BFRAG.yoda
      ''


in  Prelude.Text.concatSep
      "\n\n"
      [ setup
      -- , pvalue "rho" "pythia" pythia
      -- , pvalue "rho" "pythiaA14" pythiaa14s
      -- , pvalue "rho" "herwig" herwigs
      -- , pvalue "rho" "sherpa" sherpas
      -- , config "pythiaA14ratio" ([data] # pythiaa14s) False
      -- , plot (Some "-c plot/pythiaA14ratio.plot") "pythiaA14ratio" ([data] # pythiaa14s)
      , plot (None Text) "pythia" pythia -- ([data] # pythia)
      , plot (None Text) "pythiaA14" pythiaa14s -- ([data] # pythiaa14s)
      , plot (None Text) "herwig" herwigs -- ([data] # herwigs)
      , plot (None Text) "sherpa" sherpas -- ([data] # sherpas)
      , plot (None Text) "generators" gens -- ([data] # gens)
      , "rm -f BFRAG.yoda\n"
      ]
