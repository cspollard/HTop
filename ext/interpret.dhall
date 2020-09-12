let Prelude =
      https://prelude.dhall-lang.org/package.dhall sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e

let concatSep = Prelude.Text.concatSep

let lmap = Prelude.List.map

let default = Prelude.Optional.default

let PlotRecord =
      { path : Text
      , title : Text
      , name : Text
      , color : Text
      , line : Optional Text
      , marker : Optional Text
      , extra : Optional Text
      }

let Plot = List PlotRecord

let black = { color = "black" }

let red = { color = "red" }

let blue = { color = "blue" }

let green = { color = "green" }

let orange = { color = "orange" }

let noline = { line = None Text }

let solid = { line = Some "solid" }

let dotted = { line = Some "dotted" }

let dashed = { line = Some "dashed" }

let dashdotted = { line = Some "dashdotted" } 

let dot = { marker = Some "*" }

let asterisk = { marker = Some "asterisk" }

let diamond = { marker = Some "diamond*" }

let triangle = { marker = Some "triangle*" }

let square = { marker = Some "square*" }

let nomarker = { marker = None Text }

let extra = λ(t : Text) → { extra = Some t }

let nomore = { extra = None Text }

let data =
        { path = "BFRAGDATA.yoda", title = "Data", name = "DATA" }
      ∧ black
      ∧ solid
      ∧ dot
      ∧ extra ":ConnectBins=0:ErrorBars=1:HorizLine=0"

let powpy8 =
        { path = "gridYoda/Merge_410503_PhPy8.yoda"
        , title = "Pow+Py8 \$r_B = 0.855 \\, \\alpha_S = 0.127\$"
        , name = "powpy8"
        }
      ∧ green
      ∧ solid
      ∧ nomarker
      ∧ nomore

let aMCNLOPy8 =
        { path = "gridYoda/Merge_410465_aMcAtNloPy8.yoda"
        , title = "aMC@NLO+Pythia8"
        , name = "aMCNLOPy8"
        }
      ∧ green
      ∧ dotted
      ∧ nomarker
      ∧ nomore

let powPy8_410470 =
        { path = "gridYoda/Merge_410470_PhPy8.yoda"
        , title = "Pow+Py8 (410470)"
        , name = "powPy8_410470"
        }
      ∧ green
      ∧ dashed
      ∧ nomarker
      ∧ nomore

let powPy8_MECoff_grec =
        { path = "gridYoda/Merge_411360_PP8_MECoff_grec.yoda"
        , title = "Pow+Py8 MECoff_grec"
        , name = "powPy8_MECoff_grec"
        }
      ∧ green
      ∧ dashdotted
      ∧ nomarker
      ∧ nomore

let powpy8_fsrup =
        { path = "gridYoda/Merge_410028_V2Up.yoda"
        , title = "Pow+Py8 \$r_B = 0.855 \\, \\alpha_s = 0.139\$"
        , name = "powpy8_fsrup"
        }
      ∧ green
      ∧ dotted
      ∧ nomarker
      ∧ nomore

let powpy8_fsrdown =
        { path = "gridYoda/Merge_410029_V2Down.yoda"
        , title = "Pow+Py8 \$r_B = 0.855 \\, \\alpha_s = 0.111\$"
        , name = "powpy8_fsrdown"
        }
      ∧ green
      ∧ dashed
      ∧ nomarker
      ∧ nomore

let powpy8_a14rb =
        { path = "gridYoda/Merge_411291_PP8_rb1p05.yoda"
        , title = "Pow+Py8 \$r_B = 1.050 \\, \\alpha_s = 0.127\$"
        , name = "powpy8_a14rb"
        }
      ∧ green
      ∧ dashdotted
      ∧ nomarker
      ∧ nomore

let powher704 =
        { path = "gridYoda/Merge_PH704.yoda"
        , title = "Pow+Her 7.0.4"
        , name = "powher704"
        }
      ∧ blue
      ∧ dotted
      ∧ nomarker
      ∧ nomore

let powher713 =
        { path = "gridYoda/Merge_411234_PH713.yoda"
        , title = "Pow+Her 7.1.3"
        , name = "powher713"
        }
      ∧ blue
      ∧ dashdotted
      ∧ nomarker
      ∧ nomore

let powher716 =
        { path = "gridYoda/Merge_PH16.yoda"
        , title = "Pow+Her 7.1.6"
        , name = "powher716"
        }
      ∧ blue
      ∧ dashed
      ∧ nomarker
      ∧ nomore

let sh221 =
        { path = "gridYoda/Merge_410252_Sh221.yoda"
        , title = "Sherpa 2.2.1"
        , name = "sh221"
        }
      ∧ orange
      ∧ dashdotted
      ∧ diamond
      ∧ nomore

let sh228Zbb =
        { path = "gridYoda/Merge_950010_Sh228_HTprime.yoda"
        , title = "Sherpa 2.2.8 ($Z+bb$ tune)"
        , name = "sh228Zbb"
        }
      ∧ orange
      ∧ dotted
      ∧ nomarker
      ∧ nomore

let sh228 =
        { path = "gridYoda/Merge_950033_Sh228_CSSevol.yoda"
        , title = "Sherpa 2.2.8 defaults"
        , name = "sh228"
        }
      ∧ orange
      ∧ dashed
      ∧ nomarker
      ∧ nomore

let sh2210 =
        { path = "gridYoda/Merge_Sherpa2210.yoda"
        , title = "Sherpa 2.2.10"
        , name = "sh2210"
        }
      ∧ orange
      ∧ solid
      ∧ nomarker
      ∧ nomore : PlotRecord


let pythia
    : Plot
    = [ powpy8, aMCNLOPy8, powPy8_410470, powPy8_MECoff_grec ]

let pythiaa14s = [ powpy8, powpy8_fsrup, powpy8_fsrdown, powpy8_a14rb ]

let herwigs = [ powher704, powher713 ]

let sherpas = [ sh221, sh228Zbb, sh228, sh2210 ]

let gens = [ powpy8, powher713, sh2210 ]

let linestyle : Optional Text -> Text =
      \(o : Optional Text) ->
        merge { Some = \(t : Text) -> ":LineStyle=${t}", None = "" } o
        
let markerstyle : Optional Text -> Text =
      \(o : Optional Text) ->
        merge { Some = \(t : Text) -> ":PolyMarker=${t}:DotScale=1.5", None = "" } o


let pathstyle =
      λ(p : PlotRecord) →
        "'${p.path}:Name=${p.name}:Title=${p.title}:PlotOrder=0:LineColor=${p.color}${linestyle p.line}${markerstyle p.marker}${default Text "" p.extra}'"

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
            rivet-mkhtml --font helvetica --single \
              -m "/BFRAG/(rho|zbtc|zblc|nsvtrk)$" \
              -c ext/htop.plot ${Prelude.Optional.default Text "" c} \
              ${t} \
              -o unfold/interpret/${p}.plots
            ''

let getName = λ(x : PlotRecord) → x.name

let config =
      λ(t : Text) →
      λ(p : Plot) →
      λ(main : Bool) →
        ''
        cat << EOF > plot/${t}.plot
        # BEGIN PLOT /BFRAG/.*

        ${if    main
          then  "MainPlot=1"
          else  ''
                MainPlot=0
                RatioPlotLegend=1
                RatioPlotLegendXPos=0.1
                RatioPlotYMax=2.5
                RatioPlotYSize=10''}

        RatioPlot=1
        RatioPlotReference=DATA
        RatioPlotErrorBandColor={[cmyk]{0,0,0,0.15}}

        # END PLOT

        # BEGIN PLOT /BFRAG/rho

        ${if    main
          then  ""
          else  "RatioPlotYMax=2"}

        # END PLOT
        EOF
        ''

let setup =
      ''
      yodamerge -o BFRAGDATA.yoda unfold/particlelevel/data/unfold*/htop.yoda
      yodacnv -m "(rho|zbtc|zblc|nsvtrk)norm$" BFRAGDATA.yoda BFRAGDATA.yoda

      perl -p -i -e "s/htop.*jets/BFRAG/g" BFRAGDATA.yoda
      perl -p -i -e "s/norm//g" BFRAGDATA.yoda
      perl -p -i -e "s/\/REF//g" BFRAGDATA.yoda
      perl -p -i -e "s/IsRef:\ 1/IsRef: 0/g" BFRAGDATA.yoda
      ''

in  Prelude.Text.concatSep
      "\n\n"
      [ setup
      -- , config "pythiaA14ratio" ([ data ] # pythiaa14s) False
      -- , plot
      --     (Some "-c plot/pythiaA14ratio.plot")
      --     "pythiaA14ratio"
      --     ([ data ] # pythiaa14s)
      -- , config "sherparatio" ([ data ] # sherpas) False
      -- , plot
      --     (Some "-c plot/sherparatio.plot")
      --     "sherparatio"
      --     ([ data ] # sherpas)
      -- , config "herwigratio" ([ data ] # herwigs) False
      -- , plot
      --     (Some "-c plot/herwigratio.plot")
      --     "herwigratio"
      --     ([ data ] # herwigs)
      , plot (None Text) "generators" ([ data ] # gens)
      , plot (None Text) "pythiaA14" ([ data ] # pythiaa14s)
      , plot (None Text) "herwig" ([ data ] # herwigs)
      , plot (None Text) "sherpa" ([ data ] # sherpas)
      , ''
        rm -f BFRAGDATA.yoda
        ''
      ]
