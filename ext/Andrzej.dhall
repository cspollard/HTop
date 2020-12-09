let Prelude =
      https://prelude.dhall-lang.org/package.dhall sha256:eb693342eb769f782174157eba9b5924cf8ac6793897fc36a31ccbd6f56dafe2

let concatSep = Prelude.Text.concatSep

let lmap = Prelude.List.map

let default = Prelude.Optional.default

let Line = < Solid | Dotted | Dashed | DashDotted | LongDashed >

let PlotRecord =
      { path : Text
      , title : Text
      , name : Text
      , color : Text
      , line : Optional Line
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

let solid = { line = Some Line.Solid }

let dotted = { line = Some Line.Dotted }

let longdashed = { line = Some Line.LongDashed }

let dashed = { line = Some Line.Dashed }

let dashdotted = { line = Some Line.DashDotted } 

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
        , title = "Pow+Py8 \$r_B = 0.855 \\, \\alpha_S^{\\mathrm{FSR}} = 0.127\$"
        , name = "powpy8"
        }
      ∧ green
      ∧ solid
      ∧ nomarker
      ∧ nomore


let powher713 =
        { path = "gridYoda/Merge_411234_PH713.yoda"
        , title = "Pow+Her 7.1.3 + EvtGen"
        , name = "powher713"
        }
      ∧ red
      ∧ dashdotted
      ∧ nomarker
      ∧ nomore




let her721JLtune =
        { path = "fromAndrzej/H721_string_JLtune.yoda"
        , title = "Her 7.2.1 JLtune"
        , name = "her721JLtune"
        }
      ∧ green
      ∧ solid
      ∧ nomarker
      ∧ nomore


let her721MAtune1 =
        { path = "fromAndrzej/H721_string_MAtune1.yoda"
        , title = "Her 7.2.1 MAtune1"
        , name = "her721MAtune1"
        }
      ∧ red
      ∧ solid
      ∧ nomarker
      ∧ nomore


let her721MAtune2 =
        { path = "fromAndrzej/H721_string_MAtune2.yoda"
        , title = "Her 7.2.1 MAtune2"
        , name = "her721MAtune2"
        }
      ∧ orange
      ∧ solid
      ∧ nomarker
      ∧ nomore


let her721 =
        { path = "fromAndrzej/H721_original.yoda"
        , title = "Her 7.2.1"
        , name = "her721"
        }
      ∧ blue
      ∧ solid
      ∧ nomarker
      ∧ nomore


let her715 =
        { path = "fromAndrzej/H715_original.yoda"
        , title = "Her 7.1.5"
        , name = "her715"
        }
      ∧ green
      ∧ dashed
      ∧ nomarker
      ∧ nomore

let her715JLtune =
        { path = "fromAndrzej/H715_string_JLtune.yoda"
        , title = "Her 7.1.5 JLtune"
        , name = "her715JLtune"
        }
      ∧ red
      ∧ dashed
      ∧ nomarker
      ∧ nomore



let her721s = [ her721, her721JLtune, her721MAtune1, her721MAtune2 ]
let her715s = [ her715, her715JLtune ]
let hers = [ powher713, her715, her721 ]

let maybe : forall (a : Type) -> forall (b : Type) -> (a -> b) -> b -> Optional a -> b =
      \(a : Type) ->
      \(b : Type) ->
      \(f : a -> b) ->
      \(def : b) ->
      \(o : Optional a) -> 
       merge { Some = \(y : a) -> f y, None = def } o

let linestyle : Line -> Text =
      \(l : Line) ->
        merge
        { Solid = ":LineStyle=solid"
        , Dotted = ":LineStyle=dotted"
        , Dashed = ":LineStyle=dashed"
        , DashDotted = ":LineStyle=dashdotted"
        , LongDashed = ":LineStyle=dashed:LineDash=10pt"
        }
        l

let markerstyle : Optional Text -> Text =
      \(o : Optional Text) ->
        merge { Some = \(t : Text) -> ":PolyMarker=${t}:DotScale=1.5", None = "" } o


let pathstyle =
      λ(p : PlotRecord) →
        "'${p.path}:Name=${p.name}:Title=${p.title}:PlotOrder=0:LineColor=${p.color}"
        ++ maybe Line Text linestyle "" p.line
        ++ "${markerstyle p.marker}${default Text "" p.extra}'"

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

        # BEGIN PLOT /BFRAG/nsvtrk

        ${if    main
          then  ""
          else  ''
                RatioPlotYMax=1.75
                RatioPlotYMin=0.75
                ''}

        # END PLOT


        # BEGIN PLOT /BFRAG/rho

        ${if    main
          then  ""
          else  ''
                RatioPlotYMax=1.5
                RatioPlotYMin=0.7
                ''}

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
      , config "hersratio" ([ data ] # hers) False
      , plot
          (Some "-c plot/hersratio.plot")
          "hersratio"
          ([ data ] # hers)
      , config "hers" ([ data ] # hers) True
      , plot (Some "-c plot/hers.plot") "hers" ([ data ] # hers)
      , config "her715sratio" ([ data ] # her715s) False
      , plot
          (Some "-c plot/her715sratio.plot")
          "her715sratio"
          ([ data ] # her715s)
      , config "her715s" ([ data ] # her715s) True
      , plot (Some "-c plot/her715s.plot") "her715s" ([ data ] # her715s)
      , config "her721sratio" ([ data ] # her721s) False
      , plot
          (Some "-c plot/her721sratio.plot")
          "her721sratio"
          ([ data ] # her721s)
      , config "her721s" ([ data ] # her721s) True
      , plot (Some "-c plot/her721s.plot") "her721s" ([ data ] # her721s)
      , ''
        rm -f BFRAGDATA.yoda
        ''
      ]
