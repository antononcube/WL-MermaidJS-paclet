BeginPackage["AntonAntonov`MermaidJS`"];

MermaidJS::usage = "Returns an image corresponding to a mermaid-js specification via the command line interface of mermaid-js";

MermaidInk::usage = "Get an image corresponding to a Mermaid-js specification via the web Mermaid-ink interface of Mermaid-js";

Begin["`Private`"];

(*********************************************************)
(* MermaidJS                                             *)
(*********************************************************)
Clear[MermaidJS];

MermaidJS::args =
    "The first argument is expected to be a string or Graph object; the second argument is expected to be one of \"PNG\", \"PDF\", \"SVG\", or Automatic.";

MermaidJS::nsvg =
    "Using PDF instead of SVG. (SVGImport does not work on mmdc generated files.)";

MermaidJS::nfmt = "Unknown format; continuing with PNG.";

MermaidJS::nmo =
    "The value of the option MermaidOptions is expected to be a string.";

MermaidJS::nmd =
    "The value of the option MermaidDirectives is expected to be a string.";

MermaidJS::nsp =
    "The value of the option SessionProlog is expected to be a string.";

MermaidJS::nss =
    "The value of the option ShellSession is expected to be a \"Shell\", Automatic, or ExternalSessionObject.";

Options[MermaidJS] =
    Join[{"MermaidOptions" -> "--pdfFit", "MermaidDirectives" -> "TD",
      "ShellSession" -> Automatic, "Prolog" -> "source ~/.zshrc"},
      Options[Graphics]];

MermaidJS[mSpec : (_String | _Graph), opts : OptionsPattern[]] :=
    MermaidJS[mSpec, Automatic, opts];

Clear[GetShellSession];
GetShellSession[] :=
    Block[{lsSS = Select[ExternalSessions[], #["System"] == "Shell" &]},
      If[Length[lsSS] == 0, "Shell", First[lsSS]]];

MermaidJS[mSpec_String, typeArg : (_String | Automatic),
  opts : OptionsPattern[]] :=
    Block[{type = typeArg, res, fname, command, mmdOpts, sessionProlog,
      shellSession, in},

      mmdOpts = OptionValue[MermaidJS, "MermaidOptions"];
      If[! StringQ[mmdOpts],
        ResourceFunction["ResourceFunctionMessage"][MermaidJS::nmo];
        mmdOpts = "";
      ];

      If[TrueQ[type === Automatic], type = "png"];

      If[! (StringQ[type] && MemberQ[{"png", "svg", "pdf"}, ToLowerCase[type]]),
        ResourceFunction["ResourceFunctionMessage"][MermaidJS::nfmt];
        type = "png"
      ];

      type = ToLowerCase[type];

      If[type == "svg",
        ResourceFunction["ResourceFunctionMessage"][MermaidJS::nsvg];
        type = "pdf"
      ];

      sessionProlog = OptionValue[MermaidJS, "Prolog"];
      If[! StringQ[sessionProlog],
        ResourceFunction["ResourceFunctionMessage"][MermaidJS::nsp];
        sessionProlog = "";
      ];

      shellSession = OptionValue[MermaidJS, "ShellSession"];
      If[! (shellSession == "Shell" ||
          TrueQ[Head[shellSession] === ExternalSessionObject] ||
          TrueQ[shellSession === Automatic]),
        ResourceFunction["ResourceFunctionMessage"][MermaidJS::nss];
        shellSession = "Shell"
      ];
      If[TrueQ[shellSession === Automatic], shellSession = GetShellSession[]];

      in = FileNameJoin[{$TemporaryDirectory, "mmdc-in.mmd"}]; 
      Export[in, mSpec, "String", CharacterEncoding -> "UTF-8"]; 

      fname = FileNameJoin[{$TemporaryDirectory, "mmdc-out." <> type}];
      command = "mmdc -i " <> in <> " -o " <> fname <> " " <> mmdOpts; 
      res = ExternalEvaluate[<|"System" -> shellSession|>, command]; 
      (*res=ExternalEvaluate[shellSession,command];*)

      Which[
        type == "pdf",
        res = Import[fname, "PageGraphics"][[1]];
        res = DeleteCases[res, HoldPattern[(ImageSize -> _) | (PlotRange -> _)]];
        res =
            DeleteCases[res, Style[_, FaceForm[RGBColor[1.`, 1.`, 1.`, 1.`]]],
              Infinity];
        Graphics[res[[1]], FilterRules[{opts}, Options[Graphics]]],

        type == "svg",
        ResourceFunction["SVGImport"][fname],

        True,
        Import[fname]
      ]
    ];

MermaidJS[gr_Graph, type : (_String | Automatic), opts : OptionsPattern[]] :=
    Block[{spec = "graph", lsEdges, lsEdges2, directives},

      directives = OptionValue[MermaidJS, "MermaidDirectives"];
      If[! StringQ[directives],
        ResourceFunction["ResourceFunctionMessage"][MermaidJS::nmd];
        directives = "";
      ];

      lsEdges = EdgeList[gr];
      lsEdges2 = lsEdges /. {
        DirectedEdge[x_, y_] :> ToString[x] <> " --> " <> ToString[y],
        DirectedEdge[x_, y_, t_] :>
            ToString[x] <> " --> |" <> ToString[t] <> "|" <> ToString[y],
        UndirectedEdge[x_, y_] :> ToString[x] <> " --- " <> ToString[y],
        UndirectedEdge[x_, y_, t_] :>
            ToString[x] <> " --- |" <> ToString[t] <> "|" <> ToString[y]
      };
      spec =
          spec <> " " <> directives <> "\n" <>
              StringRiffle[Map["  " <> # &, lsEdges2], "\n"];
      MermaidJS[spec, type, opts]
    ];

MermaidJS[___] := (ResourceFunction["ResourceFunctionMessage"][
  MermaidJS::args]; $Failed);

(*********************************************************)
(* MermaidInk                                            *)
(*********************************************************)

Clear[MermaidInk];

MermaidInk::args =
    "The first argument is expected to be a string or Graph object; the second argument is expected to be one of \"PNG\", \"PDF\", \"SVG\", or Automatic.";

MermaidInk::nmd =
    "The value of the option MermaidDirectives is expected to be a string.";

Options[MermaidInk] = {"URL" -> "https://mermaid.ink/img", "MermaidDirectives" -> "TD"}

MermaidInk[mSpec_String, opts : OptionsPattern[]] :=
    Block[{res, url},
      url = OptionValue[MermaidInk, "URL"];
      res = ExportString[StringTrim[mSpec], "Base64"];
      Import[URLBuild[{url, res}]]
    ];

MermaidInk[gr_Graph, opts : OptionsPattern[]] :=
    Block[{spec = "graph", lsEdges, lsEdges2, directives},

      directives = OptionValue[MermaidInk, "MermaidDirectives"];
      If[! StringQ[directives],
        ResourceFunction["ResourceFunctionMessage"][MermaidInk::nmd];
        directives = "";
      ];

      lsEdges = EdgeList[gr];
      lsEdges2 = lsEdges /. {
        DirectedEdge[x_, y_] :> ToString[x] <> " --> " <> ToString[y],
        DirectedEdge[x_, y_, t_] :>
            ToString[x] <> " --> |" <> ToString[t] <> "|" <> ToString[y],
        UndirectedEdge[x_, y_] :>
            ToString[x] <> " --- " <> ToString[y],
        UndirectedEdge[x_, y_, t_] :>
            ToString[x] <> " --- |" <> ToString[t] <> "|" <> ToString[y]
      };
      spec =
          spec <> " " <> directives <> "\n" <>
              StringRiffle[Map["  " <> # &, lsEdges2], "\n"];
      MermaidInk[spec, opts]
    ];

MermaidInk[___] := (ResourceFunction["ResourceFunctionMessage"][MermaidInk::args]; $Failed);

End[];
EndPackage[];