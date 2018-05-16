(* ::Package:: *)

(* ::Input:: *)
(*(* PACKAGE.M*)
(* * Progetto d'esame di Matematica Computazionale + Calcolo Numerico e Software Didattico*)
(* * Corsi di laurea magistrale in Informatica e Matematica*)
(* * Anno accademico 2017/2018*)
(* *)
(* * Autori:
  Virginia Spaccessi,
  Federico Cappelli,
  Antonio Lategano,
  Salvatore Visaggi,
  Davide Montanari,
  Antonio Conteduca

*)
(* *   *)
(* **)
(* * Versione di sviluppo: Wolfram Mathematica 11.1*)
(* * Versione di testing: Wolfram Mathematica 11.1 (Windows), 11.0 (Windows) e 10.2 (Debian)*)
(* *)*)
(**)


BeginPackage[ "ProvaProgetto`"];
Unprotect["ProvaProgetto` *"] (* toglie temporaneamente la protezione per ridefinire le funzioni *)
ClearAll["ProvaProgetto` *"];

(* ::InheritFromParent:: *)
(**)
(*  TEORIA  *)
DisegnaCirconferenza::usage = "disegna circ goniometrica con funzioni di seno e coseno";
DisegnaCirconferenzaInit::usage = "grafico della circ goniometrica";
DisegnaPianoCartesiano::usage = "disegna assi";
TipoAngolo::usage = "per la def di angolo";
GraficoTangente::usage = "manipulate per la funzione tangente";
AngoliAssociati0::usage = "manipulate di angoli notevoli 0";
AngoliAssociati60::usage = "manipulate di angoli notevoli 60";
AngoliAssociati30::usage = "manipulate di angoli notevoli 30";
AngoliAssociati45::usage = "manipulate di angoli notevoli 45";
ThGradRad::usage = "manipulate della conversione radianti gradi";
GraficoPrimaRelazione::usage = "manipulate della prima relazione fondamentale";

(*  ESERCIZI  *)
EsCoordinate::usage = "esercizio su coordinate";
EsDistanze::usage = "calcola la distanza distanza tra due punti";
EsSinCos::usage = "esercizio sin cos";

(* APPLICAZIONI*)
AltezzaTorre::usage = "applicazione altezza torre";


(*DEFINIZIONI di GRADO e RADIANTE*)
defGradi = Text["Consideriamo un angolo giro e dividiamolo in 360 parti congruenti tra loro.
Poniamo 1\[Degree] una di queste parti. Preso dunque un angolo qualsiasi, contiamo quante
 volte un grado  \[EGrave]  contenuto in esso."];
defRadianti = Text["Prendiamo una circonferenza di raggio r e un angolo al centro \[Alpha];
l  \[EGrave]  l'arco sotteso dall'angolo. La misura in radianti  \[EGrave]  data dal rapporto l/r.
 In particolare, 1 rad  \[EGrave]  un angolo che sottende un arco lungo quanto il raggio."];

Begin["`Private`"]; (* Comincia spazio privato *)

(*FUNZIONI E STRUTTURE DATI AUX*)
FromRadToGrad[x_] := Return[x * 180 / Pi];
FromGradToRad[x_] := Pi Rationalize[N[x]] / Pi;
getAngle[p1_, p2_] := Mod[ArcTan @@ (p2 - p1), 2 Pi];
pointList = List[{3, 2}, {1, 3}, {-1, 1}, {-2, -5}, {-3, 8}, {-1, 3}, {-2, 1}, {-7, 3}];
pointListTwo = List[{2, 0}, {-9, 5}, {-10, 7}, {5, 2}, {-5, 4}, {-8, -5}, {2, 9}, {3, -6}];
angleList = List[0, 30, 45, 60, 90, 120, 135, 150, 180, 210, 225, 240, 270, 300, 315, 330, 360];
sinCosList = List[0, 1, -1, 1 / 2, -1 / 2, Sqrt[2] / 2, -Sqrt[2] / 2, Sqrt[3] / 2, -Sqrt[3] / 2];

(* ::InheritFromParent:: *)
(**)

DisegnaCirconferenzaInit[] := Graphics[{
  Circle[],
  Thickness[0.01],
  Text[Style["Raggio=1 ", 14], {0.6, 0.30}],
  Line[{{0, 0}, {Sqrt[2] / 2, Sqrt[2] / 2}}],

  Text[Style["A(0,1)", 15, Bold], {0.2, 1.1}],
  {Red, PointSize -> 0.02, Point[{0, 1}]},
  {Red, PointSize -> 0.02, Point[{Sqrt[2] / 2, Sqrt[2] / 2}]},

  Text[Style["B(1,0)", 15, Bold], {1.2, 0.1}],
  {Red, PointSize -> 0.02, Point[{1, 0}]}
},
  Axes -> True,
  Ticks -> False,
  PlotRange -> {{-1, 1.4}, {-1, 1.4}}
];

EsSinCos[] := DynamicModule[{esitoSin = "", colorEsitoSin = "", esitoCos = "", colorEsitoCos = ""},
  checkRisp[risp_, correct_] := (
    If[risp == correct, Return["Risposta Corretta"], Return["Risposta Sbagliata"]]
  );
  Row[{
    random = RandomSample[angleList, 1];
    x = random[[1]] Degree;
    sinCosList2 = DeleteCases[sinCosList, Cos[x]];
    sinCosList2 = DeleteCases[sinCosList2, Sin[x]];

    sin = Sin[x];
    cos = Cos[x];

    Graphics[{
      Circle[],
      Point[{0, 0}],
      PointSize[Large],
      Circle[{0, 0}, 0.2, {0, FromGradToRad[x]}],
      {Dashed, Line[{{0, 0}, {cos, sin}}]}, (*raggio*)
      Thickness[0.01],
      RGBColor[0, 255, 0],
      Line[{{0, 0}, {cos, 0}}],
      RGBColor[255, 0, 0],
      Line[{ {cos, 0}, {cos, sin} }],
      Text[Style[IntegerString[random[[1]]] <> "\[Degree]", Large, Red], {-0.90, 1}],
      {Black, PointSize -> .02, Point[{cos, sin}]}},
      ImageSize -> 350,
      Axes -> True,
      Ticks -> None
    ] (*FINE GRAPHICS*),
    Row[{
      Column[{
        incorrects = RandomSample[sinCosList2, 3];

        btn1 = Button[Style[Rationalize[Sin[x]], 15], {esitoSin := "Corretto!", colorEsitoSin := Green}];
        btn2 = Button[Style[Rationalize[incorrects[[1]]], 15], {esitoSin := "Sbagliato!", colorEsitoSin := Red}];
        btn3 = Button[Style[Rationalize[incorrects[[2]]], 15], {esitoSin := "Sbagliato!", colorEsitoSin := Red}];
        btn4 = Button[Style[Rationalize[incorrects[[3]]], 15], {esitoSin := "Sbagliato!", colorEsitoSin := Red}];

        buttons = List[btn1, btn2, btn3, btn4];
        randomButtons = RandomSample[buttons];

        Dynamic[Text[Style[esitoSin, colorEsitoSin, 15]]],
        Style["Seleziona il valore del seno:", 15],
        randomButtons[[1]],
        randomButtons[[2]],
        randomButtons[[3]],
        randomButtons[[4]]
      }],
      Text["\t"],
      Column[{
        incorrects = RandomSample[sinCosList2, 3];

        btn5 = Button[Style[Rationalize[Cos[x]], 15], {esitoCos := "Corretto!", colorEsitoCos := Green}];
        btn6 = Button[Style[Rationalize[incorrects[[1]]], 15], {esitoCos := "Sbagliato!", colorEsitoCos := Red}];
        btn7 = Button[Style[Rationalize[incorrects[[2]]], 15], {esitoCos := "Sbagliato!", colorEsitoCos := Red}];
        btn8 = Button[Style[Rationalize[incorrects[[3]]], 15], {esitoCos := "Sbagliato!", colorEsitoCos := Red}];

        buttons = List[btn5, btn6, btn7, btn8];
        randomButtons = RandomSample[buttons];

        Dynamic[Text[Style[esitoCos, colorEsitoCos, 15]]],
        Style["Seleziona il valore del coseno:", 15],
        randomButtons[[1]],
        randomButtons[[2]],
        randomButtons[[3]],
        randomButtons[[4]]
      }]
    }]
  }]
];

(*Calcolo della distanza tra due punti attraverso le coordinate*)
computeDistance[x1_, y1_, x2_, y2_] := Return[Sqrt[(x2 - x1)^2 + (y2 - y1)^2]];

TipoAngolo[] := Manipulate[
  Row[{
    Column[{
      Row[{ Button["Nullo", a = 0], "\t",
        Button["Retto", a = 90], "\t",
        Button["Piatto", a = 180], "\t",
        Button["Giro", a = 360], "\n"}],

      Graphics[{
        Circle[],
        x := N[Cos[a Degree]];,
        y := N[Sin[a Degree]];,
        {Blue, Thick, Circle[{0, 0}, 0.4, {0, a Pi / 180}]},
      (*Text[Style[a, 15, Blue], {-0.7, 1.2}],*)
        Text[Style[a \[Degree], 15, Blue], {-1, 1}],
        Text[Style[a Pi / 180, 18, RGBColor[0, 50, 255]], {+ 1, 1}],

        Thickness[0.02],
        Line[{{0, 0}, {x, y}}]
      }, Axes -> True, ImageSize -> 350, Ticks -> None, PlotRange -> {{-1.4, 1.4}, {-1.4, 1.4}}
      ]
    }] (*fine colonna grafico*),
    Column[{"\t"}],
    Column[{
      Row[{Text[Style["GRADI", Blue, Bold, 20]], "\n", Text[Style[defGradi, 17]]}],
      Row[{"\n"}],
      Row[{Text[Style["RADIANTI", RGBColor[0, 50, 255], Bold, 20]], "\n", Text[Style[defRadianti, 17]]}]
    }]
  }](*fine row*),
  {{a, 0, "angolo"}, 0, 360, 1}];

ThGradRad[] := Manipulate[
  Row[{
    Column[{
      Row[{
        "Angoli notevoli (\[Degree]): ",
        PopupMenu[Dynamic[gradi], {0, 30, 45, 60, 90, 120, 135, 150, 180, 210, 225, 240, 270, 300, 315, 330, 360}],
        "\n\n",
        Style[gradi "\[Degree]", 20, Blue],
        "\t:\t",
        Style[gradi Pi / 180, 20, RGBColor[0, 50, 255]],
        "\t=\t",
        "360\t:\t",
        HoldForm[2 Pi],
        "\n\n\n",
        Style["Gradi", Blue], "\n",
        Style["Radianti", RGBColor[0, 50, 255]]

      }]  }], "\t",
    Column[{
      Style["Se Alpha \[EGrave] l'angolo di cui vogliamo conoscere la misura, vale la seguente formula:", 15],
      Panel[
        Image[Import["C:\\Users\\anton\\Documents\\GitHub\\mathematica_computazionale\\assets\\proporzioneGradRad.png"], ImageSize -> 200],
        ImageSize -> 600,
        Alignment -> {Center, Center}
      ]


    }]


  }](*fine row*),
  {{gradi, 0, "Gradi"}, 0, 360, 1}
];

GraficoPrimaRelazione[] := Animate[ Row[{
(*realativo angolo in radianti*)
  rad := x Pi / 180;
  P := {Cos[rad], Sin[rad]};
  proiezioneX := {Cos[rad], 0};
  proiezioneY := {Cos[rad], Sin[rad]};
  triangolo := {Yellow, Triangle[{{0, 0}, proiezioneX, P}]},
  Graphics[{
    Circle[],
    triangolo,
  (*punto centrale alla circonferenza*)
    Point[{0, 0}],
  (*dimentsione dei punti*)
    PointSize[Large],
  (**arco che identifica l'arco*)
  (*{Blue, Circle[{0, 0}, 0.2, {0, rad}]},*)
    {Dashed, Line[{{0, 0}, P}]}, (*raggio*)
    Thickness[0.01],
    RGBColor[0, 255, 0],
  (*proiezione asse x*)
    Line[{{0, 0}, proiezioneX }],
    RGBColor[255, 0, 0],
  (*proiezione asse y*)
    Line[{{Cos[rad], 0}, proiezioneY}],
  (*punto sulla circonferenza*)
    {Black, PointSize -> .02, Point[{Cos[rad], Sin[rad]}]}},
    ImageSize -> 350,
    Axes -> True,
    Ticks -> None
  ] (*FINE GRAPHICS*)
}],
  {{x, 45, ""}, 0, 360, 1},
  AnimationRate -> 30
];


GraficoTangente[] := Manipulate[
  Row[{
    Graphics[{
      Circle[],
      Point[{0, 0}],
      PointSize[Large],
      {Blue, Circle[{0, 0}, 0.2, {0, x}]},
      {Dashed, Line[{{0, 0}, {Cos[x], Sin[x]}}]}, (*raggio*)
      Thickness[0.01],
      RGBColor[0, 255, 0],
      Line[{{0, 0}, {Cos[x], 0}}],
      RGBColor[255, 0, 0],
      Line[{{Cos[x], 0}, {Cos[x], Sin[x]}}],
      Text[Style[IntegerString[FromRadToGrad[x]] <> "\[Degree]", 15, Blue], {-0.90, 1}],
      Text[Style[x, 15, Blue], {-0.90, 0.8}],
      Text[Style["Seno", Medium, Red], {0.8, 1}],
      Text[Style["Coseno", Medium, Green], {0.8, 0.9}],

      {Black, PointSize -> .02, Point[{Cos[x], Sin[x]}]}},
      ImageSize -> 350,
      Axes -> True,
      Ticks -> None
    ], (*FINE GRAPHICS*)
    Column[{
      Show[
        Plot[Tan[y], {y, 0, 2 Pi},
          ImageSize -> 400,
          Ticks -> {{0, Pi / 2, Pi, 3 Pi / 2, 2 Pi, 5 Pi / 2}, {-1, 0, 1}},
          PlotLabel -> "Funzione Tangente",
          PlotStyle -> RGBColor[1, 0, 1],
          PlotRange -> {{-1, 2 Pi + 1}, {-4, 4}}
        ],
        Graphics[{
        (*Trick per evitare il valore infinito per x = 90 e x = 270 *)
          PointSize[Large],
          If[x == Pi / 2 || x == 3 Pi / 2,
            {
              {Dashed, Line[{{x, -10}, {x, 10}}]},
              Text[Style["Infinito", 20], {x + 0.5, 0.5 + Sin[x]}]

            },
            {
              {Dashed, Line[{{x, 0}, {x, Tan[x]}}]},
              Point[{ x, Tan[x] }],
              Text[Style[Tan[x], 20], {x + 0.5, 0.5 + Sin[x]}]

            }
          ]
        }]
      ]
    }]
  }],
  {{x, 0, "Naviga"}, 0, 2 Pi, Pi / 12},
  {{x, 0, "Scegli"}, 0, 2 Pi, Pi / 12},
  ControlType -> {Slider, PopupMenu}
];

DisegnaCirconferenza[] := Manipulate[
  Row[{
    Graphics[{
      Circle[],
      Point[{0, 0}],
      PointSize[Large],
      {Blue, Circle[{0, 0}, 0.2, {0, x}]},
      {Dashed, Line[{{0, 0}, {Cos[x], Sin[x]}}]}, (*raggio*)
      Thickness[0.01],
      RGBColor[0, 255, 0],
      Line[{{0, 0}, {Cos[x], 0}}],
      RGBColor[255, 0, 0],
      Line[{{Cos[x], 0}, {Cos[x], Sin[x]}}],
      Text[Style[IntegerString[FromRadToGrad[x]] <> "\[Degree]", 15, Blue], {-0.90, 1}],
      Text[Style[x, 15, Blue], {-0.90, 0.8}],
      Text[Style["Seno", Medium, Red], {0.8, 1}],
      Text[Style["Coseno", Medium, Green], {0.8, 0.9}],

      {Black, PointSize -> .02, Point[{Cos[x], Sin[x]}]}},
      ImageSize -> 350,
      Axes -> True,
      Ticks -> None
    ], (*FINE GRAPHICS*)
  (* GRAFICI SENO E COSENO *)
    Column[{
      Show[
        Plot[Sin[y], {y, 0, 2 Pi},
          ImageSize -> 300,
          Ticks -> {{0, Pi / 2, Pi, 3 Pi / 2, 2 Pi, 5 Pi / 2}, {-1, 0, 1}},
          PlotLabel -> "Funzione Seno",
          PlotStyle -> {Red},
          PlotRange -> {{-1, 2 Pi + 1}, {-1.8, 1.8}}
        ],
        Graphics[{
          {Dashed, Line[{{x, 0}, {x, Sin[x]}}]},
          PointSize[Large],
          Text[Style[Rationalize[Sin[x]], 20], {x + 0.5, 0.5 + Sin[x]}],
          Point[{ x, Sin[x] }]}
        ]
      ],
      Show[
        Plot[Cos[y], {y, 0, 2 Pi},
          ImageSize -> 300,
          Ticks -> {{0, Pi / 2, Pi, 3 Pi / 2, 2 Pi, 5 Pi / 2}, {-1, 0, 1}},
          PlotLabel -> "Funzione Coseno",
          PlotStyle -> {Green},
          PlotRange -> {{-1, 2 Pi + 1}, {-1.8, 1.8}}
        ],
        Graphics[{
          {Dashed, Line[{{x, 0}, {x, Cos[x]}}]},
          PointSize[Large],
          Text[Style[Cos[x], 20], {x + 0.5, 0.5 + Cos[x]}],
          Point[{ x, Cos[x]}]}
        ]
      ]
    }]
  }],
  {{x, 0, "Naviga"}, 0, 2 Pi, Pi / 12},
  {{x, 0, "Scegli"}, 0, 2 Pi, Pi / 12},
  ControlType -> {Slider, PopupMenu}
];

EsDistanze[] := Quiet[DynamicModule[{x1, y1, x2, y2, Esito = ""},
  checkRisp[risp_, correct_] := (
    If[risp == correct, Return["Risposta Corretta"], Return["Risposta Sbagliata"]]
  );
  random = RandomInteger[{1, Length[pointList]}];
  randomTwo = RandomInteger[{1, Length[pointListTwo]}];
  P1 = pointList[[random]];
  P2 = pointListTwo[[randomTwo]];
  x1 = P1[[1]];
  x2 = P2[[1]];
  y1 = P1[[2]];
  y2 = P2[[2]];
  Row[{
    Column[{
      Graphics[{
        {Thickness[0.0050], Blue, Dashed, Line[{{x1, 0}, {x1, y1}}]},
        {Thickness[0.0050], Blue, Dashed, Line[{{0, y1}, {x1, y1}}]},
        {Thickness[0.0050], Blue, Dashed, Line[{{x2, 0}, {x2, y2}}]},
        {Thickness[0.0050], Blue, Dashed, Line[{{0, y2}, {x2, y2}}]},
        Text[Style["A", 20], {x1 - 0.4, y1 - 0.4}],
        Text[Style["B", 20], {x2 + 0.4, y2 + 0.4}],
        {PointSize[Large], Red, Point[{x1, y1}]},
        {PointSize[Large], Red, Point[{x2, y2}]},
        {Thickness[0.0050], Green, Line[{{x1, y1}, {x2, y2}}]}
      },
        GridLines -> {Table[point, {point, -10, 10, 1}], Table[point, {point, -10, 10, 1}]},
        Axes -> True,
        AxesStyle -> Thick, PlotRange -> 10,
        ImageSize -> 450,
        AxesLabel -> {"x", "y"},
        Ticks -> {
          Table[i, {i, -10, 10, 1}],
          Table[i, {i, -10, 10, 1}]
        }
      ](* Fine Graphics *)
    }], (*fine colonna grafico*)
    Column[{

      Style[Dynamic[Esito], 20],
      Row[{
        Button["Aiuto",
          CreateDialog[{
            TextCell["La distanza tra due punti si calcola nel seguente modo: "HoldForm[Sqrt[("xA" - "xB")^2 + ("yA" - "yB")^2]]],
            DefaultButton[]
          }],
          ImageSize -> Medium, BaseStyle -> {"GenericButton"}
        ],
        Button["Nuovo Esercizio", FrontEndExecute[FrontEndToken[NotebookLocate["esDistanza"], "Evaluate"]], ImageSize -> Medium, BaseStyle -> {"GenericButton"}]
      }],
      If[x1 < 0, SigX1 = "-", SigX1 = ""];
      If[y1 < 0, SigY1 = "-", SigY1 = ""];
      If[x2 < 0, SigX2 = "-", SigX2 = ""];
      If[y2 < 0, SigY2 = "-", SigY2 = ""];
      A = "(" <> SigX1 <> IntegerString[x1] <> "," <> SigY1 <> IntegerString[y1] <> ")";
      B = "(" <> SigX2 <> IntegerString[x2] <> "," <> SigY2 <> IntegerString[y2] <> ")";
      Style["Quanto distano tra di loro i punti A" <> A <> " e B" <> B <> ":", 20],
    (*calcolo della risposta corretta*)
      giusta = computeDistance[x1, y1, x2, y2];
      (*calcolo casuale delle risposte errate*)
      otherRisp = RandomSample[{Sqrt[37], Sqrt[76], 8 / 9, 5 / 8, 8 / 5, 2, 15 / 4, Sqrt[45], Sqrt[56], Sqrt[34], 4 / 80, 20 / 7}, 3];
      risp1 = otherRisp[[1]];
      risp2 = otherRisp[[2]];
      risp3 = otherRisp[[3]];
      (*la risp corretta viene inserita temporan nella var risp4 per poi
        inserire le 4 risposte e fare un Sort in base alla crescenza dei valori *)
      risp4 = giusta;
      Opzioni = {risp1, risp2, risp3, N[risp4]};
      RandomOptions = Sort[Opzioni];
      If[N[giusta] == RandomOptions[[1]], RandomOptions[[1]] = giusta;],
      If[N[giusta] == RandomOptions[[2]], RandomOptions[[2]] = giusta; ],
      If[N[giusta] == RandomOptions[[3]], RandomOptions[[3]] = giusta;],
      If[N[giusta] == RandomOptions[[4]], RandomOptions[[4]] = giusta;],
    (* la risposta corretta \[EGrave] ordinata in base al suo valore *)
      btn1 = Button[Style[NumberForm[RandomOptions[[1]]], 20], Esito := checkRisp[RandomOptions[[1]], giusta]];
      btn2 = Button[Style[NumberForm[RandomOptions[[2]]], 20], Esito := checkRisp[RandomOptions[[2]], giusta]];
      btn3 = Button[Style[NumberForm[RandomOptions[[3]]], 20], Esito := checkRisp[RandomOptions[[3]], giusta]];
      btn4 = Button[Style[NumberForm[RandomOptions[[4]]], 20], Esito := checkRisp[RandomOptions[[4]], giusta]];
      (*Stampa dei bottoni*)
      btn1, btn2, btn3, btn4
    }] (*fine colonna input*)
  }]
]];



ListaAngoliAssociati60 := {\[Pi] / 3, 2 \[Pi] / 3, 4 \[Pi] / 3, 5 \[Pi] / 3};
AngoliAssociati60[] := Animate[
  Row[{
    Column[{
      Row[{
        Graphics[{
          angle := ListaAngoliAssociati60[[1]];
          Text[Style[angle 180 / Pi Degree, 15, Bold, Black], {1.2 Cos[angle], 1.2 Sin[angle]}],
          MapIndexed[{
            ColorData[100][0],
            ColorData[100][First@#2 + 50],
            Text[Style[Last@# 180 / Pi Degree, 15, Bold], {1.2 Cos[Last[#]], 1.2 Sin[Last[#]]}],
            Disk[{0, 0}, 1, #]}&,
            Take[Partition[ListaAngoliAssociati60, 2, 1], n]]},
          PlotRange -> {{-1.3, 1.3}, {-1.5, 1.5}}, ImageSize -> 400
        ],
        Column[{"\t\t"}],
        Column[{
          Lista = ListaAngoliAssociati60;
          AngoloRif = Lista[[1]];
          AngoloGrad = AngoloRif 180 / Pi Degree;
          Text[Style[StringForm["Cos(``) = ``, Sen(``) = ``", AngoloGrad, Cos[AngoloRif], AngoloGrad, Sin[AngoloRif]], 25, Bold]], "\n",

          Text[Style[StringForm["Cos(``) = -Cos(``)", Lista[[2]] 180 / Pi Degree, AngoloGrad], 20]],
          Text[Style[StringForm["Sen(``) = Sen(``)", Lista[[2]] 180 / Pi Degree, AngoloGrad], 20]], "\n",

          Text[Style[StringForm["Cos(``) = -Cos(``)", Lista[[3]] 180 / Pi Degree, AngoloGrad], 20]],
          Text[Style[StringForm["Sen(``) = -Sen(``)", Lista[[3]] 180 / Pi Degree, AngoloGrad], 20]], "\n",

          Text[Style[StringForm["Cos(``) = Cos(``)", Lista[[4]] 180 / Pi Degree, AngoloGrad], 20]],
          Text[Style[StringForm["Sen(``) = -Sen(``)", Lista[[4]] 180 / Pi Degree, AngoloGrad], 20]], "\n"
        }]

      }]
    }]
  }],
  {{n, 0, "Angolo"}, 0, 3, 1 }
];

ListaAngoliAssociati0 := {0, \[Pi] / 2, \[Pi], 3 \[Pi] / 2, 2 \[Pi]};
AngoliAssociati0[] := Animate[
  Row[{
    Column[{
      Row[{
        Graphics[{
          If[n == 5, val := 5;, val := 1;];
          angle := ListaAngoliAssociati0[[val]];
          If[n < 4, Text[Style[angle Degree, 15, Bold, Black], {1.2 Cos[angle], 1.2 Sin[angle]}]],
          MapIndexed[{
            ColorData[100][0],
            ColorData[100][First@#2 + 50],
            Text[Style[Last@# 180 / Pi, 15, Bold], {1.2 Cos[Last[#]], 1.2 Sin[Last[#]]}],
            Disk[{0, 0}, 1, #]}&,
            Take[Partition[ListaAngoliAssociati0, 2, 1], n]]},
          PlotRange -> {{-1.3, 1.3}, {-1.5, 1.5}}, ImageSize -> 400
        ],
        Column[{"\t\t"}],
        Column[{
          AngoloRif = ListaAngoliAssociati0[[1]];
          AngoloGrad = AngoloRif 180 / Pi Degree;
          Text[Style[StringForm["Cos(``) = ``, Sin(``) = ``", AngoloGrad, Cos[AngoloRif], AngoloGrad, Sin[AngoloRif]], 25, Bold]], "\n",
          Text[Style["Cos(180) = -Cos(0)\nSen(180) = Sen(0)", 20]], "\n",
          Text[Style["Cos(270) = Sen(0)\nSen(270) = -Cos(0)", 20]], "\n",
          Text[Style["Cos(360) = Cos(0)\nSen(360) = Cos(0)", 20]]

        }]

      }]
    }]
  }],
  {{n, 0, "Angolo"}, 0, 4, 1 }
];

ListaAngoliAssociati30 := {\[Pi] / 6, 5 \[Pi] / 6, 7 \[Pi] / 6, 11 \[Pi] / 6};
AngoliAssociati30[] := Animate[
  Row[{
    Column[{
      Row[{
        Graphics[{
          angle := ListaAngoliAssociati30[[1]];
          Text[Style[angle 180 / Pi Degree, 15, Bold, Black], {1.2 Cos[angle], 1.2 Sin[angle]}],
          MapIndexed[{
            ColorData[100][0],
            ColorData[100][First@#2 + 50],
            Text[Style[Last@# 180 / Pi Degree, 15, Bold], {1.2 Cos[Last[#]], 1.2 Sin[Last[#]]}],
            Disk[{0, 0}, 1, #]}&,
            Take[Partition[ListaAngoliAssociati30, 2, 1], n]]},
          PlotRange -> {{-1.3, 1.3}, {-1.5, 1.5}}, ImageSize -> 400
        ],
        Column[{"\t\t"}],
        Column[{
          Lista = ListaAngoliAssociati30;
          AngoloRif = Lista[[1]];
          AngoloGrad = AngoloRif 180 / Pi Degree;
          Text[Style[StringForm["Cos(``) = ``, Sen(``) = ``", AngoloGrad, Cos[AngoloRif], AngoloGrad, Sin[AngoloRif]], 25, Bold]], "\n",

          Text[Style[StringForm["Cos(``) = -Cos(``)", Lista[[2]] 180 / Pi Degree, AngoloGrad], 20]],
          Text[Style[StringForm["Sen(``) = Sen(``)", Lista[[2]] 180 / Pi Degree, AngoloGrad], 20]], "\n",

          Text[Style[StringForm["Cos(``) = -Cos(``)", Lista[[3]] 180 / Pi Degree, AngoloGrad], 20]],
          Text[Style[StringForm["Sen(``) = -Sen(``)", Lista[[3]] 180 / Pi Degree, AngoloGrad], 20]], "\n",

          Text[Style[StringForm["Cos(``) = Cos(``)", Lista[[4]] 180 / Pi Degree, AngoloGrad], 20]],
          Text[Style[StringForm["Sen(``) = -Sen(``)", Lista[[4]] 180 / Pi Degree, AngoloGrad], 20]], "\n"
        }]

      }]
    }]
  }],
  {{n, 0, "Angolo"}, 0, 3, 1 }
];

ListaAngoliAssociati45 := {\[Pi] / 4, 3 \[Pi] / 4, 5 \[Pi] / 4, 7 \[Pi] / 4};
AngoliAssociati45[] := Animate[
  Row[{
    Column[{
      Row[{
        Graphics[{
          angle := ListaAngoliAssociati45[[1]];
          Text[Style[angle 180 / Pi Degree, 15, Bold, Black], {1.2 Cos[angle], 1.2 Sin[angle]}],
          MapIndexed[{
            ColorData[100][0],
            ColorData[100][First@#2 + 50],
            Text[Style[Last@# 180 / Pi Degree, 15, Bold], {1.2 Cos[Last[#]], 1.2 Sin[Last[#]]}],
            Disk[{0, 0}, 1, #]}&,
            Take[Partition[ListaAngoliAssociati45, 2, 1], n]]},
          PlotRange -> {{-1.3, 1.3}, {-1.5, 1.5}}, ImageSize -> 400
        ],
        Column[{"\t\t"}],
        Column[{
          Lista = ListaAngoliAssociati45;
          AngoloRif = Lista[[1]];
          AngoloGrad = AngoloRif 180 / Pi Degree;
          Text[Style[StringForm["Cos(``) = ``, Sen(``) = ``", AngoloGrad, Cos[AngoloRif], AngoloGrad, Sin[AngoloRif]], 25, Bold]], "\n",

          Text[Style[StringForm["Cos(``) = -Cos(``)", Lista[[2]] 180 / Pi Degree, AngoloGrad], 20]],
          Text[Style[StringForm["Sen(``) = Sen(``)", Lista[[2]] 180 / Pi Degree, AngoloGrad], 20]], "\n",

          Text[Style[StringForm["Cos(``) = -Cos(``)", Lista[[3]] 180 / Pi Degree, AngoloGrad], 20]],
          Text[Style[StringForm["Sen(``) = -Sen(``)", Lista[[3]] 180 / Pi Degree, AngoloGrad], 20]], "\n",

          Text[Style[StringForm["Cos(``) = Cos(``)", Lista[[4]] 180 / Pi Degree, AngoloGrad], 20]],
          Text[Style[StringForm["Sen(``) = -Sen(``)", Lista[[4]] 180 / Pi Degree, AngoloGrad], 20]], "\n"
        }]

      }]
    }]
  }],
  {{n, 0, "Angolo"}, 0, 3, 1 }
];

DisegnaPianoCartesiano[x_, y_] := DynamicModule[{colEsito = Black, esito = "", limit = 10, xIn = 0, yIn = 0},
  Row[{
    q1 = {limit, limit};
    q2 = {-limit, limit};
    q3 = {-limit, -limit};
    q4 = {limit, -limit};
    Column[{
      Graphics[{
        {PointSize[Large], Red, Point[{x, y}]}
      },
        GridLines -> {Table[point, {point, -10, 10, 1}], Table[point, {point, -10, 10, 1}]},
        Axes -> True,
        AxesStyle -> Thick, PlotRange -> 10,
        ImageSize -> 450,
        AxesLabel -> {"x", "y"},
        Ticks -> {
          Table[i, {i, -limit, limit, 1}],
          Table[i, {i, -limit, limit, 1}]
        }
      ](* Fine Graphics *)
    }], (*Fine Column*)
    Column[{

      Row[{
        Button["Aiuto",
          CreateDialog[{
            TextCell["L'asse delle X \[EGrave] la retta posta in posizione orizzontale mentre l'asse delle Y \[EGrave] in posizione verticale"],
            DefaultButton[]
          }],
          ImageSize -> Medium, BaseStyle -> {"GenericButton"}
        ],

        Button["Nuovo Esercizio", FrontEndExecute[FrontEndToken[NotebookLocate["esCoordinate"], "Evaluate"]], ImageSize -> Medium, BaseStyle -> {"GenericButton"}]

      }],

      Dynamic[Text[Style[esito, colEsito, 20]]],
      "   x: "InputField[Dynamic[xIn], Number, FieldSize -> 5], "\n",
      "   y: "InputField[Dynamic[yIn], Number, FieldSize -> 5],


      Button["Risolvi",
        If[xIn == x && yIn == y,
        (*IF*)
          {esito := "Corretto"; colEsito := Green},
        (*ELSE *)
          {esito := "Sbagliato"; colEsito := Red }], BaseStyle -> {Green}
      ]
    }]
  }](*Fine Row*)
];
(*individue le coord del punto*)
EsCoordinate[] := Quiet[DynamicModule[{},
  pointList = List[{3, 2}, {1, 3}, {-1, 1}, {-2, -5}, {-3, 8}, {-1, 3}, {-2, 1}, {-7, 3}, {2, 0}, {-9, 5}, {-10, 7}, {5, 2}, {-5, 4}, {-8, -5}, {2, 9}, {3, -6}];
  randomPoint = RandomInteger[{1, Length[pointList]}];
  P = pointList[[randomPoint]];
  DisegnaPianoCartesiano[P[[1]], P[[2]]]

]
];



GetQuad[x0_, y0_] := DynamicModule[{x = x0, y = y0, quad},
  If[x >= 0 && y >= 0, quad = 1];
  If[x < 0 && y >= 0, quad = 2];
  If[x <= 0 && y <= 0, quad = 3];
  If[x >= 0 && y < 0, quad = 4];
  quad];


GetAngolo[alt_, bas_] := Return[N[ArcTan[alt / bas] / Degree ]];
AltezzaTorre[] := Manipulate[DynamicModule[{},
  Graphics[{
    Text["Torre", {75, -40 + altezza + 5}],
  (*Torre*)
    {EdgeForm[Thick], Brown, Rectangle[{70, -40}, {80, -40 + altezza}]},
  (*Punto osservatore*)
    {Blue, PointSize[Large], Point[{x, -40}]},
    Line[{{-100, -40}, {150, -40}}],
    Line[{{x, -40}, {70, -40 + altezza}}],
    {Green, Thickness[0.01], Line[{{x, -40}, {69, -40}}]},
    dist := computeDistance[x, 0, 70, 0],
    angolo := GetAngolo[altezza, dist],

    Text[Style[StringForm["Angolo \!\(\*
StyleBox[\"\[Alpha]\", \"TradFormChar\"]\): `` \[Degree]", N[angolo]], Red, 15], {-79, 85}],
    Text[Style[StringForm["Distanza: ``", dist], Green, 15], {-87, 95}],
    Text[Style[StringForm["Altezza = Distanza x tan(``) = ``", N[angolo], altezza], 15], {-50, 75}],
    {Thickness[0.001], Red, Circle[{x, -40}, 20, {0, angolo Pi / 180}]},
    Text[Style["\!\(\*
StyleBox[\"\[Alpha]\", \"TradFormChar\"]\)", Red, 20], {x + 36, -35}]
  },
    PlotRange -> {{-110, 85}, {-45, 160}},
    ImageSize -> 450
  ]
],
  {{altezza, 20, "Altezza"}, 20, 150, 1},
  {{x, -10, "Posizione"}, -100, 65, 1}
];


(* ::InheritFromParent:: *)
(**)


(* ::InheritFromParent:: *)
(**)


(* ::InheritFromParent:: *)
(**)
End[]; (* fine sezione privata *)
EndPackage[]; (* Fine del Package *)