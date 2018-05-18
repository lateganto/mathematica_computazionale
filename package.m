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
ThTriangoliUno::usage = "grafici per formule sui triangoli";
ThTriangoliDue::usage = "grafici per formule sui triangoli";
ThTriangoloProiezione::usage = "proiezione del triangolo";
AngoloOrientato::usage = "grafico angolo orientato";
(*  ESERCIZI  *)
EsCoordinate::usage = "esercizio su coordinate";
EsDistanze::usage = "calcola la distanza distanza tra due punti";
EsCos::usage = "esercizio cos";
EsSin::usage = "esercizio cos";
EsRadiantDegree::usage = "Esercizio conversione da radianti a Gradi e viceversa";

EsProiezioneAsseX::usage = "esercizio proiezione";

(* APPLICAZIONI*)
AltezzaTorre::usage = "applicazione altezza torre";
DistancePointsApplication::usage = "Distanza fra due punti visibili di cui uno non accessibile";


(*DEFINIZIONI di GRADO e RADIANTE*)
defGradi = Text["Consideriamo un angolo giro e dividiamolo in 360 parti congruenti tra loro.
Poniamo 1\[Degree] una di queste parti. Preso dunque un angolo qualsiasi,\ncontiamo quante volte un grado \[EGrave] contenuto in esso."];
defRadianti = Text["Prendiamo una circonferenza di raggio r e un angolo al centro \[Alpha];
l \[EGrave] l'arco sotteso dall'angolo.\nLa misura in radianti  \[EGrave] data dal rapporto l/r.
In particolare, 1 rad \[EGrave] un angolo\nche sottende un arco lungo quanto il raggio."];

Begin["`Private`"]; (* Comincia spazio privato *)

(*FUNZIONI E STRUTTURE DATI AUX*)
FromRadToGrad[x_] := Return[x * 180 / Pi];
FromGradToRad[x_] := Pi Rationalize[N[x]] / Pi;
fromDegreeToPi[x_] := Pi Rationalize[N[x \[Degree]] / Pi];
fromPiToDegree[x_] := With[{d = N[x]}, Defer[d \[Degree]]];

getAngle[p1_, p2_] := Mod[ArcTan @@ (p2 - p1), 2 Pi];
pointList = List[{3, 2}, {1, 3}, {-1, 1}, {-2, -5}, {-3, 8}, {-1, 3}, {-2, 1}, {-7, 3}];
pointListTwo = List[{2, 0}, {-9, 5}, {-10, 7}, {5, 2}, {-5, 4}, {-8, -5}, {2, 9}, {3, -6}];
angleList = List[0, 30, 45, 60, 90, 120, 135, 150, 180, 210, 225, 240, 270, 300, 315, 330, 360];
sinCosList = List[0, 1, -1, 1 / 2, -1 / 2, Sqrt[2] / 2, -Sqrt[2] / 2, Sqrt[3] / 2, -Sqrt[3] / 2];

(* ::InheritFromParent:: *)
(**)
font = FontFamily -> "Helvetica";

ThTriangoliUno[] := Row[{
  Column[{
    Row[{
      Graphics[{
        {LightYellow, EdgeForm[Thick], Triangle[{{-1, 0}, {1, 1}, {1, 0}}]},
        Red,
        Thickness[0.009],
        Line[{{-1, 0}, {1, 1}}],
        Text[Style["\[Alpha]", 20], {-0.6, 0.07}],
        Text[Style["\[Beta]", 20], {0.9, 0.8}],
        Text[Style["c", 20], {0, 0.6}],
        Text[Style["a", 20, Black], {1.1, 0.5}]
      },
        Axes -> False,
        Ticks -> None,
        ImageSize -> 350
      ],
      "\t\t",
      Column[{
        Style["Cateto = Ipotenusa * seno dell'angolo opposto", 20, font, Bold],
        Style["a = c * Sen(\[Alpha])", Red, 25, Bold],
        "\n",
        Style["Cateto = Ipotenusa * coseno dell'angolo acuto adiacente", font, 20, Bold],
        Style["a = c * Cos(\[Beta])", Red, 25, Bold]
      }]
    }]
  }]
}];

ThTriangoliDue[] := Row[{
  Column[{
    Row[{
      Graphics[{
        {LightYellow, EdgeForm[Thick], Triangle[{{-1, 0}, {1, 1}, {1, 0}}]},
        Red,
        Thickness[0.009],
        Line[{{-1, 0}, {1, 0}}],
        Text[Style["\[Alpha]", 20], {-0.6, 0.07}],
        Text[Style["\[Beta]", 20], {0.9, 0.8}],
        Text[Style["b", font, 20], {0, -0.1}],
        Text[Style["a", font, 20, Black], {1.1, 0.5}]
      },
        Axes -> False,
        Ticks -> None,
        ImageSize -> 350
      ],
      "\t\t",
      Column[{
        Style["Cateto = altroCateto * Tangente angolo", font, 20, Bold],
        Style["a = b * Tan(\[Alpha])", Red, font, 25, Bold]
      }]
    }]
  }]
}];

EsProiezioneAsseX[] := DynamicModule[{esito = "", colorEsito = ""},
  Row[{
    Graphics[{
      Circle[],
      Line[{ {0, 0}, {1 / 2, Sqrt[3] / 2} }],
      {Dashed, Line[{ {1 / 2, Sqrt[3] / 2}, {1 / 2, 0} }]},
      {Thickness[0.01],
        RGBColor[255, 0, 0],
        Line[{ {0, 0}, {1 / 2, 0} }]},
      PointSize[Large],
      Point[{0, 0}],
      Point[{1 / 2, Sqrt[3] / 2}],
      Point[{1 / 2, 0}],
      Text[Style["O", Medium], {0 - 0.05, 0 - 0.05}],
      Text[Style["A", Medium], {1 / 2 + 0.05, Sqrt[3] / 2 + 0.05}],
      Text[Style["H", Medium], {1 / 2 + 0.05, 0 - 0.05}]
    },
      ImageSize -> 350,
      Axes -> True,
      Ticks -> None
    ],
    Column[{
      sinCosList2 = DeleteCases[sinCosList, 1 / 2];
      incorrects = RandomSample[sinCosList2, 3];

      btn1 = Button[Style[incorrects[[1]], 20],
        {esito := "Sbagliato!", colorEsito := Red}];
      btn2 = Button[Style[1 / 2, 20],
        {esito := "Corretto!", colorEsito := Green}];
      btn3 = Button[Style[incorrects[[2]], 20],
        {esito := "Sbagliato!", colorEsito := Red}];
      btn4 = Button[Style[incorrects[[3]], 20],
        {esito := "Sbagliato!", colorEsito := Red}];

      buttons = List[btn1, btn2, btn3, btn4];
      randomButtons = RandomSample[buttons];

      Dynamic[Text[Style[esito, colorEsito, 15]]],
      Style["Quanto misura la proiezione di OA sulla retta delle ascisse?", font, 20],
      randomButtons[[1]],
      randomButtons[[2]],
      randomButtons[[3]],
      randomButtons[[4]]
    }]
  }]
];

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

EsSin[] := DynamicModule[{esitoSin = "", colorEsitoSin = ""},
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
      Text[Style[IntegerString[random[[1]]] <> "\[Degree]", Large, Blue], {-0.90, 1}],
      Text[Style["P", 15, Bold, Black], {cos * 1.1, sin * 1.1}],
      {Black, PointSize -> .02, Point[{cos, sin}]}},
      ImageSize -> 350,
      Axes -> True,
      Ticks -> None
    ] (*FINE GRAPHICS*),
    Row[{
      Column[{
        Button["Aiuto",
          CreateDialog[{
            TextCell["l'ordinata del punto P"],
            DefaultButton[]
          }],
          ImageSize -> Medium, BaseStyle -> {"GenericButton"}
        ],

        Button["Nuovo Esercizio", FrontEndExecute[FrontEndToken[NotebookLocate["EsSin"], "Evaluate"]], ImageSize -> Medium, BaseStyle -> {"GenericButton"}],
        incorrects = RandomSample[sinCosList2, 3];
        btn1 = Button[Style[Rationalize[Sin[x]], 15], {esitoSin := "Corretto!", colorEsitoSin := Green}];
        btn2 = Button[Style[Rationalize[incorrects[[1]]], 15], {esitoSin := "Sbagliato!", colorEsitoSin := Red}];
        btn3 = Button[Style[Rationalize[incorrects[[2]]], 15], {esitoSin := "Sbagliato!", colorEsitoSin := Red}];
        btn4 = Button[Style[Rationalize[incorrects[[3]]], 15], {esitoSin := "Sbagliato!", colorEsitoSin := Red}];

        buttons = List[btn1, btn2, btn3, btn4];
        randomButtons = RandomSample[buttons];

        Dynamic[Text[Style[esitoSin, colorEsitoSin, 15]]],
        Style["Seleziona il valore del seno:", font, 15],
        randomButtons[[1]],
        randomButtons[[2]],
        randomButtons[[3]],
        randomButtons[[4]]
      }]
    }]
  }]
];

EsCos[] := DynamicModule[{esitoCos = "", colorEsitoCos = ""},
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
      Text[Style[IntegerString[random[[1]]] <> "\[Degree]", Large, Blue], {-0.90, 1}],
      Text[Style["P", 15, Bold, Black], {cos * 1.1, sin * 1.1}],
      {Black, PointSize -> .02, Point[{cos, sin}]}},
      ImageSize -> 350,
      Axes -> True,
      Ticks -> None
    ] (*FINE GRAPHICS*),
    Row[{
      Column[{

        Button["Aiuto",
          CreateDialog[{
            TextCell["l'ascissa del punto P"],
            DefaultButton[]
          }],
          ImageSize -> Medium, BaseStyle -> {"GenericButton"}
        ],
        incorrects = RandomSample[sinCosList2, 3];

        btn5 = Button[Style[Rationalize[Cos[x]], 15], {esitoCos := "Corretto!", colorEsitoCos := Green}];
        btn6 = Button[Style[Rationalize[incorrects[[1]]], 15], {esitoCos := "Sbagliato!", colorEsitoCos := Red}];
        btn7 = Button[Style[Rationalize[incorrects[[2]]], 15], {esitoCos := "Sbagliato!", colorEsitoCos := Red}];
        btn8 = Button[Style[Rationalize[incorrects[[3]]], 15], {esitoCos := "Sbagliato!", colorEsitoCos := Red}];

        buttons = List[btn5, btn6, btn7, btn8];
        randomButtons = RandomSample[buttons];
        Button["Nuovo Esercizio", FrontEndExecute[FrontEndToken[NotebookLocate["EsCos"], "Evaluate"]], ImageSize -> Medium, BaseStyle -> {"GenericButton"}],

        Dynamic[Text[Style[esitoCos, colorEsitoCos, 15]]],
        Style["Seleziona il valore del coseno:", font, 15],
        randomButtons[[1]],
        randomButtons[[2]],
        randomButtons[[3]],
        randomButtons[[4]]
      }]
    }]
  }]
];

ThTriangoloProiezione[] := Row[{
  Column[{
    Style["I triangoli ABC e APH sono simili,\nperci\[OGrave] il rapporto tra i lati\n\[EGrave] costante, da cui ricaviamo che:", font, 19],
    Panel[Style["BC : AB = PH : AP\nAC : AB = AH : AP", font, 20, Bold]],
    Style["e poich\[EGrave]:", font, 20],
    Panel[Style["AP = 1\nPH = Sen(\[Alpha])\nAH = Cos(\[Alpha])", font, 20, Bold]],
    Style["vale", font, 20],
    Panel[Style["BC = Sen(\[Alpha]) AB   AC = Cos(\[Alpha]) AB", font, 20, Bold]]
  }],
  Column[{
    "\t\t"
  }],
  Column[{
    P = {Sqrt[2] / 2, Sqrt[2] / 2};
    Graphics[{
      {LightGreen, EdgeForm[Thick], Triangle[{{0, 0}, {1.5, 0}, {1.5, 1.5}}]},
      Circle[],
    (*Seno*)
      {Red, Thickness[0.009], Line[{P, {P[[1]], 0}}]},
      Text[Style["Cos(\[Alpha])", Green, Bold, 19], {P[[1]] / 2, -0.15}],

    (*Coseno*)
      {Green, Thickness[0.009], Line[{{0, 0}, {P[[1]], 0}}]},
      Text[Style["Sen(\[Alpha])", Red, Bold, 19], {P[[1]] * 1.4, P[[2]] * 0.5}],

    (*punto intersezione P*)
      {Black, PointSize -> .02, Point[P]},
      Text[Style["P", 19], {P[[1]] * 1.1, P[[2]] * 1.2}],

    (*H*)
      Text[Style["H", 19], {P[[1]], -0.15}],
    (*A*)
      Text[Style["A", 19], {-0.09, -0.09}],
    (*B*)
      Text[Style["B", 19], {1.6, 1.6}],
    (*C*)
      Text[Style["C", 19], {1.6, -0.09}],

    (*Arco*)
      Circle[{0, 0}, 0.2, {0, Pi / 4}],
      Text[Style["\[Alpha]", 19], {0.3, 0.145}]


    },
      ImageSize -> 360,
      Axes -> True,
      Ticks -> None,
      PlotRange -> {{-1.2, 1.7}, {-1.2, 1.7}}
    ]
  }]
}];

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
        Text[Style[a \[Degree], 15, Blue], {-1, 1}],
        Text[Style[a Pi / 180, 18, Orange], {+ 1, 1}],
      (*RAGGIO*)
        Thickness[0.012],
        Line[{{0, 0}, {x, y}}]
      }, Axes -> True, ImageSize -> 350, Ticks -> None, PlotRange -> {{-1.4, 1.4}, {-1.4, 1.4}}
      ]
    }] (*fine colonna grafico*),
    Column[{"\t"}],
    Column[{
      Row[{Text[Style["GRADI (\[Degree])", Blue, Bold, 20]], "\n", Text[Style[defGradi, 17]]}],
      Row[{"\n"}],
      Row[{Text[Style["RADIANTI (rad)", Orange, Bold, 20]], "\n", Text[Style[defRadianti, 17]]}]
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
        Style[gradi Pi / 180, 20, Orange],
        "\t=\t",
        "360\t:\t",
        HoldForm[2 Pi],
        "\n\n\n",
        Style["Gradi", Blue], "\n",
        Style["Radianti", Orange]

      }]  }], "\t",
    Column[{

      Style["Se \[Alpha] \[EGrave] l'angolo di cui vogliamo conoscere la misura, vale la seguente formula:", font, 15],
      Panel[
        Style[StringForm["`` : ``= 360\[Degree] : `` \n``= `` \n ``= ``",
          HoldForm[Subscript["\[Alpha]", "grad"]],
          HoldForm[Subscript["\[Alpha]", "rad"]],
          2 Pi,
          HoldForm[Subscript["\[Alpha]", "grad"]],
          HoldForm[Subscript["\[Alpha]", "rad"] "360\[Degree]" / (2 Pi)],
          HoldForm[Subscript["\[Alpha]", "rad"]],
          HoldForm[Subscript["\[Alpha]", "grad"] 2 Pi / "360\[Degree]"]
        ], 24, Bold, font],
        ImageSize -> 600,
        Alignment -> {Center, Center}
      ]


    }]


  }](*fine row*),
  {{gradi, 0, "Gradi"}, 0, 360, 1}
];

GraficoPrimaRelazione[] :=
    Manipulate[
      Row[{
        Column[{
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
        Column[{
          "\t\t"
        }],
        Column[{
          Row[{
            Style["Il triangolo evidenziato \[EGrave] rettangolo,\nper cui possiamo utilizzare il:", font, 20]
          }],
          Row[{
            t = Style["Teorema di Pitagora", 24, Bold, font, Red];,
            Hyperlink[t, "https://it.wikipedia.org/wiki/Teorema_di_Pitagora"]
          }]
        }]

      }],
      {{x, 45, ""}, 0, 360, 1}
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
      Text[Style[StringForm["`` ~ ``", IntegerString[FromRadToGrad[x]] <> "\[Degree]", x], 15, Blue], {-0.90, 1}],
      Text[Style["Seno", Medium, Red], {0.8, 1}],
      Text[Style["Coseno", Medium, Green], {0.8, 0.9}],

      {Black, PointSize -> .02, Point[{Cos[x], Sin[x]}]}},
      ImageSize -> 350,
      Axes -> True,
      Ticks -> None,
      PlotRange -> {{-1.3, 1.3}, {-1.3, 1.3}}
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
              Text[Style["Non Definito", 20], {x + 0.5, 0.5 + Sin[x]}]

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
      Text[Style[StringForm["`` ~ ``", IntegerString[FromRadToGrad[x]] <> "\[Degree]", x], 15, Blue], {-0.90, 1}],
      Text[Style["Seno", Medium, Red], {0.8, 1}],
      Text[Style["Coseno", Medium, Green], {0.8, 0.9}],

      {Black, PointSize -> .02, Point[{Cos[x], Sin[x]}]}},
      ImageSize -> 350,
      Axes -> True,
      Ticks -> None,
      PlotRange -> {{-1.3, 1.3}, {-1.3, 1.3}}
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
          Text[Style[Rationalize[Sin[x]], 20], {x + 0.5, Sin[x] - 0.5 }],
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
          Text[Style[Cos[x], 20], {x + 0.5, Cos[x] - 0.5}],
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
AngoliAssociati60[] := Manipulate[
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

          Text[Style[StringForm["Cos(``) = -Cos(``)", Lista[[2]] 180 / Pi Degree, AngoloGrad], font, 20]],
          Text[Style[StringForm["Sen(``) = Sen(``)", Lista[[2]] 180 / Pi Degree, AngoloGrad], font, 20]], "\n",

          Text[Style[StringForm["Cos(``) = -Cos(``)", Lista[[3]] 180 / Pi Degree, AngoloGrad], font, 20]],
          Text[Style[StringForm["Sen(``) = -Sen(``)", Lista[[3]] 180 / Pi Degree, AngoloGrad], font, 20]], "\n",

          Text[Style[StringForm["Cos(``) = Cos(``)", Lista[[4]] 180 / Pi Degree, AngoloGrad], font, 20]],
          Text[Style[StringForm["Sen(``) = -Sen(``)", Lista[[4]] 180 / Pi Degree, AngoloGrad], font, 20]], "\n"
        }]

      }]
    }]
  }],
  {{n, 3, "Angolo"}, 0, 3, 1 }
];

ListaAngoliAssociati0 := {0, \[Pi] / 2, \[Pi], 3 \[Pi] / 2, 2 \[Pi]};
AngoliAssociati0[] := Manipulate[
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
          Text[Style["Cos(90) = Sen(0)\nSen(90) = Cos(0)", font, 20]], "\n",
          Text[Style["Cos(180) = -Cos(0)\nSen(180) = Sen(0)", font, 20]], "\n",
          Text[Style["Cos(270) = Sen(0)\nSen(270) = -Cos(0)", font, 20]], "\n",
          Text[Style["Cos(360) = Cos(0)\nSen(360) = Cos(0)", font, 20]]

        }]

      }]
    }]
  }],
  {{n, 4, "Angolo"}, 0, 4, 1 }
];

ListaAngoliAssociati30 := {\[Pi] / 6, 5 \[Pi] / 6, 7 \[Pi] / 6, 11 \[Pi] / 6};
AngoliAssociati30[] := Manipulate[
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

          Text[Style[StringForm["Cos(``) = -Cos(``)", Lista[[2]] 180 / Pi Degree, AngoloGrad], font, 20]],
          Text[Style[StringForm["Sen(``) = Sen(``)", Lista[[2]] 180 / Pi Degree, AngoloGrad], font, 20]], "\n",

          Text[Style[StringForm["Cos(``) = -Cos(``)", Lista[[3]] 180 / Pi Degree, AngoloGrad], font, 20]],
          Text[Style[StringForm["Sen(``) = -Sen(``)", Lista[[3]] 180 / Pi Degree, AngoloGrad], font, 20]], "\n",

          Text[Style[StringForm["Cos(``) = Cos(``)", Lista[[4]] 180 / Pi Degree, AngoloGrad], font, 20]],
          Text[Style[StringForm["Sen(``) = -Sen(``)", Lista[[4]] 180 / Pi Degree, AngoloGrad], font, 20]], "\n"
        }]

      }]
    }]
  }],
  {{n, 3, "Angolo"}, 0, 3, 1 }
];

ListaAngoliAssociati45 := {\[Pi] / 4, 3 \[Pi] / 4, 5 \[Pi] / 4, 7 \[Pi] / 4};
AngoliAssociati45[] := Manipulate[
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

          Text[Style[StringForm["Cos(``) = -Cos(``)", Lista[[2]] 180 / Pi Degree, AngoloGrad], font, 20]],
          Text[Style[StringForm["Sen(``) = Sen(``)", Lista[[2]] 180 / Pi Degree, AngoloGrad], font, 20]], "\n",

          Text[Style[StringForm["Cos(``) = -Cos(``)", Lista[[3]] 180 / Pi Degree, AngoloGrad], font, 20]],
          Text[Style[StringForm["Sen(``) = -Sen(``)", Lista[[3]] 180 / Pi Degree, AngoloGrad], font, 20]], "\n",

          Text[Style[StringForm["Cos(``) = Cos(``)", Lista[[4]] 180 / Pi Degree, AngoloGrad], font, 20]],
          Text[Style[StringForm["Sen(``) = -Sen(``)", Lista[[4]] 180 / Pi Degree, AngoloGrad], font, 20]], "\n"
        }]

      }]
    }]
  }],
  {{n, 3, "Angolo"}, 0, 3, 1 }
];

AngoloOrientato[] := Row[{
  Column[{
    Graphics[{
      Text[Style["O", 15], {-0.1, 0}],
      Line[{{0, 0}, {1, 1}}],
      Line[{{0, 0}, {1, -1}}],
      Line[{{0, 0}, {1, 0}}],
      {Red, Circle[{0, 0}, 0.5, {0, Pi / 4}]},
      {Blue, Circle[{0, 0}, 0.5, {0, -Pi / 4}]},
      Text[Style["\[Alpha] Angolo Positivo", 17, Red, Bold], {0.9, 0.25}],
      Text[Style["\[Beta] Angolo Negativo", 17, Blue, Bold], {0.9, -0.25}]

    }, ImageSize -> 300]
  }],
  Column[{
    "\t"
  }]
  , Column[{
    Style["Conviene collegare l'idea di angolo \na quella di rotazione di uno dei due lati.\nQuesta rotazione pu\[OGrave] avvenire in verso:\n", font, 17],
    Style["- Antiorario, e diciamo che \[EGrave] positivo;", font, 17],
    Style["- Orario, e diciamo che \[EGrave] negativo.", font, 17]


  }]

}];

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

getAngle[p1_, p2_, p3_] :=
    Return[ArcCos[
      (EuclideanDistance[p1, p2]^2 + EuclideanDistance[p1, p3]^2 - EuclideanDistance[p2, p3]^2) /
          (2 * EuclideanDistance[p1, p2] * EuclideanDistance[p1, p3]) ]];

GetAngolo[alt_, bas_] := Return[N[ArcTan[alt / bas] / Degree ]];
AltezzaTorre[] :=
    Row[{
      Column[{
        Manipulate[DynamicModule[{},
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
        ]
      }],
      Column[{
        "\t"
      }],
      Column[{
        Text[Style["Lorenzo, dopo questa lettura, ha avuto un'idea\nsu come calcolare facilmente l'altezza della tour Eiffel.\nProcede in questo modo: \nmisura la sua distanza dalla base della torre\ne approssima l'angolo \[Alpha] (con un ottante).\nOsserva che si forma un triangolo rettangolo\ne pu\[OGrave] quindi usare la propriet\[AGrave] della tangente.", font, 20]]
      }]

    }];
DistancePointsApplication[] := Row[{
  Column[{
    Row[{
      Manipulate[
        Deploy[
          DynamicModule[
            { p1 = {-10, -10},
              p2 = {10, 10},
              p3 = {10, -10}
            },

            Dynamic@Graphics[
              {
                pts := {{-20, 0}, {-6, -7}, {10, 4}, {23, -5}},
                {Thickness[0.13], LightCyan, BezierCurve[pts]},
                PointSize[0.02],

                Point[p1],
                Locator[Dynamic[p1], None],

                {Red, Point[p2]},
                {Red, Point[p3]},

                If[First[p1] <= First[p3],
                  If[Last[p1] <= Last[p3],
                    {LightBlue, Disk[p1, 3, {0 + getAngle[p1, p3, {First[p3], Last[p1]}], getAngle[p1, p2, p3] + getAngle[p1, p3, {First[p3], Last[p1]}]}]},
                    {LightBlue, Disk[p1, 3, {0 - getAngle[p1, p3, {First[p3], Last[p1]}], getAngle[p1, p2, p3] - getAngle[p1, p3, {First[p3], Last[p1]}]}]}],

                  If[Last[p1] <= Last[p3],
                    {LightBlue, Disk[p1, 3, {Pi / 2 + getAngle[p1, p2, {First[p1], Last[p2]}], Pi / 2 + getAngle[p1, p2, p3] + getAngle[p1, p2, {First[p1], Last[p2]}]}]},

                    If[Last[p1] <= Last[p2],
                      {LightBlue, Disk[p1, 3, {Pi / 2 + getAngle[p1, p2, {First[p1], Last[p2]}], Pi / 2 + getAngle[p1, p2, p3] + getAngle[p1, p2, {First[p1], Last[p2]}]}]},
                      {LightBlue, Disk[p1, 3, {Pi + getAngle[p1, p2, {First[p2], Last[p1]}], Pi + getAngle[p1, p2, p3] + getAngle[p1, p2, {First[p2], Last[p1]}]}]}]
                  ]
                ],

                If[Last[p1] >= Last[p3],
                  If[First[p1] < First[p3],
                    {LightRed, Disk[p3, 2.5, {Pi / 2, getAngle[p3, p1, p2] + Pi / 2}]},
                    {LightRed, Disk[p3, 2.5, {getAngle[p3, p1, {First[p1], Last[p3]}], getAngle[p3, p1, p2] + getAngle[p3, p1, {First[p1], Last[p3]}]}]}],

                  If[First[p1] < First[p3],
                    {LightRed, Disk[p3, 2.5, {Pi / 2, getAngle[p3, p1, p2] + Pi / 2}]},
                    {LightRed, Disk[p3, 2.5, {2 Pi - getAngle[p3, p1, {First[p1], Last[p3]}], 2 Pi + Pi / 2}]}]
                ],

                If[Last[p1] >= Last[p2],
                  If[First[p1] < First[p2],
                    {LightGray, Disk[p2, 2.5, {Pi / 2 + getAngle[p2, p1, {First[p2], Last[p1]}], Pi / 2 + getAngle[p2, p1, {First[p2], Last[p1]}] + getAngle[p2, p1, p3]}]},
                    {LightGray, Disk[p2, 2.5, {Pi + Pi / 2, Pi + Pi / 2 + getAngle[p2, p1, p3]}]}],

                  If[First[p1] < First[p2],
                    {LightGray, Disk[p2, 2.5, {Pi + getAngle[p2, p1, {First[p1], Last[p2]}], Pi + getAngle[p2, p1, {First[p1], Last[p2]}] + getAngle[p2, p1, p3]}]},
                    {LightGray, Disk[p2, 2.5, {Pi + Pi / 2, Pi + Pi / 2 + getAngle[p2, p1, p3]}]}]
                ],

                Style[Text["Prova a spostare il punto A nel piano", {0, 18}], FontSize -> 16, font, Blue],
                Style[Text["A", {First[p1] - 1, Last[p1] - 1}], FontSize -> 18],
                Style[Text["C", {First[p3] + 1, Last[p3] - 1}], FontSize -> 18],
                Style[Text["B", {First[p2] + 1, Last[p2] + 1}], FontSize -> 18],
                Style[Text["\[Alpha]", {First[p1] + 3.5, Last[p1] + 1.5}], FontSize -> 18, Blue],
                Style[Text["\[Beta]", {First[p3] - 3, Last[p3] + 1.5}], FontSize -> 18, Red],
                Style[Text["\[Gamma]", {First[p2] - 1.5, Last[p2] - 3}], FontSize -> 18, Gray],
                Style[Text[StringForm["AC =  ``", SetPrecision[EuclideanDistance[p1, p3], 4]], {-10, -16}], FontSize -> 14],
                Style[Text[StringForm["AB =  ``", SetPrecision[EuclideanDistance[p1, p2], 4]], {-10, -18}], FontSize -> 14],
                Style[Text[StringForm["CAB = \[Alpha] =  ``", SetPrecision[N[getAngle[p1, p2, p3] 180 / Pi], 4]], {4, -16}], FontSize -> 14],
                Style[Text[StringForm["BCA = \[Beta] = ``", SetPrecision[N[getAngle[p3, p1, p2] 180 / Pi], 4]], {4, -18}], FontSize -> 14],

                {Thickness[0.005], Line[{p1, p2}]},
                {Thickness[0.005], Blue, Line[{p1, p3}]},
                {Thickness[0.005], Red, Line[{p2, p3}]},

                {Red, Point[p2]},
                {Red, Point[p3]}
              },
              PlotRange -> {{-16, 20}, {-20, 20}}
            ]
          ]
        ]
      ]
    }]
  }],
  Text["\t"],
  Column[{
    Row[{
      Style[Text["Supponiamo di voler calcolare la distanza fra due punti A e B:\nio mi trovo in A ma non posso raggiungere B perch\[EAcute] \[EGrave] al di l\[AGrave] del fiume."], font, FontSize -> 16]
    }],
    Row[{
      Style[Text["Possiamo spostarci in un punto C e calcolare la distanza AC\ned inoltre gli angoli CAB=\[Alpha] e ACB=\[Beta]."], font, FontSize -> 16]
    }],
    Text["\n"],
    Row[{
      Style[Text["Abbiamo quindi il triangolo ABC in cui conosciamo due angoli ed il lato compreso,\nquindi per risolvere il triangolo possiamo calcolare il terzo angolo ricordando che\nla somma degli angoli interni di un triangolo e' un angolo piatto."], font, FontSize -> 16]
    }],
    Row[{
      Style[Text["\[Gamma] = 180\[Degree] - \[Alpha] - \[Beta]"], FontSize -> 20, Blue]
    }],
    Text["\n"],
    Row[{
      Style[Text["E poi applicare il teorema dei seni:  "], font, FontSize -> 16],
      Button["Teorema Dei Seni",
        CreateDialog[{
          TextCell["In ogni triangolo \[EGrave] costante il rapporto fra ogni lato\ned il seno dell'angolo opposto\ne tale costante equivale al doppio del raggio\ndel cerchio circoscritto al triangolo."],
          DefaultButton[]
        }],
        ImageSize -> Large, font, BaseStyle -> {"GenericButton"}
      ]
    }],
    Row[{
      Style[
        StringForm["\n `` = ``",
          HoldForm["AC" / "Sen(\[Gamma])"],
          HoldForm["AB" / "Sen(\[Beta])"]
        ], font, 25, Bold, Blue]
    }],
    Row[{
      Style[Text["Ottenendo quindi:\n"], font, FontSize -> 16, font]
    }],
    Row[{

      Style[
        StringForm[" AB = ``",
          HoldForm["( AC Sen(\[Beta]) )" / "Sen(\[Gamma])" ]
        ], 25, Bold, font, Blue]
    }]
  }]
}];

angleList = List[0, 30, 45, 60, 90, 120, 135, 150, 180, 210, 225, 240, 270, 300, 315, 330, 360];
EsRadiantDegree := DynamicModule[{esito = "", colorEsito = Black}, Row[{
  Column[{
    randomAngleType = RandomInteger[{0, 1}];

    random = RandomSample[angleList, 4];
    incorrects = random[[2 ;; 4]];
    correct = First[random];

    If[randomAngleType == 0,
      btn5 = Button[Style[Rationalize[fromDegreeToPi[correct]], 15], {esito := "Corretto!", colorEsito := Green}];
      btn6 = Button[Style[Rationalize[fromDegreeToPi[incorrects[[1]]]], 15], {esito := "Sbagliato!", colorEsito := Red}];
      btn7 = Button[Style[Rationalize[fromDegreeToPi[incorrects[[2]]]], 15], {esito := "Sbagliato!", colorEsito := Red}];
      btn8 = Button[Style[Rationalize[fromDegreeToPi[incorrects[[3]]]], 15], {esito := "Sbagliato!", colorEsito := Red}],

      btn5 = Button[Style[Rationalize[correct], 15], {esito := "Corretto!", colorEsito := Green}];
      btn6 = Button[Style[Rationalize[incorrects[[1]]], 15], {esito := "Sbagliato!", colorEsito := Red}];
      btn7 = Button[Style[Rationalize[incorrects[[2]]], 15], {esito := "Sbagliato!", colorEsito := Red}];
      btn8 = Button[Style[Rationalize[incorrects[[3]]], 15], {esito := "Sbagliato!", colorEsito := Red}];
    ];

    buttons = List[btn5, btn6, btn7, btn8];
    randomButtons = RandomSample[buttons];
    Button["Nuovo Esercizio", FrontEndExecute[FrontEndToken[NotebookLocate["EsRadGrad "], "Evaluate"]], ImageSize -> Medium, BaseStyle -> {"GenericButton"}],

    Dynamic[Text[Style[esito, colorEsito, 20]]],


  (*Dynamic[Text[Style[esito, colorEsito, 15]]],*)
  (*Style[Dynamic[Esito]],*)
    If[randomAngleType == 0,
      Style[StringForm["Seleziona il valore dell'angolo: ``", correct], 15, font],
      Style[StringForm["Seleziona il valore dell'angolo: ``", fromDegreeToPi[correct]], 15, font]
    ],

    randomButtons[[1]],
    randomButtons[[2]],
    randomButtons[[3]],
    randomButtons[[4]]
  }]
}]
];





(* ::InheritFromParent:: *)
(**)


(* ::InheritFromParent:: *)
(**)


(* ::InheritFromParent:: *)
(**)
End[]; (* fine sezione privata *)
EndPackage[]; (* Fine del Package *)