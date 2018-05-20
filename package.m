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
puntiInaccessibili::usage = "Distanza fra due punti inaccessibili";

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


(*LE seguenti due Built-In hanno funzioni identiche in contesti diversi*)
(*Crea Grafico per le formule applicate al triangolo adicente*)
ThTriangoliUno[] := Row[{
  Column[{
    Row[{
    (*Inizio Graphics*)
      Graphics[{
      (*Colore, Bordo(Continuo), Punti del triangolo*)
        {LightYellow, EdgeForm[Thick], Triangle[{{-1, 0}, {1, 1}, {1, 0}}]},
      (*Le seguenti 3 tre linee determinano
   graficamente il cateto che viene calcolato nella formula *)
        Red,
        Thickness[0.009],
        Line[{{-1, 0}, {1, 1}}],
        Text[Style["\[Alpha]", 20], {-0.6, 0.07}],
        Text[Style["\[Beta]", 20], {0.9, 0.8}],
        Text[Style["c", 20], {0, 0.6}],
        Text[Style["a", 20, Black], {1.1, 0.5}]
      },
      (*Nessun asse con nessun intervallo*)
        Axes -> False,
        Ticks -> None,
        ImageSize -> 350
      ](*FIne graphics*),
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

(*Crea Grafico per le formule applicate al triangolo adicente*)
ThTriangoliDue[] := Row[{
  Column[{
    Row[{
    (*Inizio Graphics*)
      Graphics[{
      (*Colore, Bordo(Continuo), Punti del triangolo*)
        {LightYellow, EdgeForm[Thick], Triangle[{{-1, 0}, {1, 1}, {1, 0}}]},
      (*Le seguenti 3 tre linee determinano
      graficamente il cateto che viene calcolato nella formula *)
        Red, (*Avrà colore rosso*)
        Thickness[0.009], (* Uno spessore pari a 0.009 *)
        Line[{{-1, 0}, {1, 0}}], (*Linee del triangolo interessata*)
      (*Angoli - Testo*)
        Text[Style["\[Alpha]", 20], {-0.6, 0.07}],
        Text[Style["\[Beta]", 20], {0.9, 0.8}],
      (*Lati - Testo*)
        Text[Style["b", font, 20], {0, -0.1}],
        Text[Style["a", font, 20, Black], {1.1, 0.5}]
      },
      (*nessun asse  e nessun intervallo sull'asse*)
      (*nessun asse  e nessun intervallo sull'asse*)
        Axes -> False,
        Ticks -> None,
        ImageSize -> 350
      ],
      "\t\t",
    (*Altra colonna per mostrare le formule*)
      Column[{
        Style["Cateto = altroCateto * Tangente angolo", font, 20, Bold],
        Style["a = b * Tan(\[Alpha])", Red, font, 25, Bold]
      }]
    }]
  }]
}];

(*Disegna la circonferenza iniziale per la definizione di circ Goniometrica*)
DisegnaCirconferenzaInit[] := Graphics[{
(*Dichiarazione cerchio*)
  Circle[],
(*Spessore*)
  Thickness[0.01],
(*testo relativo al raggio*)
  Text[Style["Raggio=1 ", 14], {0.6, 0.30}],
(*Linea che unisce centro e il punto sulla circonferenza*)
  Line[{{0, 0}, {Sqrt[2] / 2, Sqrt[2] / 2}}],
(*Punto sulla circonferenza toccatao da cui passa il raggio*)
  {Red, PointSize -> 0.02, Point[{Sqrt[2] / 2, Sqrt[2] / 2}]},

(*Punto A*)
  Text[Style["A(0,1)", 15, Bold], {0.2, 1.1}],
(*Punto sulla circonferenza che identifica A*)
  {Red, PointSize -> 0.02, Point[{0, 1}]},
(*Punto B*)
  Text[Style["B(1,0)", 15, Bold], {1.2, 0.1}],
(*Punto sulla circonferenza che identifica B*)
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

(*Grafico per la Teoria della proiezione dei cateti sugli assi*)
ThTriangoloProiezione[] := Row[{
(*la prima colonna viene utlizzata per la stampa della teoria*)
  Column[{
    Style["I triangoli ABC e APH sono simili,\nperci\[OGrave] il rapporto tra i lati\n\[EGrave] costante, da cui ricaviamo che:", font, 19],
  (*i panel servono per evidenziare le formule*)
    Panel[Style["BC : AB = PH : AP\nAC : AB = AH : AP", font, 20, Bold]],
    Style["e poich\[EGrave]:", font, 20],
    Panel[Style["AP = 1\nPH = Sen(\[Alpha])\nAH = Cos(\[Alpha])", font, 20, Bold]],
    Style["vale", font, 20],
    Panel[Style["BC = Sen(\[Alpha]) AB   AC = Cos(\[Alpha]) AB", font, 20, Bold]]
  }],
(*Colonna per distanziare le colonne adiacenti*)
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
    (*Stampa dei punti nelle rispettive coordinate*)
      Text[Style["P", 19], {P[[1]] * 1.1, P[[2]] * 1.2}],
      Text[Style["H", 19], {P[[1]], -0.15}], (*H*)
      Text[Style["A", 19], {-0.09, -0.09}], (*A*)
      Text[Style["B", 19], {1.6, 1.6}], (*B*)
      Text[Style["C", 19], {1.6, -0.09}], (*C*)
      (*Arco con centro nell'origine
        dimensione raggio e intervallo da disegnare
      *)
      Circle[{0, 0}, 0.2, {0, Pi / 4}],
      (*label del angolo*)
      Text[Style["\[Alpha]", 19], {0.3, 0.145}]


    },
      (*opzione per la graphics*)
      ImageSize -> 360,
      Axes -> True,
      Ticks -> None,
      PlotRange -> {{-1.2, 1.7}, {-1.2, 1.7}}
    ]
  }]
}];

(*Calcolo della distanza tra due punti attraverso le coordinate*)
computeDistance[x1_, y1_, x2_, y2_] := Return[Sqrt[(x2 - x1)^2 + (y2 - y1)^2]];

(*Manipulate per la teoria relativa ai tipi di angolo*)
TipoAngolo[] := Manipulate[
  Row[{
    Column[{
      Row[{
        (*definzione dei 4 bottoni che modificano la manipulate settando il relativo
        valore di a che rappresenta la variabile dinamica con cui la manipate viene aggiornata*)
        Button["Nullo", a = 0], "\t",
        Button["Retto", a = 90], "\t",
        Button["Piatto", a = 180], "\t",
        Button["Giro", a = 360], "\n"}],

      Graphics[{
        (*Disengo della circ di raggio 1*)
        Circle[],
        (*punto x della angolo che si vuole visualizzare*)
        x := N[Cos[a Degree]];,
      (*punto y della angolo che si vuole visualizzare*)
        y := N[Sin[a Degree]];,
        (*Arco che identifica graficamnete l'arco*)
        {Blue, Thick, Circle[{0, 0}, 0.4, {0, a Pi / 180}]},
        (*Testo relativo al valore dell'angolo in gradi*)
        Text[Style[a \[Degree], 15, Blue], {-1, 1}],
      (*Testo relativo al valore dell'angolo in radianti*)
        Text[Style[a Pi / 180, 18, Orange], {+ 1, 1}],
        (*RAGGIO*)
        Thickness[0.012],
        (*dall'origine al punto calcolato dall'angolo selezionato*)
        Line[{{0, 0}, {x, y}}]
      }, Axes -> True, ImageSize -> 350, Ticks -> None, PlotRange -> {{-1.4, 1.4}, {-1.4, 1.4}}
      ]
    }] (*fine colonna grafico*),
    Column[{"\t"}],
    Column[{
      (*Definizione di gradi il cui corpo è all'inzio del package.m*)
      Row[{Text[Style["GRADI (\[Degree])", Blue, Bold, 20]], "\n", Text[Style[defGradi, 17]]}],
      Row[{"\n"}],
    (*Definizione di radianti il cui corpo è all'inzio del package.m*)
      Row[{Text[Style["RADIANTI (rad)", Orange, Bold, 20]], "\n", Text[Style[defRadianti, 17]]}]
    }]
  }](*fine row*),
  (*range della manipulate*)
  {{a, 0, "angolo"}, 0, 360, 1}];

(*Manipulate per la conversione gradi radianti*)
ThGradRad[] := Manipulate[
  Row[{
    Column[{
      Row[{
        "Angoli notevoli (\[Degree]): ",
        (*Lista di angoli notevi attraverso la Dynamic[gradi] ossia la varibile della manipulate*)
        PopupMenu[Dynamic[gradi], {0, 30, 45, 60, 90, 120, 135, 150, 180, 210, 225, 240, 270, 300, 315, 330, 360}],
        "\n\n",
        (*testo per la visione dei gradi*)
        Style[gradi "\[Degree]", 20, Blue],
        "\t:\t",
      (*testo per la visione dei radianti con il rispettivo valore in gradi*)
        Style[gradi Pi / 180, 20, Orange],
        "\t=\t",
        "360\t:\t",
        HoldForm[2 Pi],
        "\n\n\n",
        (*Colori al testo per gradi*)
        Style["Gradi", Blue], "\n",
      (*Colori al testo per radianti*)

        Style["Radianti", Orange]

      }]  }], "\t",
    Column[{
      (*Teoria accanto la manipulate*)
      Style["Se \[Alpha] \[EGrave] l'angolo di cui vogliamo conoscere la misura, vale la seguente formula:", font, 15],
      Panel[
        (*il panel serve per fare da sfondo alla formula della conversione
        che viene rappresentata attrerso una StringForm che funge da parser
        ai simboli stampati dalla HoldForm*)
        Style[StringForm["`` : ``= 360\[Degree] : `` \n``= `` \n ``= ``",
          HoldForm[Subscript["\[Alpha]", "grad"]],
          HoldForm[Subscript["\[Alpha]", "rad"]],
          2 Pi, (*Simbolo 2 pigreco*)
          (*la subscript serve per avere il pedice per i grad*)
          HoldForm[Subscript["\[Alpha]", "grad"]],
          (*per i radianti *)
          HoldForm[Subscript["\[Alpha]", "rad"] "360\[Degree]" / (2 Pi)],
          HoldForm[Subscript["\[Alpha]", "rad"]],
          HoldForm[Subscript["\[Alpha]", "grad"] 2 Pi / "360\[Degree]"]
        ], 24, Bold, font],
        ImageSize -> 600,
        (*la formula viene collocata al centro, sia verticale che orizzontale*)
        Alignment -> {Center, Center}
      ]
    }]


  }](*fine row*),
    (*range della manipulate*)
  {{gradi, 0, "Gradi"}, 0, 360, 1}
];

GraficoPrimaRelazione[] := Manipulate[
      Row[{
        Column[{
        (*realativo angolo in radianti*)
          rad := x Pi / 180;
          (*Punto sulla circonferenza*)
          P := {Cos[rad], Sin[rad]};
          (*punto x della relativa proiezione di P su asse x*)
          proiezioneX := {Cos[rad], 0};
          (*punto y della relativa proiezione di P su asse y*)
          proiezioneY := {Cos[rad], Sin[rad]};
          (*Definzione di triangolo giallo con punti {origine, punti sull'asse delle x ,
          ed il punto sulla circonferenza*)
          triangolo := {Yellow, Triangle[{{0, 0}, proiezioneX, P}]},
          Graphics[{ (*inzio graphics*)
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
          (*per distanziare le colonne adiacenti*)
          "\t\t"
        }],
        Column[{
          (*colonna per la teoria*)
          Row[{
            Style["Il triangolo evidenziato \[EGrave] rettangolo,\nper cui possiamo utilizzare il:", font, 20]
          }],
          Row[{
            (*collegamento alla pagina wikipedia del th di pitagora*)
            t = Style["Teorema di Pitagora", 24, Bold, font, Red];,
            Hyperlink[t, "https://it.wikipedia.org/wiki/Teorema_di_Pitagora"]
          }]
        }]

      }],

    (*range della manipulate in base agli angoli con valore di
    default = 45*)
      {{x, 45, ""}, 0, 360, 1}
    ];

(*circonferenza goniometrica + Grafico della funzione tangente*)
GraficoTangente[] := Manipulate[
  Row[{
    (*inzio della graphics*)
    Graphics[{
      (*draw della circonfernza di raggio 1*)
      Circle[],
      (*disegno punto origine*)
      Point[{0, 0}],
      (*setto la dimensione dei punti*)
      PointSize[Large],
      (*arco che identifica l'arco e che dipende dal valore della var x della manipulate*)
      {Blue, Circle[{0, 0}, 0.2, {0, x}]},
      (*linea tratteggiata per il raggio che congiunge l'origine con il punto sulla circoferenza*)
      {Dashed, Line[{{0, 0}, {Cos[x], Sin[x]}}]}, (*raggio*)
      Thickness[0.01],
      (*COLORE VERDE PER IL COSENO*)
      RGBColor[0, 255, 0],
    (*PROIEZIONE SULL'ASSE DELLE y*)
      Line[{{0, 0}, {Cos[x], 0}}],
      (*COLORE ROSSO PER IL SENO*)
      RGBColor[255, 0, 0],
      (*PROIEZIONE SULL'ASSE DELLE X*)
      Line[{{Cos[x], 0}, {Cos[x], Sin[x]}}],
      (*Stampa del valore in gradi*)
      Text[Style[StringForm["`` ~ ``", IntegerString[FromRadToGrad[x]] <> "\[Degree]", x], 15, Blue], {-0.90, 1}],
      (*LEGENDA*)
      Text[Style["Seno", Medium, Red], {0.8, 1}], (*DEL SENO*)
      Text[Style["Coseno", Medium, Green], {0.8, 0.9}], (*E DEL COSENO*)
      (*PUNTO SULLA CIRCONFERNZA*)
      {Black, PointSize -> .02, Point[{Cos[x], Sin[x]}]}
    },
      ImageSize -> 350,
      (*si per la view degli assi*)
      Axes -> True,
      (*con nessun intervallo*)
      Ticks -> None,
      PlotRange -> {{-1.3, 1.3}, {-1.3, 1.3}}
    ], (*FINE GRAPHICS*)
    Column[{
      (*grafico sulla destra per la plot della funzione tangente*)
      Show[
        (*disegno della tangente con periodo 0 2PiGreco*)
        Plot[Tan[y], {y, 0, 2 Pi},
          ImageSize -> 400,
          (*intervalli visibilie*)
          Ticks -> {{0, Pi / 2, Pi, 3 Pi / 2, 2 Pi, 5 Pi / 2}, {-1, 0, 1}},
          (*titolo della plot*)
          PlotLabel -> "Funzione Tangente",
          (*colore della plot*)
          PlotStyle -> RGBColor[1, 0, 1],
          PlotRange -> {{-1, 2 Pi + 1}, {-4, 4}}
        ],
        Graphics[{
        (*Trick per evitare il valore infinito per x = 90 e x = 270 *)
          PointSize[Large],
          (*trucco per non fare andare in crash il sistema quando si assume valore infinito*)
          If[x == Pi / 2 || x == 3 Pi / 2,
            {
              (*si blocca l'esecuzione e si stampa a video 'non definito' "*)
              {Dashed, Line[{{x, -10}, {x, 10}}]}, (*Asintoto*)
              Text[Style["Non Definito", 20], {x + 0.5, 0.5 + Sin[x]}]

            },{
                (*se non si assume valore infinito si disegna normalmente la proienzione*)
            {Dashed, Line[{{x, 0}, {x, Tan[x]}}]},
            (*punto sulla curva in base al valore della var x*)
              Point[{ x, Tan[x] }],
            (*testo sul punto*)
              Text[Style[Tan[x], 20], {x + 0.5, 0.5 + Sin[x]}]

            }
          ]
        }]
      ]
    }]
  }],
  (*Input per lo slider*)
  {{x, 0, "Naviga"}, 0, 2 Pi, Pi / 12},
  (*input per la tendina*)
  {{x, 0, "Scegli"}, 0, 2 Pi, Pi / 12},
  (*scelta dei tipi di controllo*)
  ControlType -> {Slider, PopupMenu}
];

(*Circonferenza goniometrica + funzioni di seno e coseno*)
DisegnaCirconferenza[] := Manipulate[
  Row[{
    (*inzio graphics*)
    Graphics[{
      (*definzione cerchio raggio 1*)
      Circle[],
      (*punto origine*)
      Point[{0, 0}],
      (*dimensione dei punti*)
      PointSize[Large],
      (*arco identificato dal valore della x della manipulate*)
      {Blue, Circle[{0, 0}, 0.2, {0, x}]},
      (*raggio tratteggiato il cui secondo punto è calcolato in
      base al valore della variabile x*)
      {Dashed, Line[{{0, 0}, {Cos[x], Sin[x]}}]}, (*raggio*)
      (*proeizione asse delle x*)
      Thickness[0.01],
      (*di colore verde*)
      RGBColor[0, 255, 0],
      (*per identificare la proiezione di P su asse delle X*)
      Line[{{0, 0}, {Cos[x], 0}}],
      (*di colore rosso*)
      RGBColor[255, 0, 0],
      (*per identificare la proiezione di P su asse delle Y*)
      Line[{{Cos[x], 0}, {Cos[x], Sin[x]}}],
    (*Stampa del valore in gradi*)
      Text[Style[StringForm["`` ~ ``", IntegerString[FromRadToGrad[x]] <> "\[Degree]", x], 15, Blue], {-0.90, 1}],
     (*Legenda per i colori sul grafico*)
      Text[Style["Seno", Medium, Red], {0.8, 1}],
      Text[Style["Coseno", Medium, Green], {0.8, 0.9}],
      (*punto sulla circonferenza*)
      {Black, PointSize -> .02, Point[{Cos[x], Sin[x]}]}},
      ImageSize -> 350,
      Axes -> True,
      Ticks -> None,
      PlotRange -> {{-1.3, 1.3}, {-1.3, 1.3}}
    ], (*FINE GRAPHICS*)
  (* GRAFICI SENO E COSENO *)
    Column[{
      (*SENO*)
      Show[
        (*PLOT DELLA FUNZIONE SENO nel periodo indicato*)
        Plot[Sin[y], {y, 0, 2 Pi},
          (*dimensione grafico*)
          ImageSize -> 300,
          (*intervalli sull'asse x da visualizzare*)
          Ticks -> {{0, Pi / 2, Pi, 3 Pi / 2, 2 Pi, 5 Pi / 2}, {-1, 0, 1}},
          (*titolo grafico*)
          PlotLabel -> "Funzione Seno",
          (*colore rosso coerente alla legenda*)
          PlotStyle -> {Red},
          PlotRange -> {{-1, 2 Pi + 1}, {-1.8, 1.8}}
        ],
        Graphics[{
          (*Punto dinamico sulla curve per identificare il rispettivo valore*)
          {Dashed, Line[{{x, 0}, {x, Sin[x]}}]},
          (*dimesione punto*)
           PointSize[Large],
          (*e relativa label*)
          Text[Style[Rationalize[Sin[x]], 20], {x + 0.5, Sin[x] - 0.5 }],
          (*disengo sul grafico del seno il punto calcolato*)
          Point[{ x, Sin[x] }]}
        ]
      ],
      (*COSENO*)
      Show[
      (*PLOT DELLA FUNZIONE COSENO nel periodo indicato*)

        Plot[Cos[y], {y, 0, 2 Pi},
          (*dimensione grafico*)
          ImageSize -> 300,
          (*intervalli sull'asse x da visualizzare*)
          Ticks -> {{0, Pi / 2, Pi, 3 Pi / 2, 2 Pi, 5 Pi / 2}, {-1, 0, 1}},
          (*titolo grafico*)
          PlotLabel -> "Funzione Coseno",
        (*colore verde coerente alla legenda*)
          PlotStyle -> {Green},
          PlotRange -> {{-1, 2 Pi + 1}, {-1.8, 1.8}}
        ],
        Graphics[{
          (*Punto dinamico sulla curve per identificare il rispettivo valore*)
          {Dashed, Line[{{x, 0}, {x, Cos[x]}}]},
          (*dimensione punto*)
          PointSize[Large],
          (*e relativa label*)
          Text[Style[Cos[x], 20], {x + 0.5, Cos[x] - 0.5}],
        (*disengo sul grafico del coseno il punto calcolato*)
          Point[{ x, Cos[x]}]}
        ]
      ]
    }]
  }],
  {{x, 0, "Naviga"}, 0, 2 Pi, Pi / 12},
  {{x, 0, "Scegli"}, 0, 2 Pi, Pi / 12},
  ControlType -> {Slider, PopupMenu}
];

(*es per il calcolo delle distanza tra due punti*)

EsDistanze[] := Quiet[
  (*quiet per nascondare eventuali warning*)
  DynamicModule[
    (*dichiarazioni di variabili da utilizzare*)
    {x1, y1, x2, y2, Esito = ""},
    (*controllo sulla risposta corretta*)
  checkRisp[risp_, correct_] := (
    If[risp == correct, Return["Risposta Corretta"], Return["Risposta Sbagliata"]]
  );
  (*calcolo indice del primo punto in maniera casuale*)
  random = RandomInteger[{1, Length[pointList]}];
  (*calcolo indice del secondo punto in maniera casuale*)
  randomTwo = RandomInteger[{1, Length[pointListTwo]}];
  (*get del primo punto in base all'indice*)
  P1 = pointList[[random]];
  (*get del secondo punto in base all'indice*)
  P2 = pointListTwo[[randomTwo]];
  (*punto 1*)
  x1 = P1[[1]];
  y1 = P1[[2]];
  (*punto 2*)
  x2 = P2[[1]];
  y2 = P2[[2]];
  Row[{
    Column[{
      Graphics[{ (*inzio graphics*)
        (*varie proiezioni Blu dei punti calcolati*)
        (*asse x punto 1*)
        {Thickness[0.0050], Blue, Dashed, Line[{{x1, 0}, {x1, y1}}]},
        (*asse y punto 1*)
        {Thickness[0.0050], Blue, Dashed, Line[{{0, y1}, {x1, y1}}]},
        (*asse x punto 2*)
        {Thickness[0.0050], Blue, Dashed, Line[{{x2, 0}, {x2, y2}}]},
        (*asse y punto 2*)
        {Thickness[0.0050], Blue, Dashed, Line[{{0, y2}, {x2, y2}}]},
        (*label del punto 1*)
        Text[Style["A", 20], {x1 - 0.4, y1 - 0.4}],
        (*label del punto 2*)
        Text[Style["B", 20], {x2 + 0.4, y2 + 0.4}],
        (*punto 1, point per idetificare visivamente*)
        {PointSize[Large], Red, Point[{x1, y1}]},
      (*punto 2, point per identificare visivamente*)
        {PointSize[Large], Red, Point[{x2, y2}]},
        (*linea tra i due punt*)
        {Thickness[0.0050], Green, Line[{{x1, y1}, {x2, y2}}]}
      },
        (*dichiaro quali intervalli vedere sugli assi*)
        GridLines -> {Table[point, {point, -10, 10, 1}], Table[point, {point, -10, 10, 1}]},
        Axes -> False,
        AxesStyle -> Thick, PlotRange -> 10,
        ImageSize -> 450,
        AxesLabel -> {"x", "y"},
        Ticks -> {
          Table[i, {i, -10, 10, 1}],
          Table[i, {i, -10, 10, 1}]
        }
      ](* Fine Graphics *)
    }], (*fine colonna grafico*)
    Column[{      (*spazio per la domanda e risposta*)

      (*esito delle risposta*)
      Style[Dynamic[Esito], 20],
      Row[{
        (*l'aiuto mostra il popup con la relativa formula*)
        Button["Aiuto",
          (*crea il popup*)
          CreateDialog[{
            (*scrittura della formula*)
            TextCell["La distanza tra due punti si calcola nel seguente modo: "HoldForm[Sqrt[("xA" - "xB")^2 + ("yA" - "yB")^2]]],
            DefaultButton[]
          }],
          ImageSize -> Medium, BaseStyle -> {"GenericButton"}
        ],
        (*permette di riutilzzare la stessa cella per ottenere lo stesso esercizio ma con punti diversi*)
        Button["Nuovo Esercizio", FrontEndExecute[FrontEndToken[NotebookLocate["esDistanza"], "Evaluate"]], ImageSize -> Medium, BaseStyle -> {"GenericButton"}]
      }],
      (*Calcolo del sengno dei punti*)
      If[x1 < 0, SigX1 = "-", SigX1 = ""];
      If[y1 < 0, SigY1 = "-", SigY1 = ""];
      If[x2 < 0, SigX2 = "-", SigX2 = ""];
      If[y2 < 0, SigY2 = "-", SigY2 = ""];
      (*stampa dei punti nella domanda*)
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

(*disegna un angolo positivo e negativo*)
AngoloOrientato[] := Row[{
  Column[{
    Graphics[{
      (*label dell'origine*)
      Text[Style["O", 15], {-0.1, 0}],
      (*semirette*)
      Line[{{0, 0}, {1, 1}}],
      Line[{{0, 0}, {1, -1}}],
      Line[{{0, 0}, {1, 0}}],
      (*arco rosso positivo*)
      {Red, Circle[{0, 0}, 0.5, {0, Pi / 4}]},
      (*arco blu positivo*)
      {Blue, Circle[{0, 0}, 0.5, {0, -Pi / 4}]},
      (*label angolo positivo*)
      Text[Style["\[Alpha] Angolo Positivo", 17, Red, Bold], {0.9, 0.25}],
      (*label angolo negativo*)
      Text[Style["\[Beta] Angolo Negativo", 17, Blue, Bold], {0.9, -0.25}]
    }, ImageSize -> 300]
  }],
  Column[{
    (*per distanziaare*)
    "\t"
  }], Column[{
    (*Teoria sugli angoli*)
    Style["Conviene collegare l'idea di angolo \na quella di rotazione di uno dei due lati.\nQuesta rotazione pu\[OGrave] avvenire in verso:\n", font, 17],
    Style["- Antiorario, e diciamo che \[EGrave] positivo;", font, 17],
    Style["- Orario, e diciamo che \[EGrave] negativo.", font, 17]
  }]

}];

(*eserzio per determinare le coordinate tra due punti*)
DisegnaPianoCartesiano[x_, y_] := DynamicModule[
  (*variabili utlizzate*)
  {
    (*colore per l'esito, red(sbagliato) , green(Giusto) *)
    colEsito = Black,
    (*text per l'esito*)
    esito = "",
    limit = 10,
    (*coordinate del punto*)
    xIn = 0,
    yIn = 0},
  Row[{
    Column[{
      Graphics[{
      (*punto da determinare di dimensione Large e colore rosso*)
        {PointSize[Large], Red, Point[{x, y}]}
      },
        (*grid*)
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
        (*bottono per l'aiuto*)
        Button["Aiuto",
          (*creazione del popup*)
          CreateDialog[{
            (*testo di suggerimento*)
            TextCell["L'asse delle X \[EGrave] la retta posta in posizione orizzontale mentre l'asse delle Y \[EGrave] in posizione verticale"],
            DefaultButton[]
          }],
          ImageSize -> Medium, BaseStyle -> {"GenericButton"}
        ],
        (*permette di utilizare la stessa cella e di ottenere lo stesso esercizio
        con un punto diverso*)
        Button["Nuovo Esercizio", FrontEndExecute[FrontEndToken[NotebookLocate["esCoordinate"], "Evaluate"]], ImageSize -> Medium, BaseStyle -> {"GenericButton"}]

      }],
      (*Stampa dell'esito e input per l'inserimento dei valori x ed y*)
      Dynamic[Text[Style[esito, colEsito, 20]]],
      "   x: "InputField[Dynamic[xIn], Number, FieldSize -> 5], "\n",
      "   y: "InputField[Dynamic[yIn], Number, FieldSize -> 5],

      (*button risolvi con conseguente controllo sulla risposta che modifica il core dell'esito*)
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
EsCoordinate[] := Quiet[

  DynamicModule[{},
  (*lista dei punti che vengono scelti in maniera casuale*)
    pointList = List[{3, 2}, {1, 3}, {-1, 1}, {-2, -5}, {-3, 8}, {-1, 3}, {-2, 1}, {-7, 3}, {2, 0}, {-9, 5}, {-10, 7}, {5, 2}, {-5, 4}, {-8, -5}, {2, 9}, {3, -6}];
    (*scelta indice del punto casuale*)
    randomPoint = RandomInteger[{1, Length[pointList]}];
    (*get del punto tramite l'indice*)
  P = pointList[[randomPoint]];
    (*disgna il grafico dando i punti calcolati come input*)
  DisegnaPianoCartesiano[P[[1]], P[[2]]]

]
];




(*utlizzato da salvo per la distanza tra due punt*)
(*dati  3 punti determina l'angolo individuato*)
getAngle[p1_, p2_, p3_] :=
    Return[ArcCos[
      (EuclideanDistance[p1, p2]^2 + EuclideanDistance[p1, p3]^2 - EuclideanDistance[p2, p3]^2) /
          (2 * EuclideanDistance[p1, p2] * EuclideanDistance[p1, p3]) ]];

(*utilizzato da antoc per l'altezza della torre*)
GetAngolo[alt_, bas_] := Return[N[ArcTan[alt / bas] / Degree ]];

(*applizione per determinare l'altezza della torre*)
AltezzaTorre[] :=
    Row[{
      Column[{
        Manipulate[DynamicModule[{},
          Graphics[{
            (*carimento dell'img della torre eiffel*)
            img = Import["./assets/torre.png"];
            (*linea per determianre il cateto minore dell'triangolo rettangolo*)
            {LightOrange, Thickness[0.01], Line[{{74, 0}, {74, -40}}]},
            (*disengo del triangolo rettangolo formatosi*)
            {LightRed, Triangle[{{x, -40}, {74, 50}, {74, -40}}]},
            (*inserimento della torre nelle relative coordinate*)
            Inset[img, {75, 7}],
          (*Punto osservatore*)
            {Blue, PointSize[Large], Point[{x, -40}]},
            (*linea di terra*)
            Line[{{-100, -40}, {150, -40}}],
            (*linea che identifica visivamente la distaza tra l'osservatore e la torre*)
            {Green, Thickness[0.01], Line[{{x, -40}, {74, -40}}]},
            (*calcolo della distanza tra la base della torre e l'osservatore*)
            dist := computeDistance[x, 0, 70, 0],
            (*calcolo angolo tra il punto osservatore e la punto della torre*)
            angolo := GetAngolo[90, dist],
            (*linea che congiunge punto ossevatore e cima della torre*)
            Line[{{x, -40}, {74, 50}}],
            (*varie text per visualizzare i passaggi*)
            Text[Style[StringForm["Altezza = Distanza x tan(``\[Degree]) = ``m", N[angolo], 300], 15], {-35, 35}],
            Text[Style[StringForm["\!\(\*
StyleBox[\"\[Alpha]\", \"TradFormChar\"]\): `` \[Degree]", N[angolo]], Red, 15], {-87, 25}],
            Text[Style[StringForm["Distanza: ``m", dist], Green, 15], {-83, 15}],
            {Thickness[0.001], Red, Circle[{x, -40}, 15, {0, angolo Pi / 180}]},
            Text[Style["\!\(\*
StyleBox[\"\[Alpha]\", \"TradFormChar\"]\)", Red, 20], {x + 20, -30}]
          },
            PlotRange -> {{-110, 100}, {-45, 50}},
            ImageSize -> 400
          ]
        ],
          (*range della manipulate*)
          {{x, -10, "Posizione"}, -100, 50, 1}
        ]
      }],
      Column[{
        "\t"
      }],
      Column[{
        (*testo per spiegazione dell'applicazione*)
        Text[Style["Lorenzo, dopo questa lettura, ha avuto un'idea\nsu come calcolare facilmente l'altezza della tour Eiffel.\nProcede in questo modo: \nmisura la sua distanza dalla base della torre\ne approssima l'angolo \[Alpha] (con un ottante).\nOsserva che si forma un triangolo rettangolo\ne pu\[OGrave] quindi usare la propriet\[AGrave] della tangente.", font, 20]]
      }]

    }];

DistancePointsApplication[] :=
(* Gestione della disposizione degli elementi attraverso l'uso di Colonne e Righe annidati *)
    Column[{
      Row[{
        Column[{
        (* Manipulate permette la manipolazione controllata degli elementi dinamici attraverso l'uso di appositi controlli *)
          Manipulate[
          (* Deploy consente di ottenere una versione dell'espressione valutata in cui elementi quali slider, locator,
            e button sono attivi ma in cui in genere non è consentito selezionare elementi o modificarli *)
            Deploy[
            (* DynamicMoudule rappresenta un oggetto che mantiene le istanze locali passate come argomenti durante
              la valutazione di tutti gli elementi dinamici presenti *)
              DynamicModule[
              (* Definizione delle coordinate iniziali dei tre vertici del triangolo ABC *)
                {
                  p1 = {-10, -10},
                  p2 = {10, 10},
                  p3 = {10, -10}
                },

              (* Rappresentazione di un ogetto grafico di tipo dinamico *)
                Dynamic@Graphics[
                  {
                  (* definizione dei punti di interpolazione per l'elemento river (disegno del fiume) *)
                    pts := {{-21, 0}, {-6, -1}, {10, 1}, {21, 0}},
                  (* calcolo della funzione interpolata sui punti definiti nell'array pts *)
                    riverFunction := Interpolation[pts],
                    riverPoints := {{-21, 0}, {-18, riverFunction[-18]}, {-16, riverFunction[-16]}, {-13, riverFunction[-13]},
                      {-10, riverFunction[-10]}, {-8, riverFunction[-8]}, {-6, -1}, {-3, riverFunction[-3]},
                      {0, riverFunction[0]}, {3, riverFunction[3]}, {7, riverFunction[7]}, {10, 1},
                      {12, riverFunction[12]}, {15, riverFunction[15]}, {18, riverFunction[18]}, {21, 0}},
                  (* rappresentazione grafica del fiume ottenuto calcolando la funzione interpolante *)
                    {Thickness[0.13], LightCyan, Line[riverPoints]},

                  (* definizione del vertice A del triangolo: attraverso l'uso di locator, tale punto è l'unico
                    libero di essere spostato all'interno dell'area del grafico *)
                    Point[p1],
                    Locator[Dynamic[p1], None],

                  (* Inset ci permette di inserire un oggetto in un Graphocs
                    In questo caso lo si è scelto per effettuare la valutazione dell'espressione che determina i
                    limiti di movimento del punto A: si vuole che il punto A rimanga al di sotto della riva del
                    fiume, dunque, ad ogni suo spostamento si valuta la funzione interpolata del fiume sull'ascissa
                    del punto A, se il valore ottenuto è oltre la riva del fiume, si blocca il punto nella posizone
                    non consentendone la variazione dell'ordinata *)
                    Inset[
                      With[
                        {x = First[p1], y = Last[p1]},
                        If[y >= riverFunction[x] - 2.4, {p1 = {x, riverFunction[x] - 2.4}}]
                      ],
                      {-30, -30}
                    ],

                  (* rappresentazione dei vertici B e C del triangolo *)
                    {Red, Point[p2]},
                    {Red, Point[p3]},

                  (* Si effettuano dei controlli sui valori assunti dall'ascissa del punto e dalla sua ordinata ad
                    ogni suo spostamento: a seconda della sua posizione relativa rispetto agli altri vertici del
                    triangolo, si effettuano le dovute considerazione per la rappresentazione corretta degli angoli
                    nei tre vertici del triangolo *)
                    If[First[p1] <= First[p3],
                      If[Last[p1] <= Last[p3],
                        {LightBlue, Disk[p1, 3, {0 + getAngle[p1, p3, {First[p3], Last[p1]}], getAngle[p1, p2, p3] + getAngle[p1, p3, {First[p3], Last[p1]}]}]},
                        {LightBlue, Disk[p1, 3, {0 - getAngle[p1, p3, {First[p3], Last[p1]}], getAngle[p1, p2, p3] - getAngle[p1, p3, {First[p3], Last[p1]}]}]}],

                      If[Last[p1] <= Last[p3],
                        {LightBlue, Disk[p1, 3, {Pi / 2 + getAngle[p1, p2, {First[p1], Last[p2]}], Pi / 2 + getAngle[p1, p2, p3] + getAngle[p1, p2, {First[p1], Last[p2]}]}]},

                        If[Last[p1] <= Last[p2],
                          {LightBlue, Disk[p1, 3, {Pi / 2 + getAngle[p1, p2, {First[p1], Last[p2]}], Pi / 2 + getAngle[p1, p2, p3] + getAngle[p1, p2, {First[p1], Last[p2]}]}]},
                          {LightBlue, Disk[p1, 3, {Pi + getAngle[p1, p2, {First[p2], Last[p1]}], Pi + getAngle[p1, p2, p3] + getAngle[p1, p2, {First[p2], Last[p1]}]}]}]]
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

                  (* elenco di testi visualizzati nel grafico *)
                    Style[Text["Prova a spostare il punto A nel piano", {0, 18}], FontSize -> 16],
                    Style[Text["A", {First[p1] - 1, Last[p1] - 1}], FontSize -> 18],
                    Style[Text["C", {First[p3] + 1, Last[p3] - 1}], FontSize -> 18],
                    Style[Text["B", {First[p2] + 1, Last[p2] + 1}], FontSize -> 18],
                    Style[Text["\[Alpha]", {First[p1] + 3.5, Last[p1] + 1.5}], FontSize -> 18, Blue],
                    Style[Text["\[Beta]", {First[p3] - 3, Last[p3] + 1.5}], FontSize -> 18, Red],
                    Style[Text["\[Gamma]", {First[p2] - 1.5, Last[p2] - 3}], FontSize -> 18, Gray],
                    Style[Text[StringForm["AC =  ``", SetPrecision[EuclideanDistance[p1, p3], 4]], {-10, -16}], FontSize -> 14],
                    Style[Text[StringForm["AB =  ``", SetPrecision[EuclideanDistance[p1, p2], 4]], {-10, -18}], FontSize -> 14],
                    Style[Text[StringForm["CAB = \[Alpha] =  ``\[Degree]", SetPrecision[N[getAngle[p1, p2, p3] 180 / Pi], 4]], {4, -16}], FontSize -> 14],
                    Style[Text[StringForm["BCA = \[Beta] = ``\[Degree]", SetPrecision[N[getAngle[p3, p1, p2] 180 / Pi], 4]], {4, -18}], FontSize -> 14],

                  (* rappresentazione dei lati del triangolo *)
                    {Thickness[0.005], Line[{p1, p2}]},
                    {Thickness[0.005], Blue, Line[{p1, p3}]},
                    {Thickness[0.005], Red, Line[{p2, p3}]},

                    {Red, Point[p2]},
                    {Red, Point[p3]}
                  },
                (* definizione dei limiti del grafico *)
                  PlotRange -> {{-16, 20}, {-20, 20}}
                ]
              ]
            ]
          ]
        }],
      (* testo di spiegazione dell'applicazione *)
        Text["    "],
        Column[{
          Row[{
            Text[Style["\n\nSupponiamo di voler calcolare la distanza fra due punti A e B:\nio mi trovo in A ma non posso raggiungere B perch\[EAcute] \[EGrave] al di l\[AAcute]  del fiume.", font, 20]]
          }],
          Row[{
            Text[Style["Possiamo spostarci in un punto C e calcolare la distanza AC\ned inoltre gli angoli CAB=\[Alpha] e ACB=\[Beta].", font, 20]]
          }],
          Text["\n"],
          Row[{
            Style[Text["Abbiamo quindi il triangolo ABC in cui conosciamo due angoli ed il lato compreso,\nquindi per risolvere il triangolo possiamo calcolare il terzo angolo ricordando che\nla somma degli angoli interni di un triangolo \[EGrave] un angolo piatto.\n"], font, 20]
          }],
          Row[{
            Style[HoldForm["\[Gamma] = 180\[Degree] - \[Alpha] - \[Beta]"], 20, Bold, Blue]
          }],
          Row[{
            Style[Text["\nE poi applicare il teorema dei seni:  "], font, 20]
          }],
          Row[{
            Style[
              StringForm["\n `` = ``",
                HoldForm["AC" / "Sen(\[Gamma])"],
                HoldForm["BC" / "Sen(\[Beta])"]
              ], font, 20, Bold, Blue]
          }],
          Row[{
            Style[Text["\nOttenendo quindi:\n"], font, 20, font]
          }],
          Row[{
            Style[
              StringForm[" BC = ``",
                HoldForm["AC Sen(\[Beta])" / "Sen(\[Gamma])" ]
              ], 20, Bold, font, Blue]
          }]
        }]
      }],
      Row[{
        Column[{
          Row[{
            Style[Text["Esempio\n"], 18]
          }],
          Row[{
            Style[Text["AC = 20m"], 15]
          }],
          Row[{
            Style[Text["CAB = \[Alpha] = 45\[Degree]"], 15]
          }],
          Row[{
            Style[Text["BCA = \[Beta] = 90\[Degree]"], 15]
          }]
        }],
        Text["\t"],
        Column[{
          Row[{
            Style[Text["\nQuanto misura BC?"], 15]
          }],
          Row[{
            Style[Text[" "], 15]
          }],
          Row[{
            Column[{
              Row[{
                Button["12m", answer := "Risposta Errata!"]
              }]
            }],
            Column[{
              Row[{
                Button["18m", answer := "Risposta Errata!"]
              }]
            }],
            Column[{
              Row[{
                Button["21m", answer := "Risposta Corretta!"]
              }]
            }],
            Column[{
              Row[{
                Button["28m", answer := "Risposta Errata!"]
              }]
            }]
          }],
          Row[{
            Style[Text[""], 15],
            answer := "",
            Style[Dynamic[answer], 15]
          }]
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
    Button["Nuovo Esercizio", FrontEndExecute[FrontEndToken[NotebookLocate["esGradRad"], "Evaluate"]], ImageSize -> Medium, BaseStyle -> {"GenericButton"}],
    Dynamic[Text[Style[esito, colorEsito, 20]]],

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

puntiInaccessibili[] := DynamicModule[{p1 = {-2, -1}, p2 = {-0.5, 1}, p3 = {2, 1}, p4 = {0.5, -1}, Esito = ""},

  checkRisp[risp_, correct_] := (
    If[risp == correct, Return[Style["Risposta Corretta", Green, FontFamily -> Roboto]], Return[Style["Risposta Errata", Red, FontFamily -> Roboto]]]
  );

  esatta = 15;
  (* dynamic; *)
  Row[{
    Column[{
      Row[{
        Manipulate[
          Deploy[
            DynamicModule[{},
              Dynamic@Graphics[{

                upperGrass = FilledCurve[{Line[{{-3, 0.1}, {-3, 3}}], Line[{{3, 3}, {3, 0.6}}] }];
                lowerGrass = FilledCurve[{Line[{{-3, -0.1}, {-3, -3}}], Line[{{3, -3}, {3, 0.1}}]}];
                {Opacity[.3], Green, upperGrass},
                {Opacity[.3], Green, lowerGrass},

              (* Fiume *)
                {Thickness[0.005], Darker[LightBlue], BezierCurve[{ {-3, 0.3}, {-0.8, -0.1}, {0.5, 0.8}, {3, 0.6}}]},
                {Thickness[0.005], Darker[LightBlue], BezierCurve[{ {-3, -0.3}, {-0.8, -0.7}, {0.5, 0.2}, {3, 0}}]},
                river = BezierCurve[{ {-3, 0}, {-0.8, -0.4}, {0.5, 0.5}, {3, 0.3}}];
                {Thickness[0.1], LightBlue, river},

              (* Parallelogramma *)
                l1 = {Thickness[0.004], Red, Line[{p1, p2}]},
                l2 = {Thickness[0.004], Blue, Line[{p2, p3}]},
                l3 = {Thickness[0.004], Orange, Line[{p3, p4}]},
                l4 = {Thickness[0.004], Line[{p4, p1}]}, (* AC *)
              (* Diagonali *)
                d1 = {Thickness[0.004], Orange, Line[{p1, p3}]},
                d2 = {Thickness[0.004], Red, Line[{p2, p4}]},

                {PointSize[0.012], Point[p1], Locator[Dynamic[p1], None]},
                {PointSize[0.012], Point[p4], Locator[Dynamic[p4], None]},
                Inset[
                  {With[{x = Last[p1]}, If[x >= -0.4, {p1 = {First[p1], -0.5}}]],
                    With[{y = First[p1]}, If[y >= First[p4], {p1 = {First[p4] - 0.3, Last[p1]}}]],
                    With[{w = Last[p4]}, If[w >= -0.4, {p4 = {First[p4], -0.5}}]],
                    With[{z = First[p4]}, If[z <= First[p1], {p4 = {First[p1] - 0.3, Last[p4]}}]]},
                  {-5, -5}
                ],
                Style[Text["A", {First[p4], Last[p4] - 0.2}], 15],
                Style[Text[Subscript["\[Alpha]", 1], {First[p4], Last[p4] + 0.4}], 15],
                Style[Text[Subscript["\[Alpha]", 2], {First[p4] - 0.2, Last[p4] + 0.12}], 12],
                Style[Text["C", {First[p1], Last[p1] - 0.2}], 15],
                Style[Text[Subscript["\[Gamma]", 1], {First[p1] + 0.4, Last[p1] + 0.1}], 12],
                Style[Text[StringForm["``", Subscript["\[Gamma]", 2]], {First[p1] + 0.6, Last[p1] + 0.5}], 15],
                {PointSize[0.012], Point[p2], Locator[Dynamic[p2], None, Enabled -> False]},
                Style[Text["\[Beta]", {First[p2] - 0.03, Last[p2] - 0.3}], 15],
                Style[Text["B", {First[p2], Last[p2] + 0.2}], 15],
                {PointSize[0.012], Point[p3], Locator[Dynamic[p3], None, Enabled -> False]},
                Style[Text["D", {First[p3], Last[p3] + 0.2}], 15],
                Style[Text["\[Delta]", {First[p3] - 0.35, Last[p3] - 0.3}], 15],

              (* Visualizzazione ampiezza degli angoli *)
                Style[Text[StringForm["DCA = `` = ``", Subscript["\[Gamma]", 1], Round[N[getAngle[p1, p3, p4]] * 180 / Pi]], {1, -2.5}], FontFamily -> Roboto, FontSize -> 18],
                Style[Text[StringForm["BCA = `` = ``", Subscript["\[Gamma]", 2], Round[N[getAngle[p1, p2, p4]] * 180 / Pi]], {1, -2.75}], FontFamily -> Roboto, FontSize -> 18],

                Style[Text[StringForm["CAD = `` = ``", Subscript["\[Alpha]", 1], Round[N[getAngle[p4, p3, p1]] * 180 / Pi]], {-1, -2.5}], FontFamily -> Roboto, FontSize -> 18],
                Style[Text[StringForm["BAC = `` = ``", Subscript["\[Alpha]", 2], Round[N[getAngle[p4, p1, p2]] * 180 / Pi]], {-1.025, -2.75}], FontFamily -> Roboto, FontSize -> 18],

              (* Alpha 1 *)
                If[First[p4] <= First[p3],
                  Circle[p4, 0.5,
                    {
                      0 + getAngle[p4, p3, {First[p3], Last[p4]}],
                      0 + getAngle[p4, p3, {First[p3], Last[p4]}] + getAngle[p4, p2, p3] + getAngle[p4, p1, p2]
                    }
                  ],
                  Circle[p4, 0.5,
                    {
                      0 + getAngle[p4, p3, {3, Last[p4]}],
                      0 + getAngle[p4, p3, {3, Last[p4]}] + getAngle[p4, p2, p3] + getAngle[p4, p1, p2]
                    }
                  ]
                ],

              (* Alpha 2 *)
                If[First[p4] <= First[p3],
                  Circle[p4, 0.35, {
                    0 + getAngle[p4, p3, {First[p3], Last[p4]}] + getAngle[p4, p2, p3],
                    0 + getAngle[p4, p3, {First[p3], Last[p4]}] + getAngle[p4, p2, p3] + getAngle[p4, p1, p2]
                  }
                  ],
                  Circle[p4, 0.35, {
                    0 + getAngle[p4, p3, {3, Last[p4]}] + getAngle[p4, p2, p3],
                    0 + getAngle[p4, p3, {3, Last[p4]}] + getAngle[p4, p2, p3] + getAngle[p4, p1, p2]
                  }
                  ]

                ],

              (* Gamma 1 *)
                If[Last[p1] <= Last[p4],
                  If[First[p1] <= First[p2],
                    Circle[p1, 0.35, {
                      0 + getAngle[p1, p4, {First[p2], Last[p1]}],
                      0 + getAngle[p1, p4, {First[p2], Last[p1]}] + getAngle[p1, p4, p3]
                    }
                    ],
                    Circle[p1, 0.35, {
                      0 + getAngle[p1, p4, {3, Last[p1]}],
                      0 + getAngle[p1, p4, {3, Last[p1]}] + getAngle[p1, p4, p3]
                    }
                    ]
                  ],
                  Circle[p1, 0.35,
                    {
                      3 / 2 * Pi + getAngle[p1, p4, {First[p1], -3}],
                      3 / 2 * Pi + getAngle[p1, p4, {First[p1], -3}] + getAngle[p1, p3, p4]
                    }
                  ]
                ],

              (* Gamma 2 *)

                If[Last[p1] <= Last[p4],
                  If[First[p1] <= First[p2],
                    Circle[p1, 0.5, {
                      0 + getAngle[p1, p4, {First[p2], Last[p1]}],
                      0 + getAngle[p1, p4, {First[p2], Last[p1]}] + getAngle[p1, p4, p2]
                    }
                    ],
                    Circle[p1, 0.5, {
                      0 + getAngle[p1, p4, {3, Last[p1]}],
                      0 + getAngle[p1, p4, {3, Last[p1]}] + getAngle[p1, p4, p2]
                    }
                    ]
                  ],
                  Circle[p1, 0.5,
                    {
                      3 / 2 * Pi + getAngle[p1, p4, {First[p1], -3}],
                      3 / 2 * Pi + getAngle[p1, p4, {First[p1], -3}] + getAngle[p1, p2, p4]
                    }
                  ]
                ]

              },
                ImageSize -> 500, PlotRange -> 3]
            ]
          ]
        ]
      }]
    }],
    Text["\t"],
    Column[{
      Row[{
        Style[Text["Determinare la distanza tra due punti B e D entrambi inaccessibili.\nSpostandoci da A a B possiamo considerare ADC ed ABC."], FontFamily -> Roboto, FontSize -> 20]
      }],
      Text["\n"],
      Row[{
        Column[{
          Style[Text["Di ADC conosciamo:\n- la misura di AC\n- l'angolo DAC = \[Alpha]1\n- l'angolo DCA = \[Gamma]1\n"], FontFamily -> Roboto, FontSize -> 20]
        }],
        Text["\t\t"],
        Column[{
          Style[Text["Di ABC conosciamo:\n- la misura di AC\n- l'angolo BAC = \[Alpha]2\n- l'angolo BCA = \[Gamma]2\n"], FontFamily -> Roboto, FontSize -> 20]
        }]
      }],
      Row[{
        Button["Calcolare \[Delta]=CDA e \[Beta]=CBA",
          CreateDialog[{
            TextCell["L'angolo \[Delta]=CDA si ricava dalla differenza \ntra la somma totale degli angoli interni \ne i due angoli dati \n(stessa cosa vale per \[Beta]=CBA)"],
            DefaultButton[]
          }],
          ImageSize -> Medium, BaseStyle -> {"GenericButton"}
        ],
        Text["   "],
        Button["Calcolare AD e AB",
          CreateDialog[{
            TextCell["AD e AB si ricavano applicando il teorema dei seni:\n\tAD = (AC sin\[Gamma]1)/sin\[Delta]\n\tAB = (AC sin\[Gamma])/sin\[Beta]"],
            DefaultButton[]
          }],
          ImageSize -> Medium, BaseStyle -> {"GenericButton"}
        ],
        Text["   "],
        Button["Calcolare BD",
          CreateDialog[{
            TextCell["È possibile calcolare la lunghezza di BD applicando Carnot:\n\n\tBD = \[Sqrt]AB+AD-2*AB*AD*cos(α1-α2)"],
            DefaultButton[]
          }],
          ImageSize -> Medium, BaseStyle -> {"GenericButton"}
        ],
        Text["\n"]
      }],
      Row[{
        Column[{
          Row[{
            Style[Text["Esempio"], FontFamily -> Roboto, 20]
          }],
          Row[{
            Style[Text["- AC = 20m"], FontFamily -> Roboto, 15]
          }],
          Row[{
            Style[Text[StringForm["- CAD = `` = 100\[Degree]", Subscript["\[Alpha]", 1]]], FontFamily -> Roboto, 15]
          }],
          Row[{
            Style[Text[StringForm["- DCA = `` = 50\[Degree]", Subscript["\[Gamma]", 1]]], FontFamily -> Roboto, 15]
          }],
          Row[{
            Style[Text[StringForm["- BAC = `` = 60\[Degree]", Subscript["\[Alpha]", 2]]], FontFamily -> Roboto, 15]
          }],
          Row[{
            Style[Text[StringForm["- BCA = `` = 70\[Degree]", Subscript["\[Gamma]", 2]]], FontFamily -> Roboto, 15]
          }]
        }],
        Text["\t\t"],
        Column[{
          Row[{
            Style[Text[""], FontFamily -> Roboto, 15]
          }],
          Row[{
            Style[Text["Quanto misura BD?"], FontFamily -> Roboto, 15]
          }],
          Row[{
            Style[Text[" "], FontFamily -> Roboto, 15]
          }],
          Row[{
            Column[{
              Row[{
                Button["10m", Esito := checkRisp[10, esatta]]
              }]
            }],
            Column[{
              Row[{
                Button["15m", Esito := checkRisp[15, esatta]]
              }]
            }],
            Column[{
              Row[{
                Button["25m", Esito := checkRisp[25, esatta]]
              }]
            }],
            Column[{
              Row[{
                Button["40m", Esito := checkRisp[40, esatta]]
              }]
            }]
          }],
          Row[{
            Style[Text[""], FontFamily -> Roboto, 15]
          }],
          Style[Dynamic[Esito], 20],
          Row[{
            Style[Text[""], FontFamily -> Roboto, 15]
          }]
        }]
      }]
    }]
  }]
]



(* ::InheritFromParent:: *)
(**)


(* ::InheritFromParent:: *)
(**)


(* ::InheritFromParent:: *)
(**)
End[]; (* fine sezione privata *)
EndPackage[]; (* Fine del Package *)