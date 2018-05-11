(* ::Package:: *)

(* ::Input:: *)
(*(* PACKAGE.M*)
(* * Progetto d'esame di Matematica Computazionale + Calcolo Numerico e Software Didattico*)
(* * Corsi di laurea magistrale in Informatica e Matematica*)
(* * Anno accademico 2017/2018*)
(* *)
(* * Autori:*)
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


drawCircle::usage = "disegna un grafico";
drawCartesian::usage = "disegna assi"
startGame::usage = "gioco del quadrante";
distanceGame::usage = "calcola la distanza distanza tra due punti";
es4::usage = "secante/tangente";
Begin["`Private`"]; (* Comincia spazio privato *)

FromRadToGrad[x_] := Return[x*180/Pi];
FromGradToRad[x_] := Return[N[Pi*x/180]];
getAngle[p1_,p2_] := Mod[ArcTan @@ (p2 - p1), 2 Pi];  
pointList = List[{3,2},{1,3},{-1,1},
				{-2,-5},{-3,8},{-1,3},
				{-2,1},{-7,3},{-10, 1}, {5,2}];


(* ::InheritFromParent:: *)
(**)


drawCartesian[x_,y_]:= DynamicModule[{colEsito=Black, esito="",limit = 10, xIn=0, yIn=0},
Row[{ 
	q1 = {limit,limit};
	q2 = {-limit,limit};
	q3 = {-limit,-limit};
	q4 = {limit,-limit};
	Column[{
	Graphics[{
		Style[Rectangle[{0,0},q1],White], 
		Style[Rectangle[{0,0},q2],White],
		Style[Rectangle[{0,0},q3],White],
		Style[Rectangle[{0,0},q4],White],
		{PointSize[Large],Red,Point[{x,y}]}
	},	
	Axes->True,
	ImageSize->450,
	AxesLabel->{"x","y"},
	Ticks->{
		Table[i,{i,-limit, limit, 1}],
		Table[i,{i,-limit, limit, 1}]
		}
	](* Fine Graphics *)
	}],(*Fine Column*)
	Column[{
	Dynamic[Text[Style[esito,colEsito, 20]]],
	"   x: "InputField[Dynamic[xIn],Number,FieldSize->5], "\n",
	"   y: "InputField[Dynamic[yIn],Number,FieldSize->5],
	Button["Risolvi", 
		If[xIn == x && yIn ==y,
			(*IF*)
		  {esito:="Corretto"; colEsito:=Green}, 
		   (*ELSE *)
		  {esito:="Sbagliato"; colEsito:=Red }] 
	]}]
	}]Fine Row
];

startGame[] := DynamicModule[{},
	 random = RandomInteger[{1,Length[pointList]}];
	 P = pointList[[random]];
	 drawCartesian[P[[1]],P[[2]]];
   es4[]
];

GetQuad[x0_,y0_] := DynamicModule[{x=x0, y=y0, quad},
	If[x>=0&&y>=0, quad=1];
	If[x<0&&y>=0, quad=2];
	If[x<=0&&y<=0, quad=3];
	If[x>=0&&y<0, quad=4];
quad];

drawCircle[] := Manipulate[
  Row[{
    Graphics[{
      Circle[],
      Point[{0,0}],
      PointSize[Large],
      Circle[{0,0},0.2,{0,x}],
      {Dashed, Line[{{0,0},{Cos[x],Sin[x]}}]},(*raggio*)
      Thickness[0.01],
      RGBColor[0,255,0],
      Line[{{0,0},{Cos[x],0}}],
      RGBColor[255,0,0],
      Line[{{Cos[x],0},{Cos[x],Sin[x]}}],
      Text[Style[IntegerString[FromRadToGrad[x]] <> "\[Degree]",Large,Red],{-0.90,1}],
      Text[Style["Seno",Medium,Red],{0.8,1}],
      Text[Style["Coseno",Medium,Green],{0.8,0.9}],

      {Black,PointSize ->.02, Point[{Cos[x], Sin[x]}]}},
      ImageSize->350,
      Axes->True
    ] (*FINE GRAPHICS*),
    Column[{
        (* GRAFICI SENO E COSENO *)
        Show[
          Plot[Sin[y], {y, 0,  2 Pi}, 
            ImageSize->300, 
            Ticks -> {{0, Pi/2, Pi, 3 Pi/2, 2 Pi, 5 Pi/2}, {-1, 0, 1}},
            PlotLabel -> "Sine function",
            PlotStyle -> {Orange}
          ], 
          Graphics[{ 
            {Dashed, Line[{{x,0},{x,Sin[x]}}]},
            PointSize[Large],
            Point[{ x, Sin[x] }]}
          ]
        ],
        Show[
          Plot[Cos[y], {y, 0,  2 Pi}, 
            ImageSize->300,
            Ticks -> {{0, Pi/2, Pi, 3 Pi/2, 2 Pi, 5 Pi/2}, {-1, 0, 1}},
            PlotLabel -> "Cosine function",
            PlotStyle -> {Blue}
          ],
          Graphics[{
            {Dashed, Line[{{x,0},{x,Cos[x]}}]},
            PointSize[Large],
            Point[{ x, Cos[x]}]}
          ]
        ]
    }]
  }],
  {{x,0,"Angolo"}, 0, 2 Pi, 2 Pi/360}
];

secantList  = List[ {{0,-20}, {18,18}}, {{-20,9}, {18,6}}, {{-10,-13}, {6,20}} ];

tangentList = List[ {{-20,0}, {15,19.7}}, {{14,-20}, {14,19.7}}, {{-6.075,-20}, {-6.075,20}} ];

outList = List[ {{-17,-11.1}, {20,-11.1}}, {{-20,16}, {20,16}}, {{-7,-10}, {-10,20}} ];

es4[] := DynamicModule[{},

      random1 = RandomInteger[{1,Length[secantList]}];
      random2 = RandomInteger[{1,Length[tangentList]}];
      random3 = RandomInteger[{1,Length[outList]}];

      R1 = secantList[[random1]];
      R2 = tangentList[[random2]];
      R3 = outList[[random3]];
      
      Column[{
        Row[{
          Text[Style["Si considerino le rette in figura e si individui, la posizione che ciascuna retta assume rispetto alla circonferenza.", FontFamily -> "Roboto", 20]],
          Graphics[{
            { ColorData[1,6], Thick, Circle[{4,2}, 10] }, (* Circonferenza Viola *)
            { ColorData[2,1], Thick, Line[R1] }, (* Secante Rossa *)
            { ColorData[1,1], Thick, Line[R3] }, (* Esterna Blu *)
            { ColorData[1,8], Thick, Line[R2]} }, (* Tangente Verde *) 
            Axes -> True,
            ImageSize -> 450, 
            PlotRange -> 20,
            AxesLabel->{"x","y"},
            Ticks->{
              Table[i,{i,-20, 20, 2}],
              Table[i,{i,-20, 20, 2}]
            }
          ],
          Text[Style["\t"]],
          Column[{
            wrong1 = Row[{
              btn1 = Button[Text[">"], Print[Text["Risposta errata...", BaseStyle -> {RGBColor[1,0,0], FontSize -> 20}]]],
              Text["\t"],
              Text[" Tangente ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[1,8]],
              Text["\t"],
              Text["   Esterna  ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[2,1]], 
              Text["\t"],
              Text["  Secante  ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[1,1]],
              Text["\n\n"]
            }];
            wrong2 = Row[{
              btn2 = Button[">", Print[Text["Risposta errata...", BaseStyle -> {RGBColor[1,0,0], FontSize -> 20}]]],
              Text["\t"],
              Text["  Secante  ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[1,8]],
              Text["\t"],
              Text[" Tangente ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[2,1]],
              Text["\t"],
              Text["  Esterna  ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[1,1]],
              Text["\n\n"]
            }];
            wrong3 = Row[{
              btn3 = Button[">", Print[Text["Risposta errata...", BaseStyle -> {RGBColor[1,0,0], FontSize -> 20}]]]
              Text["\t"],
              Text["  Esterna   ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[1,8]],
              Text[" \t"],
              Text[" Tangente ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[2,1]],
              Text["\t"],
              Text["  Secante  ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[1,1]],
              Text["\n\n"]
            }];
            right = Row[{
              btn4 = Button[">", Print[Text["Risposta corretta!!!", BaseStyle -> {RGBColor[0,1,0], FontSize -> 20}]]]
              Text["\t"],
              Text[" Tangente ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[1,8]],
              Text["\t"],
              Text["  Secante  ", BaseStyle ->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[2,1]],
              Text["\t"],
              Text["  Esterna  ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[1,1]],
              Text["\n\n"]
            }];
            rand1 = {wrong2, wrong1, right, wrong3};
            rand2 = {wrong3, wrong2, right, wrong1};
            rand3 = {wrong1, wrong3, wrong2, right};
            rand4 = {wrong2, wrong1, right, wrong3};
            rand5 = {wrong1, wrong2, wrong3, right};
            answerRows = List[rand1, rand2, rand3, rand4, rand5];
            zz = RandomInteger[{1,5}];
          
            Column[answerRows[[zz]]],
            Row[{
              Button["Aiuto",
                CreateDialog[{
                  TextCell["Data una retta r e una circonferenza c, diciamo che: \n > r è esterna a c se r \[Intersection] c = \[EmptySet] \n > r è esterna a c se r \[Intersection] c = P \n > r è esterna a c se r \[Intersection] c = {P, \[EmptySet]}"],
                  DefaultButton[]
                }],
                ImageSize->Medium,BaseStyle->{"GenericButton"}
              ]
            }],
            Row[{
              Button["Nuovo Esercizio",FrontEndExecute[FrontEndToken[NotebookLocate["es4"],"Evaluate"]],ImageSize->Medium,BaseStyle->{"GenericButton"}]
		        }]
          }]
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
