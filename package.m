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
Begin["`Private`"]; (* Comincia spazio privato *)

FromRadToGrad[x_] := Return[x*180/Pi];
FromGradToRad[x_] := Return[N[Pi*x/180]];
getAngle[p1_,p2_] := Mod[ArcTan @@ (p2 - p1), 2 Pi];  
pointList = List[{3,2},{1,3},{-1,1},{-2,-5},{-3,8},{-1,3},{-2,1},{-7,3}];
pointListTwo = List[{2,0},{-9,5},{-10,7},{5,2},{-5,4}, {-8,-5},{2,9},{3,-6}];


(* ::InheritFromParent:: *)
(**)


computeDistance[x1_,y1_,x2_,y2_] := Return[Sqrt[(x2 - x1)^2+(y2 - y1)^2]];

distanceGame[]:= DynamicModule[{x1,y1,x2,y2,Esito=""},
	checkRisp[risp_, correct_] := (
		If[risp == correct, Return["Risposta Corretta"], Return["Risposta Sbagliata"]]
	);
	random = RandomInteger[{1,Length[pointList]}];
	randomTwo = RandomInteger[{1,Length[pointListTwo]}];
	P1 = pointList[[random]];
	P2 = pointListTwo[[randomTwo]];
	x1 = P1[[1]];
	x2 = P2[[1]];
	y1 = P1[[2]];
	y2 = P2[[2]];
	Row[{
		Column[{
			Graphics[{
				{Dashed,Line[{{x1,0},{x1,y1}}]},
				{Dashed,Line[{{0,y1},{x1,y1}}]},
				{Dashed,Line[{{x2,0},{x2,y2}}]},
				{Dashed,Line[{{0,y2},{x2,y2}}]},
				Text[Style["A",20],{x1-0.4,y1-0.4}],
				Text[Style["B",20],{x2+0.4,y2+0.4}],
				{PointSize[Large],Red,Point[{x1,y1}]},
				{PointSize[Large],Red,Point[{x2,y2}]},
				 Line[{{x1,y1},{x2,y2}}]
	},	
	Axes->True,
	AxesStyle->Thick, PlotRange->10,
	ImageSize->450,
	AxesLabel->{"x","y"},
		Ticks->{
		Table[i,{i,-10, 10, 1}],
		Table[i,{i,-10, 10, 1}]
		}
	](* Fine Graphics *)
		}],(*fine colonna grafico*)
		Column[{
		
		Style[Dynamic[Esito],20],
		Row[{
				Button["Aiuto",
			CreateDialog[{
				TextCell["La distanza tra due punti si calcola nel seguente modo: "HoldForm[Sqrt[("xA" - "xB")^2 + ("yA" - "yB")^2]]],
				DefaultButton[]
			}],
			ImageSize->Medium,BaseStyle->{"GenericButton"}
		],
		Button["Nuovo Esercizio",FrontEndExecute[FrontEndToken[NotebookLocate["esDistanza"],"Evaluate"]],ImageSize->Medium,BaseStyle->{"GenericButton"}]
		}],	
		If[x1<0, SigX1="-", SigX1=""];
		If[y1<0, SigY1="-", SigY1=""];
		If[x2<0, SigX2="-", SigX2=""];
		If[y2<0, SigY2="-", SigY2=""];
		A = "("<>SigX1<>IntegerString[x1]<>","<>SigY1<>IntegerString[y1]<>")";
		B = "("<>SigX2<>IntegerString[x2]<>","<>SigY2<>IntegerString[y2]<>")";
		Style["Quanto distano tra di loro i punti A"<>A <>" e B"<>B<>":",20],
		giusta = computeDistance[x1,y1,x2,y2];
		risp1 =  RandomInteger[{1,10}];
		risp2 =  RandomInteger[{11,20}];
		risp3 =  RandomInteger[{21,30}];
		risp4 = giusta;
		Opzioni = {risp1, risp2, risp3, N[risp4]};
		RandomOptions = Sort[Opzioni];
		If[N[giusta]==RandomOptions[[1]], RandomOptions[[1]] = giusta;],
		If[N[giusta]==RandomOptions[[2]], RandomOptions[[2]] = giusta; ],
		If[N[giusta]==RandomOptions[[3]], RandomOptions[[3]]= giusta;],
		If[N[giusta]==RandomOptions[[4]], RandomOptions[[4]] = giusta;],		
		btn1 = Button[Style[NumberForm[RandomOptions[[1]]],20], Esito:=checkRisp[RandomOptions[[1]],giusta]];
		btn2 = Button[Style[NumberForm[RandomOptions[[2]]],20], Esito:=checkRisp[RandomOptions[[2]],giusta]];
		btn3 = Button[Style[NumberForm[RandomOptions[[3]]],20], Esito:=checkRisp[RandomOptions[[3]],giusta]];
		btn4 = Button[Style[NumberForm[RandomOptions[[4]]],20], Esito:=checkRisp[RandomOptions[[4]],giusta]];
		btn1, btn2, btn3, btn4
		}] (*fine colonna input*)
	}]
];




drawCartesian[x_,y_]:= DynamicModule[{colEsito=Black, esito="",limit = 10, xIn=0, yIn=0},
Row[{ 
    q1 = {limit,limit};
	q2 = {-limit,limit};
	q3 = {-limit,-limit};
	q4 = {limit,-limit};
	Column[{
	Graphics[{
		{PointSize[Large],Red,Point[{x,y}]}
	},	
	Axes->True,
	AxesStyle->Thick, PlotRange->10,
	ImageSize->450,
	AxesLabel->{"x","y"},
	Ticks->{
		Table[i,{i,-limit, limit, 1}],
		Table[i,{i,-limit, limit, 1}]
		}
	](* Fine Graphics *)
	}],(*Fine Column*)
	Column[{
	
	Row[{
			Button["Aiuto",
			CreateDialog[{
				TextCell["L'asse delle X \[EGrave] la retta posta in posizione orizzontale mentre l'asse delle Y \[EGrave] in posizione verticale"],
				DefaultButton[]
			}],
			ImageSize->Medium,BaseStyle->{"GenericButton"}
		],
			
		Button["Nuovo Esercizio",FrontEndExecute[FrontEndToken[NotebookLocate["esCoordinate"],"Evaluate"]],ImageSize->Medium,BaseStyle->{"GenericButton"}]
	
	}],
	
	Dynamic[Text[Style[esito,colEsito, 20]]],
	"   x: "InputField[Dynamic[xIn],Number,FieldSize->5], "\n",
	"   y: "InputField[Dynamic[yIn],Number,FieldSize->5],
	
	
	Button["Risolvi", 
		If[xIn == x && yIn ==y,
			(*IF*)
		  {esito:="Corretto"; colEsito:=Green}, 
		   (*ELSE *)
		  {esito:="Sbagliato"; colEsito:=Red }], BaseStyle->{Green} 
	]
	}]
	}](*Fine Row*)
];
(*individue le coord del punto*)
startGame[] := DynamicModule[{},
	pointList = List[{3,2},{1,3},{-1,1},{-2,-5},{-3,8},{-1,3},{-2,1},{-7,3},{2,0},{-9,5},{-10,7},{5,2},{-5,4},{-8,-5},{2,9},{3,-6}];
     randomPoint = RandomInteger[{1,Length[pointList]}];
	 P = pointList[[randomPoint]];
	 drawCartesian[P[[1]],P[[2]]]
	
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







(* ::InheritFromParent:: *)
(**)


(* ::InheritFromParent:: *)
(**)


(* ::InheritFromParent:: *)
(**)


End[]; (* fine sezione privata *)
EndPackage[]; (* Fine del Package *)
