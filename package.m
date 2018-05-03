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

Begin["`Private`"]; (* Comincia spazio privato *)

FromRadToGrad[x_] := Return[x*180/Pi];
FromGradToRad[x_] := Return[N[Pi*x/180]];
getAngle[p1_,p2_] := Mod[ArcTan @@ (p2 - p1), 2 Pi];  
pointList = List[{3,2},{1,3},{-1,1},
				{-2,-5},{-3,8},{-1,3},
				{-2,1},{-7,3}];


drawCartesian[x_,y_]:= DynamicModule[{limit = 10},

	q1 = {limit,limit};
	q2 = {-limit,limit};
	q3 = {-limit,-limit};
	q4 = {limit,-limit};
	Graphics[{
		Style[Rectangle[{0,0},q1],White], 
		Style[Rectangle[{0,0},q2],White],
		Style[Rectangle[{0,0},q3],White],
		Style[Rectangle[{0,0},q4],White],
		{PointSize[Large],Red,Point[{x,y}]}
	}(* Fine Graphics *),	
	Axes->True,
	AxesLabel->{"x","y"},
	Ticks->{
		Table[i,{i,-limit, limit, 1}],
		Table[i,{i,-limit, limit, 1}]
		}
	]
];

startGame[] := DynamicModule[{},
	 random = RandomInteger[{1,Length[pointList]}];
	 P = pointList[[random]];
	 drawCartesian[P[[1]],P[[2]]]
];

GetQuad[x0_,y0_] := DynamicModule[{x=x0, y=y0, quad},
	If[x>=0&&y>=0, quad=1];
	If[x<0&&y>=0, quad=2];
	If[x<=0&&y<=0, quad=3];
	If[x>=0&&y<0, quad=4];
quad];

 
ShowCirc[]:=
  DynamicModule[{quad, start, end,x},
	Manipulate[ (* permette all'utente di modificare i parametri *)
	x = FromGradToRad[grad];
			Row[{
			Button["0\[Degree]",grad=0],Button["30\[Degree]",grad=30],
			Button["45\[Degree]",grad=45],Button["60\[Degree]",grad=60],
			Button["90\[Degree]",grad=90],"\n",
			If[Cos[x]>=0&&Sin[x]>=0, quad=1;start=0; end=x;,
				If[Cos[x]<0&&Sin[x]>0, quad=2;start=Pi; end= x;,
					If[Cos[x]<0&&Sin[x]<0, quad=3;start=Pi; end= x;, quad= 4; start=0; end= -(2 Pi - x);]				
			    ]
			],
			"Quadrante: ", quad, "\n",
			"Angolo: ", grad, "\[Degree] || ", x,"\n",
			"Coseno -> ", Cos[Pi/2], " ||| Seno -> ", Sin[x], "\n",
			
	 Graphics[{
    Circle[],
    Point[{0,0}],
    PointSize[Large],
    Circle[{0,0},0.1,{start, end}],
    {Dashed,
      Line[{{0,0},{Cos[x],Sin[x]}}]},(*raggio*)
    Thickness[0.01],
      RGBColor[0,255,0],
      Line[{{0,0},{Cos[x],0}}],
      RGBColor[255,0,0],
      Line[{{Cos[x],0},{Cos[x],Sin[x]}}],
      Text[Style[IntegerString[FromRadToGrad[x]] <> "\[Degree]",Large,Red],{-0.90,1}],
      Text[Style["Seno",Medium,Red],{0.8,1}],
      Text[Style["Coseno",Medium,Green],{0.8,0.9}],

      {Yellow, PointSize ->.02, Point[{Cos[x], Sin[x]}]}},
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