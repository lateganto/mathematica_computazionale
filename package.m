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
Off[Syntax::shdw]; (* warning di definizioni oscurate *)


ShowCirc::usage = "linea";

FromRadToGrad[x_] := Return[x*180/Pi];
FromGradToRad[x_] := Return[N[Pi*x/180]];

 
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
			"Coseno -> ", Cos[x*Pi/2], " ||| Seno -> ", Sin[x], "\n",
			
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
  {Yellow, PointSize @ .02, Point@{Cos[x], Sin[x]}}
   },
   ImageSize->Medium,
   Axes->True] (*FINE GRAPHICS*),(*End Graphics*)
	}],
		(* parametri modificabili: i 3 coefficenti della retta *)
		{{grad,0,"Angolo"}, 0, 360,1}
	]
  ];
  
  





EndPackage[]; (* Fine del Package *)



