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


Unprotect["ProvaProgetto`"] (* toglie temporaneamente la protezione per ridefinire le funzioni *)
ClearAll["ProvaProgetto`"];


drawCircle::usage = "disegna un grafico";

Begin["`Private`"]; (* Comincia spazio privato *)
getAngle[p1_,p2_] := Mod[ArcTan @@ (p2 - p1), 2 Pi];  

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
      {Yellow, PointSize @ .02, Point@{Cos[x], Sin[x]}}},
      ImageSize->350,
      Axes->True
    ] (*FINE GRAPHICS*),
    Column[{
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
  {{x,0,"Angolo"}, 0, 2 Pi}
];

End[]; (* fine sezione privata *)

EndPackage[]; (* Fine del Package *)