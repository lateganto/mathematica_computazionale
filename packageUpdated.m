BeginPackage[ "ProvaProgetto`"];
Unprotect["ProvaProgetto` *"] (* toglie temporaneamente la protezione per ridefinire le funzioni *)
ClearAll["ProvaProgetto` *"];

drawCircle::usage = "disegna un grafico";
drawCartesian::usage = "disegna assi"
startGame::usage = "gioco del quadrante";
distanceGame::usage = "calcola la distanza distanza tra due punti";
es4::usage = "secante/tangente";
puntiInaccessibili::usage = "Determinare la distanza fra due punti inaccessibili";
Begin["`Private`"]; (* Comincia spazio privato *)

(* ::InheritFromParent:: *)
(**)

startGame[] := DynamicModule[{},
	 random = RandomInteger[{1,Length[pointList]}];
	 P = pointList[[random]];
	 drawCartesian[P[[1]],P[[2]]];
   es4[]
];

secantList  = List[ {{0,-20}, {18,18}}, {{-20,9}, {18,6}}, {{-10,-13}, {6,20}} ];

tangentList = List[ {{-20,0}, {15,19.7}}, {{14,-20}, {14,19.7}}, {{-6.075,-20}, {-6.075,20}} ];

outList = List[ {{-17,-11.1}, {20,-11.1}}, {{-20,16}, {20,16}}, {{-7,-10}, {-10,20}} ];

es4[] := DynamicModule[{Esito=""},

      checkRisp[risp_, correct_] := (
		    If[risp == correct, Return["Risposta Corretta"], Return["Risposta Sbagliata"]]
	    );

      random1 = RandomInteger[{1,Length[secantList]}];
      random2 = RandomInteger[{1,Length[tangentList]}];
      random3 = RandomInteger[{1,Length[outList]}];

      R1 = secantList[[random1]];
      R2 = tangentList[[random2]];
      R3 = outList[[random3]];
      
      risp1="TES";
      risp2="STE";
      risp3="ETS";
      risp4="TSE";
      esatta="TSE";

      Column[{
        Row[{
          Text[Style["Si considerino le rette in figura e si individui, la posizione che ciascuna retta assume rispetto alla circonferenza.", FontFamily -> "Roboto", 20]],
          Graphics[
            {
              { ColorData[1,6], Thick, Circle[{4,2}, 10] }, (* Circonferenza Viola *)
              { ColorData[2,1], Thick, Line[R1] }, (* Secante Rossa *)
              { ColorData[1,1], Thick, Line[R3] }, (* Esterna Blu *)
              { ColorData[1,8], Thick, Line[R2] }  (* Tangente Verde *) 
            },
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
            Style[Dynamic[Esito],20],
            wrong1 = Row[{
              btn1 = Button[Text[">"], Esito:=checkRisp[risp1, esatta]],
              Text["\t"],
              Text[" Tangente ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[1,8]],
              Text["\t"],
              Text["   Esterna  ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[2,1]], 
              Text["\t"],
              Text["  Secante  ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[1,1]],
              Text["\n\n"]
            }];
            wrong2 = Row[{
              btn2 = Button[">", Esito:=checkRisp[risp2, esatta]],
              Text["\t"],
              Text["  Secante  ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[1,8]],
              Text["\t"],
              Text[" Tangente ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[2,1]],
              Text["\t"],
              Text["  Esterna  ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[1,1]],
              Text["\n\n"]
            }];
            wrong3 = Row[{
              btn3 = Button[">", Esito:=checkRisp[risp3, esatta]],
              Text["\t"],
              Text["  Esterna   ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[1,8]],
              Text[" \t"],
              Text[" Tangente ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[2,1]],
              Text["\t"],
              Text["  Secante  ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[1,1]],
              Text["\n\n"]
            }];
            right = Row[{
              btn4 = Button[">", Esito:=checkRisp[risp4, esatta]],
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

FromRadToGrad[x_] := Return[(Pi/4)*(x 180 / Pi)];

getAngle[p1_, p2_, p3_] :=
    Return[
      ArcCos[(EuclideanDistance[p1, p2]^2 + EuclideanDistance[p1, p3]^2 - EuclideanDistance[p2, p3]^2) /
          (2 * EuclideanDistance[p1, p2] * EuclideanDistance[p1, p3]) 
      ]
    ];


puntiInaccessibili[] := DynamicModule[{p1={-2,-1}, p2={-0.5,1}, p3={2,1}, p4={0.5,-1}, Esito=""},
  
  checkRisp[risp_, correct_] := (
    If[risp == correct, Return[Style["Risposta Corretta", Green, FontFamily->Roboto]], Return[Style["Risposta Errata", Red, FontFamily->Roboto]]]
  );

  esatta = 15;
  dynamic;
  Row[{
    Column[{
      Row[{
        Manipulate[
          Deploy[
            DynamicModule[{},
              Dynamic@Graphics[{
                  
                  {Thickness[0.005], Darker[Green], Line[{{-3,-3},{3,-3}}]},
                  {Thickness[0.005], Darker[Green], Line[{{-3,3},{3,3}}]},
                  upperGrass = FilledCurve[{Line[{{-3,0.1},{-3,3}}], Line[{{3,3},{3,0.6}}] }];
                  lowerGrass = FilledCurve[{Line[{{-3,-0.1},{-3,-3}}], Line[{{3,-3}, {3,0.1}}]}];
                  {Opacity[.7], Green, upperGrass},
                  {Opacity[.7], Green, lowerGrass},
                  
                  (* Fiume *)
                  {Thickness[0.005], Darker[Blue], BezierCurve[{ {-3,0.3}, {-0.8,-0.1}, {0.5,0.8}, {3,0.6}}]},
                  {Thickness[0.005], Darker[Blue], BezierCurve[{ {-3,-0.3}, {-0.8,-0.7}, {0.5,0.2}, {3,0}}]},
                  river = BezierCurve[{ {-3,0}, {-0.8,-0.4}, {0.5,0.5}, {3,0.3}}];
                  {Thickness[0.1], Blue, river},

                  (* Parallelogramma *)
                  l1={Thickness[0.005], Red, Line[{p1,p2}]},
                  l2={Thickness[0.005], Blue, Line[{p2,p3}]},
                  l3={Thickness[0.005], Yellow, Line[{p3,p4}]},
                  l4={Thickness[0.005], Line[{p4,p1}]}, (* AC *)
                  
                  (* Diagonali *)
                  d1={Thickness[0.005], Yellow, Line[{p1,p3}]},
                  d2={Thickness[0.005], Red, Line[{p2,p4}]},
                  
                  {PointSize[0.015], Point[p1], Locator[Dynamic[p1], None]},
                  Style[Text["C", {First[p1], Last[p1]-0.2}], 15],
                  Style[Text["\[Gamma]1", {First[p1]+0.5, Last[p1]+0.1}], 15],
                  Style[Text["\[Gamma]2", {First[p1]+0.6, Last[p1]+0.5}], 15],
                  {PointSize[0.015], Point[p2], Locator[Dynamic[p2], None, Enabled->False]},
                  Style[Text["\[Beta]", {First[p2]-0.03, Last[p2]-0.3}], 15],
                  Style[Text["B", {First[p2], Last[p2]+0.2}], 15],
                  {PointSize[0.015], Point[p3], Locator[Dynamic[p3], None, Enabled->False]},
                  Style[Text["D", {First[p3], Last[p3]+0.2}], 15],
                  {PointSize[0.015], Point[p4], Locator[Dynamic[p4], None]},
                  Style[Text["A", {First[p4], Last[p4]-0.2}], 15],
                  Style[Text["\[Alpha]1", {First[p4]-0.5, Last[p4]+0.3}], 15],
                  Style[Text["\[Alpha]2", {First[p4]-0.2, Last[p4]+0.1}], 15],

                  If[First[p1] <= First[p3],
                    If[Last[p1] <= Last[p3], 
                      Circle[p1, 0.5, {0 + getAngle[p1, p3, {First[p3], Last[p1]}], getAngle[p1, p2, p3] + getAngle[p1, p3, {First[p3], Last[p1]}]}],
                      Circle[p1, 0.5, {0 - getAngle[p1, p3, {First[p3], Last[p1]}], getAngle[p1, p2, p3] - getAngle[p1, p3, {First[p3], Last[p1]}]}]],
                  ],

                  If[Last[p1] >= Last[p3],
                    If[First[p1] < First[p3],
                      Circle[p3, 0.5, {Pi/2, getAngle[p3, p1, p2] + Pi/2}],
                      Circle[p3, 0.5, {getAngle[p3, p1, {First[p1], Last[p3]}], getAngle[p3, p1, p2] + getAngle[p3, p1, {First[p1], Last[p3]}]}]
                    ],
                  ],
                },
                ImageSize -> 500, 
                PlotRange -> 3
              ]
            ]
          ]
        ]
      }]
    }],
    Text["\t\t"],
    Column[{
      Row[{
        Style[Text["Determinare la distanza tra due punti B e D entrambi inaccessibili.\nSpostandoci da A a B possiamo considerare ADC ed ABC."], FontFamily->Roboto, FontSize->20]
      }],
      Text["\n"],
      Row[{
        Column[{
          Style[Text["Di ADC conosciamo:\n- la misura di AC\n- l'angolo DAC=\[Alpha]1\n- l'angolo DCA=\[Gamma]1\n"], FontFamily->Roboto, FontSize->20]
        }],
        Text["\t\t"],
        Column[{
          Style[Text["Di ABC conosciamo:\n- la misura di AC\n- l'angolo BAC=\[Alpha]2\n- l'angolo BCA=\[Gamma]2\n"], FontFamily->Roboto, FontSize->20]
        }]
      }],
      Row[{
        Button["Calcolare \[Delta]=CDA e \[Beta]=CBA",
          CreateDialog[{
            TextCell["L'angolo \[Delta]=CDA si ricava dalla differenza \ntra la somma totale degli angoli interni \ne i due angoli dati \n(stessa cosa vale per \[Beta]=CBA)"],
            DefaultButton[]
          }],
          ImageSize->Medium,BaseStyle->{"GenericButton"}
        ],
        Text["   "],
        Button["Calcolare AD e BD",
          CreateDialog[{
            TextCell["AD e AB si ricavano applicando il teorema dei seni:\n\tAD = (AC sin\[Gamma]1)/sin\[Delta]\n\tAB = (AC sin\[Gamma])/sin\[Beta]"],
            DefaultButton[]
          }],
          ImageSize->Medium,BaseStyle->{"GenericButton"}
        ],
        Text["   "],
        Button["Calcolare BD",
          CreateDialog[{
            TextCell["È possibile calcolare la lunghezza di BD applicando Carnot:\n\n\tBD = \[Sqrt]AB+AD-2*AB*AD*cos(α1-α2)"],
            DefaultButton[]
          }],
          ImageSize->Medium,BaseStyle->{"GenericButton"}
        ],
        Text["\n"],
        Button["Ricarica",FrontEndExecute[FrontEndToken[NotebookLocate["punti"],"Evaluate"]],ImageSize->Medium,BaseStyle->{"GenericButton"}]
	    }]
    }]
  }]
];
End[]; (* fine sezione privata *)
EndPackage[]; (* Fine del Package *)
