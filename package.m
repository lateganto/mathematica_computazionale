BeginPackage[ "ProvaProgetto`"];
Unprotect["ProvaProgetto` *"] (* toglie temporaneamente la protezione per ridefinire le funzioni *)
ClearAll["ProvaProgetto` *"];


(* ::InheritFromParent:: *)
(**)

es4::usage = "secante/tangente";
Begin["`Private`"]; (* Comincia spazio privato *)

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

End[];
EndPackage[];
