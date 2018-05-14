BeginPackage[ "ProvaProgetto`"];
Unprotect["ProvaProgetto` *"] (* toglie temporaneamente la protezione per ridefinire le funzioni *)
ClearAll["ProvaProgetto` *"];


(* ::InheritFromParent:: *)

(*

	RICORDARE DI NOMINARE LA CELLA DEL NOTEBOOK "es4"

*)

secantList  = List[ {{0,-20}, {18,18}}, {{-20,9}, {18,6}}, {{-10,-13}, {6,20}} ]; (* Lista di coppie di punti per disegnare 4 secanti diverse *)

tangentList = List[ {{-20,0}, {15,19.7}}, {{14,-20}, {14,19.7}}, {{-6.075,-20}, {-6.075,20}} ]; (* Lista di coppie di punti per disegnare 4 tangenti diverse *)

outList = List[ {{-17,-11.1}, {20,-11.1}}, {{-20,16}, {20,16}}, {{-7,-10}, {-10,20}} ]; (* Lista di coppie di punti per disegnare 4 esterne diverse *)

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
          Graphics[{
            { ColorData[1,6], Thick, Circle[{4,2}, 10] }, (* Circonferenza Viola *)
            { ColorData[2,1], Thick, Line[R1] }, (* Secante Rossa *)
            { ColorData[1,1], Thick, Line[R3] }, (* Esterna Blu *)
            { ColorData[1,8], Thick, Line[R2]} }, (* Tangente Verde *) 
            Axes -> True,
            ImageSize -> 450, (* Setto la dimensione del grafico *) 
            PlotRange -> 20, (* Setto il range degli assi *)
            AxesLabel->{"x","y"},
            Ticks->{
              Table[i,{i,-20, 20, 2}], (* Setto la granularità degli assi *)
              Table[i,{i,-20, 20, 2}]
            }
          ],
          Text[Style["\t"]],
	  (* Mi creo una colonna con le quattro opzioni tra cui è compresa la risposta esatta *)
          Column[{
            
	    Style[Dynamic[Esito],20], (* La variabile esito può assumere valori differenti in fase esecuzione *)
            
	    (* Setto il primo bottone con la risposta relativa *)
	    wrong1 = Row[{
              btn1 = Button[Text[">"], Esito:=checkRisp[risp1, esatta]], (* Primo bottone con relativa risposta *)
              Text["\t"],
              Text[" Tangente ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[1,8]],
              Text["\t"],
              Text["   Esterna  ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[2,1]], 
              Text["\t"],
              Text["  Secante  ", BaseStyle->{RGBColor[1,1,1], FontSize -> 20}, Background -> ColorData[1,1]],
              Text["\n\n"]
            }];
	    
	    (* Setto il secondo bottone con la risposta relativa *)
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

	    (* Setto il terzo bottone con la risposta relativa *)
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
	    
	    (* Setto il quarto bottone con la risposta esatta *)
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
	    
	    (* Inizializzo 5 variabile con le 4 risposte permutate *)
            rand1 = {wrong2, wrong1, right, wrong3};
            rand2 = {wrong3, wrong2, right, wrong1};
            rand3 = {wrong1, wrong3, wrong2, right};
            rand4 = {wrong2, wrong1, right, wrong3};
            rand5 = {wrong1, wrong2, wrong3, right};

	    (* Inserisco le 5 permutazioni in una lista e ne scelgo una in maniera casuale *)
            answerRows = List[rand1, rand2, rand3, rand4, rand5];
            zz = RandomInteger[{1,5}];
          
	    (* Mi creo una column con la permutazione salvata in zz *)
            Column[answerRows[[zz]]],
            Row[{
	    (* Bottone di aiuto con le tre definizioni di tangente, secante ed esterna *)
              Button["Aiuto",
                CreateDialog[{
                  TextCell["Data una retta r e una circonferenza c, diciamo che: \n > r è esterna a c se r \[Intersection] c = \[EmptySet] \n > r è esterna a c se r \[Intersection] c = P \n > r è esterna a c se r \[Intersection] c = {P, \[EmptySet]}"],
                  DefaultButton[]
                }],
                ImageSize->Medium,BaseStyle->{"GenericButton"}
              ]
            }],
            Row[{
	    (* Bottone che reinizializza l'esercizio 
	    con una nuova disposizione delle rette sul piano 
	    e una permutazione diversa delle risposte possibili *)
              Button["Nuovo Esercizio",FrontEndExecute[FrontEndToken[NotebookLocate["es4"],"Evaluate"]],ImageSize->Medium,BaseStyle->{"GenericButton"}]
		        }]
          }]
          }]
        }]
];

End[];
EndPackage[];
