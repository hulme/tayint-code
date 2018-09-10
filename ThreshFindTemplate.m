(*Here we find the thresholds*)
<<TayIntfunctions.m
{ItSecDir="D2";}
{name=N1;}
(*(*invariants*)*)
{kineinv=K1}
(*(*masses*)*)
{kinemass=K2}
kinenames=Join[kineinv,kinemass]
(*kinevals*)
KineScalesVal=K3
KineMassesVal=K4
{Feynlist=F1}
{Table[ThreshList[i]=Join[{#*KineMassesVal[[i]]},KineScalesVal,KineMassesVal]&/@Range[-1, 10],{i,1,Length[KineMassesVal]}]}
intlowlist=ConstantArray[0,Length[Feynlist]]
inthighlist=ConstantArray[1,Length[Feynlist]]
SetDirectory[ToString[ItSecDir]]
(*the names of the scales used by SecDec*)
SecDecInv=esx[#]&/@Range[0,Length[kineinv]-1]
SecDecMass=em[#]&/@Range[0,Length[kinemass]-1]
SecDecList=Join[SecDecInv,SecDecMass]
SecList=getSectors[f,#]&/@Range[1,S1]/.Thread[SecDecList->kinenames]
{PlainTay[SecList[[#]],FeynList,FeynList,intlowlist,inthighlist,0,0, ToString[name]<>"thresh",#]&/@Range[1, Length[SecList]]}
{Table[TayList[i]=PlainTayKine[FeynList,kinenames,ThreshList[i][[#]],ToString[name]<>"thresh",ToString[name]<>"threshkin",0,0,1,Length[SecList]]&/@Range[1,Length[ThreshList]],{i,1,Length[KineMassesVal]}]}
{Table[ThreshPosList[i]=If[TayList[i][[#+1]]>TayList[i][[#]]&&TayList[i][[#+1]]>TayList[i][[#+ 2]],Position[TayList[i][[#]],TayList[i]]]&/@Range[1,Length[TayList[i]]-1],{i,1,Length[KineMassesVal]}]};
{Table[ThreshValList[i]=ThreshList[i][[ThreshPosList[i][[#]]]]&/@Range[1,Length[ThreshPosList[i]]],{i,1,Length[KineMassesVal]}]};
SetDirectory["D4"]
{Table[Put[ThreshValList[i][[#]],(ToString[name]<>"Mass"<>ToString[i]<>"Thresh"<>ToString[#]<>".txt")]&/@Range[1,Length[ThreshValList]],{i,1,Length[KineMassesVal]}]}
