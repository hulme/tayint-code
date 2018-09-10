(*The full calculation for Euclidean kinematics*)
<<TayIntfunctions.m
<<TayInt.m
(*(*BEGIN user input*)*)
(*(*directories*)*)
{SecDecDir="D1"}
{ItSecDir="D2";}
{CalcDir="D7"}
(*(*order*)*)
epsord=E1;
(*(*name*)*)
{name=N1;}
(*(*invariants*)*)
{kineinv=K1}
(*(*masses*)*)
{kinemass=K2}
(*(*the numbers at which we want results*)*)
KineRun={i}
KineScalesVal=K3
KineMassesVal=K4
ResKineList=Join[KineRun,KineScalesVal,KineMassesVal]
{ResKine=N[Table[ResKineList,{i,K5,K6,K7}]];}
(*(*the variables*)*)
{Feynlist=F1;}
(*(*generate the names and prefactors*)*)
{name[#]=((ToString[name]<>ToString[#]))&/@Range[0,epsord]}
SetDirectory[ToString[SecDecDir]<>"auxres/prefactor/"]
(prefE[#]=Import[ToString[#]])&/@Range[0,epsord]
(*(*generate the full list of scales*)*)
kinenames=Join[kineinv,kinemass]
(*(*Input the sectors*)*)
SetDirectory[ToString[ItSecDir]]
(*the names of the scales used by SecDec*)
SecDecInv=esx[#]&/@Range[0,Length[kineinv]-1]
SecDecMass=em[#]&/@Range[0,Length[kinemass]-1]
SecDecList=Join[SecDecInv,SecDecMass]
SecList=getSectors[f,#]&/@Range[1,S1]/.Thread[SecDecList->kinenames];
(*(*series start and end order*)*)
serstart=ConstantArray[0,Length[SecList]]
serend=ConstantArray[T1,Length[SecList]]
(*now we generate the conformal mappings*)
map[x_]:=(-1-x)/x
maplist=map@Feynlist
{varspec1maplist=map@Feynlist/.{maplist[[Length[Feynlist]]]->varlist[[Length[Feynlist]]]}}
{secvarspec1=Join[ConstantArray[varspec1maplistI59,1],ConstantArray[maplistI59,Length[SecList]-1]]}
(*now we generate the intlist and the partitions*)
IntListEucCalc=ConstantArray[{0,1},Length[FeynList]]
IntListRatCalc=ConstantArray[{1,1,1},Length[FeynList]]
SetDirectory["D7"]
{COMPDiscTaySeriesIntegrator[SecList[[#]],Feynlist,secvarspec1[[#]],0,4,ToString[name]<>"mapsecvarspec1INT"<>ToString[epsord]<>"SEC"<>ToString[#],#]&/@Range[1,Length[SecList]]}
{COMPDiscTaySliceOutput[1,#,#,ToString[name]<>"mapsecvarspec1INT"<>ToString[epsord]<>"SEC"<>ToString[#],ToString[name]<>"SLICEmapsecvarspec1INT"<>ToString[epsord]<>"SEC"<>ToString[#],Feynlist,secvarspec1[[#]],0,4,IntListEucCalc,IntListRatCalc]&/@
Range[1,Length[SecList]]}
{Table[KineTABTABFlexVarIndFast3ThWheelB[i,i,ToString[name]<>"SLICEmapsecvarspec1INT"<>ToString[epsord]<>"SEC"<>ToString[i],ToString[name]<>"SLICEmapsecvarspec1INT"<>ToString[epsord]<>"SEC"<>ToString[i]<>"KINE" <>ToString[#],Feynlist,secvarspec1[[i]],0,4,IntListEucCalc,IntListRatCalc,kinenames,ResKine[[#]],#]&/@Range[1,Length[ResKine]],{i,1,Length[SecList]}]}
Quit
