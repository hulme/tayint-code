(*The full calculation for Euclidean kinematics*)
<<TayIntfunctions.m
<<TayInt.m
(*(*BEGIN user input*)*)
(*(*directories*)*)
{SecDecDir="/home/ttp/dhulme/SecDec-3.0.9/loop/tridom1correct/I246/I246_EUC"}
{ItSecDir="/home/ttp/dhulme/SecDec-3.0.9/loop/tridom1correct/I246/I246_EUC/numerics/together/epstothe0";}
{CalcDir="/home/ttp/dhulme/SecDec-3.0.9/loop/tridom1correct/I246/I246_EUC/TayIntAuto/calc/EUC"}
(*(*order*)*)
epsord=0;
(*(*name*)*)
{name=I246;}
(*(*invariants*)*)
{kineinv={q23s,q123s,q12s}}
(*(*masses*)*)
{kinemass={ms}}
(*(*the numbers at which we want results*)*)
KineRun={i}
KineScalesVal={-2*29929,-0.5*29929}
KineMassesVal={29929}
ResKineList=Join[KineRun,KineScalesVal,KineMassesVal]
{ResKine=N[Table[ResKineList,{i,0,-4*29929,-4/50*29929}]];}
(*(*the variables*)*)
{Feynlist={x0,x1,x2,x3,x4,x5};}
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
SecList=getSectors[f,#]&/@Range[1,36]/.Thread[SecDecList->kinenames];
(*(*series start and end order*)*)
serstart=ConstantArray[0,Length[SecList]]
serend=ConstantArray[4,Length[SecList]]
(*now we generate the conformal mappings*)
map[x_]:=(-1-x)/x
maplist=map@Feynlist
{varspec1maplist=map@Feynlist/.{maplist[[Length[Feynlist]]]->varlist[[Length[Feynlist]]]}}
{secvarspec1=Join[ConstantArray[varspec1maplistI59,1],ConstantArray[maplistI59,Length[SecList]-1]]}
(*now we generate the intlist and the partitions*)
IntListEucCalc=ConstantArray[{0,1},Length[FeynList]]
IntListRatCalc=ConstantArray[{1,1,1},Length[FeynList]]
SetDirectory["/home/ttp/dhulme/SecDec-3.0.9/loop/tridom1correct/I246/I246_EUC/TayIntAuto/calc/EUC"]
{COMPDiscTaySeriesIntegrator[SecList[[#]],Feynlist,secvarspec1[[#]],0,4,ToString[name]<>"mapsecvarspec1INT"<>ToString[epsord]<>"SEC"<>ToString[#],#]&/@Range[1,Length[SecList]]}
{COMPDiscTaySliceOutput[1,#,#,ToString[name]<>"mapsecvarspec1INT"<>ToString[epsord]<>"SEC"<>ToString[#],ToString[name]<>"SLICEmapsecvarspec1INT"<>ToString[epsord]<>"SEC"<>ToString[#],Feynlist,secvarspec1[[#]],0,4,IntListEucCalc,IntListRatCalc]&/@
Range[1,Length[SecList]]}
{Table[KineTABTABFlexVarIndFast3ThWheelB[i,i,ToString[name]<>"SLICEmapsecvarspec1INT"<>ToString[epsord]<>"SEC"<>ToString[i],ToString[name]<>"SLICEmapsecvarspec1INT"<>ToString[epsord]<>"SEC"<>ToString[i]<>"KINE" <>ToString[#],Feynlist,secvarspec1[[i]],0,4,IntListEucCalc,IntListRatCalc,kinenames,ResKine[[#]],#]&/@Range[1,Length[ResKine]],{i,1,Length[SecList]}]}
Quit
