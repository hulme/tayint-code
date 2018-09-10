(*(*Our finalised algorithm*)*)
<<TayIntfunctions.m
Return[SimplifiedSectorList]]
(*(*the user needs to input the prefactor and the start and end order of the expansion as well as integration and kinematic variables and the numbers and the actual integrands plus the precision with which you would like to scan the surfaces to determine which ID to use*)*)
(*(*BEGIN user input*)*)
(*(*directories*)*)
{SecDecDir="/home/ttp/dhulme/SecDec-3.0.9/loop/tridom1correct/I246/I246_EUC/"}
{ItSecDir="/home/ttp/dhulme/SecDec-3.0.9/loop/tridom1correct/I246/I246_EUC/numerics/together/epstothe0/";}
{CalcDir="/disk/data11/ttp/dhulme/New/I246/Results/S2/"}
(*(*order*)*)
epsord=2;
(*(*name*)*)
{name=I246;}
(*(*invariants*)*)
{kineinv={q12s,q23s,q123s}}
(*(*masses*)*)
{kinemass={ms}}
(*(*the numbers at which we want results*)*)
{ResKine=N[Table[{-2*29929,i,0.5*29929,29929},{i,0*29929,4*29929,4/50*29929}]];}
(*(*the variables*)*)
{Feynlist={x0,x1,x2,x3,x4,x5};}
(*(*If the user wants to request a certain number of partitions in advance - but this is not needed as we work this out as part of our algorithm*)*)
(*(*END USER INPUT*)*)
(*(*generate the names and prefactors*)*)
intlist={0,1.}
{(name[#]=(ToString[name]<>ToString[#]))&/@Range[0,epsord]}
{SetDirectory[ToString[SecDecDir]<>"auxres/prefactor/"]}
(prefE[#]=Import[ToString[#]])&/@Range[0,epsord]
(*(*generate the full list of scales*)*)
{kinenames=Join[kineinv,kinemass]}
(*(*the sectors*)*)
{SetDirectory[ToString[ItSecDir]]}
secnum=(Import[ToString["SecNum"<>ToString[name]<>".txt"]])
{SecDecInv=esx[#]&/@Range[0,Length[kineinv]-1]}
{SecDecMass=em[#]&/@Range[0,Length[kinemass]-1]}
{SecDecList=Join[SecDecInv,SecDecMass]}
SecList=(getSectors[f,#]/.Thread[SecDecList->kinenames])&/@Range[1,36];
(*(*series start and end order*)*)
{serstart=ConstantArray[0,Length[SecList]]}
{serend=ConstantArray[4,Length[SecList]]}
(*(*END user input*)*)
(*(*move the sectors onto the complex plane*)*)
(*(*new variables*)*)
{varlist=theta[#-1]&/@Range[1,Length[Feynlist]]}
(*(*the trafos*)*)
{comptranslist=(Mean[intlist]-Mean[intlist]*Exp[-I*varlist[[#]]])&/@Range[1,Length[varlist]]}
(*(*the jacobian*)*)
{compJac=Times@@(D[comptranslist[[#]],varlist[[#]]]&/@Range[1,Length[varlist]])}
(*(*note - our naming system is that the last names before the brackets correspond to the numbers in the brackets respectively*)*)
(*(*upgraded so it is variable independent*)*)
{(CAdomsec[#]=(compJac)*(SecList[[#]]/.Thread[Feynlist->comptranslist]))&/@Range[1,Length[SecList]];}
(*(*the possible contours*)*)
{CCNIEL=Tuples[{-1,1},{Length[varlist]}]}
{CCNIELint=CCNIEL/.{-1->-Pi,1->Pi}}
{CCNIELcalc=CCNIEL/.{-1->{0,-Pi},1->{0,Pi}}}
(*(*Algo results*)*)
{RatKMSEI2461=ConstantArray[ConstantArray[1,6],6]}
{(CCFinal[#]={9,9,17,17,1,1,32,31,31,32,16,16,7,23,27,27,2,2,1,1,19,24,2,1,25,26,11,12,8,23,23,8,27,28,21,Null}[[#]])&/@Range[1,36]}
{SetDirectory["/home/ttp/dhulme/form/TaylorExpansionWorking/ReallyFinalTaylorCodes/"]}
<<TayInt.m
SetDirectory[CalcDir]
SecStart=2
SecFin=2
COMPDiscTaySeriesIntegrator[CAdomsec[#],varlist,varlist,0,4,"I246OTE0CASEC"<>ToString[#],#]&/@Range[SecStart,SecFin]
COMPDiscTaySliceOutput[1,#,#,"I246OTE0CASEC"<>ToString[#],"I246OTE0PARTITIONCASEC"<>ToString[#],varlist,varlist,0,4,CCNIELcalc[[CCFinal[#]]],RatKMSEI2461]&/@Range[SecStart,SecFin]
Table[COMPKineTABTABFlexVarIndFast3ThWheelB[i,i,"I246OTE0PARTITIONCASEC"<>ToString[i],"I246OTE0KINECASEC"<>ToString[i],varlist,varlist,0,4,CCNIELcalc[[CCFinal[i]]],RatKMSEI2461,kinenames,UTABI246[[#]],#],{i,SecStart,SecFin}]&/@Range[1,Length[UTABI246]]
