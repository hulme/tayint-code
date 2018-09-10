(*Our finalised algorithm*)

<<TayIntfunctions.m


(* ::Input:: *)
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


(* ::Input:: *)
(*(*generate the names and prefactors*)*)


(* ::Input:: *)
(*(name[#]=(ToString[name]<>ToString[#]))&/@Range[0,epsord]*)


(* ::Input:: *)
(*SetDirectory[ToString[SecDecDir]<>"auxres/prefactor/"]*)


(* ::Input:: *)
(*(prefE[#]=<<(ToString[#]))&/@Range[0,epsord]*)


(* ::Input:: *)
(*(*generate the full list of scales*)*)


(* ::Input:: *)
(*kinenames=Join[kineinv,kinemass]*)


(* ::Input:: *)
(*(*the sectors*)*)


(* ::Input:: *)
(*SetDirectory[ToString[ItSecDir]]*)


(* ::Input:: *)
(*SecDecInv=esx[#]&/@Range[0,Length[kineinv]-1]*)


(* ::Input:: *)
(*SecDecMass=em[#]&/@Range[0,Length[kinemass]-1]*)


(* ::Input:: *)
(*SecDecList=Join[SecDecInv,SecDecMass]*)


(* ::Input:: *)
(*SecList=getSectors[f,#]&/@Range[1,22]/.Thread[SecDecList->kinenames];*)


(* ::Input:: *)
(*(*series start and end order*)*)


(* ::Input:: *)
(*serstart=ConstantArray[0,Length[SecList]]*)


(* ::Input:: *)
(*serend=ConstantArray[4,Length[SecList]]*)


(* ::Input:: *)
(*NIserend=ConstantArray[6,Length[SecList]]*)


(* ::Input:: *)
(*PartNum=10;*)


(* ::Input:: *)
(*(*END user input*)*)


(* ::Input:: *)
(*(*average kinematic values*)*)


(* ::Input:: *)
(*(*move the sectors onto the complex plane*)*)


(* ::Input:: *)
(*(*new variables*)*)


(* ::Input:: *)
(*varlist=theta[#-1]&/@Range[1,Length[Feynlist]]*)


(* ::Input:: *)
(*(*the trafos*)*)


(* ::Input:: *)
(*intlist={0,1.}*)


(* ::Input:: *)
(*comptranslist=(Mean[intlist]-Mean[intlist]*Exp[-I*varlist[[#]]])&/@Range[1,Length[varlist]]*)


(* ::Input:: *)
(*(*the jacobian*)*)


(* ::Input:: *)
(*compJac=Times@@(D[comptranslist[[#]],varlist[[#]]]&/@Range[1,Length[varlist]])*)


(* ::Input:: *)
(*(*note - our naming system is that the last names before the brackets correspond to the numbers in the brackets respectively*)*)


(* ::Input:: *)
(*(*upgraded so it is variable independent*)*)


(* ::Input:: *)
(*(CAdomsec[#]=(compJac)*(SecList[[#]]/.Thread[Feynlist->comptranslist]))&/@Range[1,Length[SecList]];*)


(* ::Input:: *)
(*(*the possible contours*)*)


(* ::Input:: *)
(*CCNIEL=Tuples[{-1,1},{Length[varlist]}]*)


(* ::Input:: *)
(*CCNIELint=CCNIEL/.{-1->-Pi,1->Pi}*)


(* ::Input:: *)
(*CCNIELcalc=CCNIEL/.{-1->{0,-Pi},1->{0,Pi}}*)


(* ::Input:: *)
(*CCNIELvarcalc=Table[Prepend[CCNIELcalc[[#]][[i]],varlist[[i]]],{i,1,Length[varlist]}]&/@Range[1,Length[CCNIELint]];*)


(* ::Input:: *)
(*(*the partial contours*)*)


(* ::Input:: *)
(*del[in_List,(q_Integer)?Positive]:=DeleteCases[in,x_/;Length[x]<q];*)


(* ::Input:: *)
(*del2[in_List,(q_Integer)?Positive]:=DeleteCases[in,x_/;Length[x]>=q];*)


(* ::Input:: *)
(*del3[in_List,(q_Integer)?Positive,(r_Integer)?Positive]:=DeleteCases[in,x_/;q<=Length[x]<=r];*)


(* ::Input:: *)
(*SubFeynList1=Drop[Subsets[Table[i,{i,1,Length[Feynlist]}]],-1]*)


(* ::Input:: *)
(*SubFeynList2=del2[del[SubFeynList1,Length[varlist]-2],Length[varlist]-1]*)


(* ::Input:: *)
(*SubFeynList3=Table[Part[Feynlist,SubFeynList2[[i]][[#]]]&/@Range[1,Length[SubFeynList2[[i]]]],{i,1,Length[SubFeynList2]}]*)


(* ::Input:: *)
(*SubVarList3=Table[Part[varlist,SubFeynList2[[i]][[#]]]&/@Range[1,Length[SubFeynList2[[i]]]],{i,1,Length[SubFeynList2]}]*)


(* ::Input:: *)
(*SubVarFinalTab=(Feynlist/.Thread[SubFeynList3[[#]]->SubVarList3[[#]]])&/@Range[1,Length[SubFeynList3]]*)


(* ::Input:: *)
(*SubCompTransList3=Table[Part[comptranslist,SubFeynList2[[i]][[#]]]&/@Range[1,Length[SubFeynList2[[i]]]],{i,1,Length[SubFeynList2]}]*)


(* ::Input:: *)
(*compJacSub=Table[Times@@(D[comptranslist[[#]],varlist[[#]]]&/@SubFeynList2[[i]]),{i,1,Length[SubFeynList2]}]*)


(* ::Input:: *)
(*CAdomsecSub=Table[((compJacSub[[i]])*(SecList[[#]]/.Thread[SubFeynList3[[i]]->SubCompTransList3[[i]]])),{i,1,Length[SubFeynList3]}]&/@Range[1,Length[SecList]];*)


(* ::Input:: *)
(*Length[SubFeynList2[[1]]]*)


(* ::Input:: *)
(*CCNIELSub=(Tuples[{-1,1},{Length[SubFeynList2[[#]]]}])&/@Range[1,Length[SubFeynList2]]/.{1->Pi,-1->-Pi}*)


(* ::Input:: *)
(*CCNIELSubFin=Take[CCNIELSub[[#]],Length[CCNIELSub[[#]]]/2]&/@Range[1,Length[SubFeynList2]]*)


(* ::Input:: *)
(*CCNIELbaselist=Table[ConstantArray[ConstantArray[1,Length[Feynlist]],Length[CCNIELSubFin[[i]]]],{i,1,Length[CCNIELSubFin]}]*)


(* ::Input:: *)
(*Table[(CCNIELbaselist[[i]][[#]][[SubFeynList2[[i]]]]=CCNIELSubFin[[i]][[#]])&/@Range[1,Length[CCNIELSubFin[[i]]]],{i,1,Length[SubFeynList2]}]*)


(* ::Input:: *)
(*CCNIELpartial=CCNIELbaselist*)


(* ::Input:: *)
(*CCNIELpartialcalc=CCNIELpartial/.{1->{0,1},-Pi->{0,-Pi},Pi->{0,Pi}}*)


(* ::Input:: *)
(*Table[(CCNIELbaselist[[i]][[#]][[SubFeynList2[[i]]]]=CCNIELSubFin[[i]][[#]])&/@Range[1,Length[CCNIELSubFin[[i]]]],{i,1,Length[SubFeynList2]}]*)


(* ::Input:: *)
(*CCNIELpartialvarcalcSubCont=Table[Table[If[CCNIELpartial[[#]][[i]][[j]]==1,Prepend[CCNIELpartialcalc[[#]][[i]][[j]],Feynlist[[j]]],Prepend[CCNIELpartialcalc[[#]][[i]][[j]],varlist[[j]]]],{j,1,Length[varlist]}],{i,1,Length[CCNIELSubFin[[#]]]}]&/@Range[1,Length[SubFeynList2]];*)


(* ::Input:: *)
(*(*now we make the bulk tables*)*)


(* ::Input:: *)
(*tabindexlist=k[#]&/@Range[1,Length[varlist]-1]*)


(* ::Input:: *)
(*tabindexlistNI=k[#]&/@Range[1,Length[varlist]]*)


(* ::Input:: *)
(*(*with exact int*)*)


(* ::Input:: *)
(*(*you can include BulkPrec here so this is user defined*)*)


(* ::Input:: *)
(*(*we do not want to go exactly onto Pi as there are very rapid changes there which are obscure the other changes*)*)


(* ::Input:: *)
(*bulktablimlist=({tabindexlist[[#]],0,9.99*Pi/10,(9.99*Pi/10-0)*0.1})&/@Range[1,Length[varlist]-1]*)


(* ::Input:: *)
(*bulktablimlistfull=({tabindexlist[[#]],0,Pi,(Pi-0)*0.1})&/@Range[1,Length[varlist]-1]*)


(* ::Input:: *)
(*bulktab=(Flatten[Table[tabindexlist,Evaluate[Sequence@@bulktablimlist]],Length[varlist]-2]);*)


(* ::Input:: *)
(*bulktabfull=(Flatten[Table[tabindexlist,Evaluate[Sequence@@bulktablimlistfull]],Length[varlist]-2]);*)


(* ::Input:: *)
(*(*no exact int*)*)


(* ::Input:: *)
(*bulktablimlistNI=({tabindexlistNI[[#]],0,9.99*Pi/10,(9.99*Pi/10-0)*0.1})&/@Range[1,Length[varlist]]*)


(* ::Input:: *)
(*bulktabpartiallimlistNI=({tabindexlistNI[[#]],0,9.99*1/10,(9.99*1/10-0)*0.1})&/@Range[1,Length[varlist]]*)


(* ::Input:: *)
(*bulktablimlistfullNI=({tabindexlistNI[[#]],0,Pi,(Pi-0)*0.1})&/@Range[1,Length[varlist]]*)


(* ::Input:: *)
(*(*exclude the corners and look in more detail*)*)


(* ::Input:: *)
(*ncbulktablimlistNI=({tabindexlistNI[[#]],0,8*Pi/10,(8*Pi/10-0)*0.05})&/@Range[1,Length[varlist]]*)


(* ::Input:: *)
(*bulktabNI=(Flatten[Table[tabindexlistNI,Evaluate[Sequence@@bulktablimlistNI]],Length[varlist]-1]);*)


(* ::Input:: *)
(*bulktabpartialNI=(Flatten[Table[tabindexlistNI,Evaluate[Sequence@@bulktabpartiallimlistNI]],Length[varlist]-1]);*)


(* ::Input:: *)
(*bulktabfullNI=(Flatten[Table[tabindexlistNI,Evaluate[Sequence@@bulktablimlistfullNI]],Length[varlist]-1]);*)


(* ::Input:: *)
(*bulktabncNI=(Flatten[Table[tabindexlistNI,Evaluate[Sequence@@ncbulktablimlistNI]],Length[varlist]-1]);*)


(* ::Input:: *)
(*(*now we make the edge table*)*)


(* ::Input:: *)
(*(*now we make the edge table*)*)


(* ::Input:: *)
(*(*the raw integration boundary will always be Pi*)*)


(* ::Input:: *)
(*(*the numbers here are always the same-a feature of the algorithm*)*)


(* ::Input:: *)
(*quat=Table[i,{i,0,Pi,0.25*Pi}]*)


(* ::Input:: *)
(*quatpart=Table[i,{i,0,1,0.25*1}]*)


(* ::Input:: *)
(*qual=Partition[quat,2,1]*)


(* ::Input:: *)
(*qualpart=Partition[quatpart,2,1]*)


(* ::Input:: *)
(*(*I do not want to hit Pi exactly because there are always very rapid changes there*)*)


(* ::Input:: *)
(*qualcor=ReplacePart[qual,{4,2}->9.99*Pi/10]*)


(* ::Input:: *)
(*qualcorpart=ReplacePart[qualpart,{4,2}->9.99*1/10]*)


(* ::Input:: *)
(*quafl=Append[qualcor[[#]],0.125*Pi]&/@Range[1,Length[qualcor]]*)


(* ::Input:: *)
(*quaflpart=Append[qualcorpart[[#]],0.125*1]&/@Range[1,Length[qualcorpart]]*)


(* ::Input:: *)
(*(*the list for the quartered edge - strictly speaking I could just type this directly as it will always be the same*)*)


(* ::Input:: *)
(*quaflcor=ReplacePart[quafl,{4,3}->(9.99*Pi/10-3*Pi/4)*0.5]*)


(* ::Input:: *)
(*quaflcorpart=ReplacePart[quaflpart,{4,3}->(9.99*1/10-3*1/4)*0.5]*)


(* ::Input:: *)
(*(*this list will always be the same - it is a feature of the code*)*)


(* ::Input:: *)
(*edgl=List[{0.9*Pi,9.99*Pi/10,(9.99*Pi/10-9*Pi/10)*0.5},{0.9*Pi,9.99*Pi/10,(9.99*Pi/10-9*Pi/10)*0.5},{0.9*Pi,9.99*Pi/10,(9.99*Pi/10-9*Pi/10)*0.5},{0.9*Pi,9.99*Pi/10,(9.99*Pi/10-9*Pi/10)*0.5}];*)


(* ::Input:: *)
(*edglpart=List[{0.9*1,9.99*1/10,(9.99*1/10-9*1/10)*0.5},{0.9*1,9.99*1/10,(9.99*1/10-9*1/10)*0.5},{0.9*1,9.99*1/10,(9.99*1/10-9*1/10)*0.5},{0.9*1,9.99*1/10,(9.99*1/10-9*1/10)*0.5}];*)


(* ::Input:: *)
(*(*we need to fix all but two of the variables*)*)


(* ::Input:: *)
(*(* I do not want to use Pi/2 exactly as that is also in the quarter list*)*)


(* ::Input:: *)
(*fixl=ConstantArray[ConstantArray[{N[Pi/2+10^-11],N[Pi/2+10^-11]},Length[varlist]-2],4]*)


(* ::Input:: *)
(*fixlpart=ConstantArray[ConstantArray[{N[1/2+10^-11],N[1/2+10^-11]},Length[varlist]-2],4]*)


(* ::Input:: *)
(*(*now we join toegther the quartered the edged and the fixed points*)*)


(* ::Input:: *)
(*quaedfixl=Join[{quaflcor[[#]]},{edgl[[#]]},fixl[[#]]]&/@Range[1,4]*)


(* ::Input:: *)
(*quaedfixlpart=Join[{quaflcorpart[[#]]},{edglpart[[#]]},fixlpart[[#]]]&/@Range[1,4]*)


(* ::Input:: *)
(*(*the ranges over which we make the tables*)*)


(* ::Input:: *)
(*(*with exact int*)*)


(* ::Input:: *)
(*fullliml=Table[Table[Prepend[quaedfixl[[i]][[j]],tabindexlist[[j]]],{j,1,Length[varlist]-1}],{i,1,Length[quaedfixl]}]*)


(* ::Input:: *)
(*(*no exact int*)*)


(* ::Input:: *)
(*fulllimlNI=Table[Table[Prepend[quaedfixl[[i]][[j]],tabindexlistNI[[j]]],{j,1,Length[varlist]}],{i,1,Length[quaedfixl]}]*)


(* ::Input:: *)
(*fulllimlNIpart=Table[Table[Prepend[quaedfixlpart[[i]][[j]],tabindexlistNI[[j]]],{j,1,Length[varlist]}],{i,1,Length[quaedfixlpart]}]*)


(* ::Input:: *)
(*(*the actual tables we will use to predict the ratios*)*)


(* ::Input:: *)
(*(*with exact int*)*)


(* ::Input:: *)
(*edgetablimlist=Table[((Table[tabindexlist,Evaluate[Sequence@@fullliml[[i]]]])),{i,1,Length[fullliml]}]*)


(* ::Input:: *)
(*(*no exact int*)*)


(* ::Input:: *)
(*edgetablimlistNI=Table[((Table[tabindexlistNI,Evaluate[Sequence@@fulllimlNI[[i]]]])),{i,1,Length[fulllimlNI]}]*)


(* ::Input:: *)
(*edgetablimlistNIpart=Table[((Table[tabindexlistNI,Evaluate[Sequence@@fulllimlNIpart[[i]]]])),{i,1,Length[fulllimlNIpart]}]*)


(* ::Input:: *)
(*(*we need all possible permutations as we need to check along the edge in each variable!*)*)


(* ::Input:: *)
(*(*this is not trivial - I have to do the permutations in two steps*)*)


(* ::Input:: *)
(*(*this is because we need to group together the lists which have the quarter variable in the same position*)*)


(* ::Input:: *)
(*(*no exact int*)*)


(* ::Input:: *)
(*(*we need some tables*)*)


(* ::Input:: *)
(*(*we now need to have the different numbers tabulated*)*)


(* ::Input:: *)
(*(*these are a feature of the code so they are always the same*)*)


(* ::Input:: *)
(*quaflcor*)


(* ::Input:: *)
(*tabquaflcor=Prepend[quaflcor[[#]],k[1]]&/@Range[1,Length[quaflcor]]*)


(* ::Input:: *)
(*quaelems=Flatten[Table[k[1],Evaluate[Sequence@tabquaflcor[[#]]]]&/@Range[1,Length[tabquaflcor]]]*)


(* ::Input:: *)
(*edgelems=Table[k[2],Evaluate[Sequence@Prepend[edgl[[1]],k[2]]]]*)


(* ::Input:: *)
(*fixelem=fixl[[1]][[1]][[1]]*)


(* ::Input:: *)
(*(*if there are only three variables no further permutations are required*)*)


(* ::Input:: *)
(*(*no exact int*)*)


(* ::Input:: *)
(*(*it is easier to permute the variables instead*)*)


(* ::Input:: *)
(*(*STEP 1*)*)


(* ::Input:: *)
(*(*no int*)*)


(* ::Input:: *)
(*pvNI=Prepend[Permute[varlist,Cycles[{{1,#}}]]&/@Range[2,Length[varlist]],varlist]*)


(* ::Input:: *)
(*(*STEP 2*)*)


(* ::Input:: *)
(*(*no int*)*)


(* ::Input:: *)
(*ppvNI=*)
(*Table[Prepend[Permute[pvNI[[i]],Cycles[{{Flatten[Position[pvNI[[i]],varlist[[2]]]][[1]],Flatten[Position[pvNI[[i]],varlist[[#]]]][[1]]}}]]&/@Range[3,Length[varlist]],pvNI[[i]]],{i,1,Length[pvNI]}]*)


(* ::Input:: *)
(*(*now we have all the tables we need to actually predict the ID*)*)


(* ::Input:: *)
(*KinePoint1=ResKine[[Round[Length[ResKine]*0.5]]]*)


(* ::Input:: *)
(*KinePoint2=ResKine[[Round[Length[ResKine]*0.25]]]*)


(* ::Input:: *)
(*KinePoint3=ResKine[[Round[Length[ResKine]*0.75]]]*)


(* ::Input:: *)
(*KinePoint4=ResKine[[Round[Length[ResKine]*1.]]]*)


(* ::Input:: *)
(*KineList={KinePoint1,KinePoint2,KinePoint3,KinePoint4};*)


(* ::Input:: *)
(*KinePoint5=ResKine[[RandomReal[{1,201}]]]*)


(* ::Input:: *)
(*(*first - find the overall integration contour*)*)


(* ::Input:: *)
(*(*UPDATE - I want to output each result and then re-insert them*)*)


(* ::Input:: *)
(*(*you must always Abs before you Mean!*)*)


(* ::Input:: *)
(*(*the bulk diff rather than the kine diff*)*)


(* ::Input:: *)
(*(*we need seperate directories for each sector! There are a lot of bulk files!*)*)


(* ::Input:: *)
(*AlgoDir="/disk/data11/ttp/dhulme/FORM_Taylor/DIFFSELECTEST/I59E0/AlgoIntCCVarHigherPointSec10/";*)


(* ::Input:: *)
(*RatDir="/disk/data11/ttp/dhulme/FORM_Taylor/DIFFSELECTEST/I59E0/I59OTE0ratfind/";*)


(* ::Input:: *)
(*ResDir="/disk/data11/ttp/dhulme/FORM_Taylor/DIFFSELECTEST/I59E0/CCVarHigherPointResultsSec10/";*)


(* ::Input:: *)
(*SetDirectory[AlgoDir]*)


(* ::Input:: *)
(*SmooThimeSecKin=(Table[Table[Timing[NIntegrate[CAdomsec[#]/.Thread[kinenames->KineList[[k]]],Evaluate[Sequence@@CCNIELvarcalc[[i]]]]][[1]],{i,1,Length[CCNIELvarcalc]/2}],{k,1,Length[KineList]}])&/@Range[1,Length[SecList]]*)


(* ::Input:: *)
(*Put[SmooThimeSecKin,(ToString[name]<>ToString[epsord]<>"SmooThimeSecKin.txt")]*)


(* ::Input:: *)
(*(*Sec is first index then kin is second index*)*)


(* ::Input:: *)
(*MinTimeSecKin=Table[Max[SmooThimeSecKin[[#]][[k]]],{k,1,Length[KineList]}]&/@Range[1,Length[SecList]]*)


(* ::Input:: *)
(*MedTimeSecKin=Table[Median[SmooThimeSecKin[[#]][[k]]],{k,1,Length[KineList]}]&/@Range[1,Length[SecList]]*)


(* ::Input:: *)
(*ContChoiSecKin=Flatten[Table[Flatten[Position[SmooThimeSecKin[[#]][[k]],MinTimeSecKin[[#]][[k]]]][[1]],{k,1,Length[KineList]}]]&/@Range[1,Length[SecList]]*)


(* ::Input:: *)
(*GoodListSecKine=ConstantArray[ConstantArray[0,Length[KineList]],Length[SecList]];*)


(* ::Input:: *)
(*Table[If[MinTimeSecKin[[#]][[k]]>1.5*MedTimeSecKin[[#]][[k]],GoodListSecKine[[#]][[k]]=ContChoiSecKin[[#]][[k]],GoodListSecKine[[#]][[k]]=Nothing],{k,1,Length[KineList]}]&/@Range[1,Length[SecList]]*)


(* ::Input:: *)
(*WorstPoint=Position[Total[MinTimeSecKin],Min[Total[MinTimeSecKin]]];*)


(* ::Input:: *)
(*If[Length[GoodListSecKine[[#]]]>=Round[Length[KineList]/2],CCFinal[#]=ContChoiSecKin[[#]][[WorstPoint]]]&/@Range[1,Length[SecList]]*)


(* ::Input:: *)
(*KinePointFinal=Position[KineList,WorstPoint]*)


(* ::Input:: *)
(*(*alternative*)*)


(* ::Input:: *)
(*MinVarDiffTimeSecKin=Abs[-VarTimeSecKin+MinTimeSecKin]*)


(* ::Input:: *)
(*MinMinVarDiffTimeSecKin=Min[MinVarDiffTimeSecKin[[#]]]&/@Range[1,Length[SecList]]*)


(* ::Input:: *)
(*PosMinMinVarDiffTimeSecKin=Position[MinVarDiffTimeSecKin[[#]],MinMinVarDiffTimeSecKin[[#]]][[1]][[1]]&/@Range[1,Length[SecList]]*)


(* ::Input:: *)
(*If[Length[GoodListSecKine[[#]]]>=Round[Length[KineList]/2],CCFinal2[#]=ContChoiSecKin[[#]][[PosMinMinVarDiffTimeSecKin[[#]]]]]&/@Range[1,Length[SecList]]*)


(* ::Input:: *)
(*(*we have to select the sub and the cont - two levels*)*)


(* ::Input:: *)
(*PartSmooThimeSecKin=If[Length[GoodListSecKine[[#]]]<Round[Length[KineList]/2],(Table[Table[Table[Timing[NIntegrate[CAdomsecSub[[#]][[i]]/.Thread[kinenames->KineList[[k]]],Evaluate[Sequence@@CCNIELpartialvarcalc[[i]][[j]]]]][[1]],{j,1,Length[CCNIELpartialvarcalcSubCont[[1]]]}],{i,1,Length[CCNIELpartialvarcalcSubCont]}],{k,1,Length[KineList]}])]&/@Range[1,Length[SecList]]*)


(* ::Input:: *)
(*MinContTimeSecKin=If[Length[GoodListSecKine[[#]]]<Round[Length[KineList]/2],Table[Table[Max[PartSmooThimeSecKin[[#]][[k]][[j]]],{j,1,Length[SubFeynList2]}],{k,1,Length[KineList]}]]&/@Range[1,Length[SecList]]*)


(* ::Input:: *)
(*MinSubTimeSecKin=If[Length[GoodListSecKine[[#]]]<Round[Length[KineList]/2],Table[Max[MinContTimeSecKin[[#]][[k]]],{k,1,Length[KineList]}]]&/@Range[1,Length[SecList]]*)


(* ::Input:: *)
(*MedSubTimeSecKin=If[Length[GoodListSecKine[[#]]]<Round[Length[KineList]/2],Table[Median[MinContTimeSecKin[[#]][[k]]],{k,1,Length[KineList]}]]&/@Range[1,Length[SecList]]*)


(* ::Input:: *)
(*(*this is the min cont for each sub*)*)


(* ::Input:: *)
(*PartContChoiSecKin=If[Length[GoodListSecKine[[#]]]<Round[Length[KineList]/2],Table[Table[Flatten[Position[PartSmooThimeSecKin[[#]][[k]][[j]],MinContTimeSecKin[[#]][[k]][[j]]]][[1]],{j,1,Length[SubFeynList2]}],{k,1,Length[KineList]}]]&/@Range[1,Length[SecList]]*)


(* ::Input:: *)
(*PartSubChoiSecKin=If[Length[GoodListSecKine[[#]]]<Round[Length[KineList]/2],Table[Flatten[Position[MinContTimeSecKin[[#]][[k]],MinSubTimeSecKin[[#]][[k]]][[1]]][[1]],{k,1,Length[KineList]}]]&/@Range[1,Length[SecList]]*)


(* ::Input:: *)
(*PartGoodListSecKine=ConstantArray[ConstantArray[0,Length[KineList]],Length[SecList]];*)


(* ::Input:: *)
(*If[Length[GoodListSecKine[[#]]]<Round[Length[KineList]/2],Table[If[MinSubTimeSecKin[[#]][[k]]>1.5*MedSubTimeSecKin[[#]][[k]],PartGoodListSecKine[[#]][[k]]=PartSubChoiSecKin[[#]][[k]],PartGoodListSecKine[[#]][[k]]=Nothing],{k,1,Length[KineList]}]]&/@Range[1,Length[SecList]]*)


(* ::Input:: *)
(*If[Length[GoodListSecKine[[#]]]<Round[Length[KineList]/2],If[Length[PartGoodListSecKine[[#]]]>=Round[Length[KineList]/2],PartSubFinal[#]=Commonest[PartGoodListSecKine[[#]]][[1]]]]&/@Range[1,Length[SecList]]*)


(* ::Input:: *)
(*If[Length[GoodListSecKine[[#]]]<Round[Length[KineList]/2],If[Length[PartGoodListSecKine[[#]]]>=Round[Length[KineList]/2],PartKineFinal[#]=Position[PartSubChoiSecKin[[#]],PartSubFinal[#]][[1]][[1]]]]&/@Range[1,Length[SecList]]*)


(* ::Input:: *)
(*If[Length[GoodListSecKine[[#]]]<Round[Length[KineList]/2],If[Length[PartGoodListSecKine[[#]]]>=Round[Length[KineList]/2],PartContFinal[#]=PartContChoiSecKin[[#]][[PartKineFinal[#]]][[PartSubFinal[#]]]]]&/@Range[1,Length[SecList]]*)


(* ::Input:: *)
(*SetDirectory["/disk/data11/ttp/dhulme/FORM_Taylor/DIFFSELECTEST/I59E0/I59OTE0ratfind/"]*)


(* ::Input:: *)
(*Table[pvpartialNI[i]=pvNI/.Thread[varlist->SubVarFinalTab[[PartSubFinal[[i]]]]],{i,1,Length[SecList]}]*)


(* ::Input:: *)
(*Table[ppvpartialNI[i]=ppvNI/.Thread[varlist->SubVarFinalTab[[PartSubFinal[[i]]]]],{i,1,Length[SecList]}]*)


(* ::Input:: *)
(*Table[Table[Put[Table[Mean[Mean[Table[Abs[D[CAdomsec[i]/.Thread[kinenames->KinePointFinal],varlist[[l]]]/.Thread[ppvNI[[l]][[m]]->CCNIEL[[CCFinal[i]]]*Flatten[edgetablimlistNI[[k]],Length[varlist]-1][[#]]]],{m,1,(Length[varlist]-1)!/(Length[varlist]-2)!}]]&/@Range[1,Length[Flatten[edgetablimlistNI[[k]],Length[varlist]-1]]]],{k,1,4}],(ToString[name1]<>"RPSE["<>ToString[i]<>","<>ToString[l]<>"]"<>".txt")],{l,1,Length[varlist]}],{i,SecStart,SecFin}]*)


(* ::Input:: *)
(*Table[If[Length[GoodListSecKine[[i]]]<Round[Length[KineList]/2],Table[Put[Table[Mean[Mean[Table[Abs[D[CAdomsecSub[[i]][[PartSubFinal[[i]]]]/.Thread[kinenames->KinePointFinal],SubVarFinalTab[[PartSubFinal[[l]]]]]/.Thread[ppvpartialNI[i][[l]][[m]]->CCNIELpartial[[PartSubFinal[[i]]]][[PartContFinal[[i]]]]*Flatten[edgetablimlistNIpart[[k]],Length[varlist]-1][[#]]]],{m,1,(Length[varlist]-1)!/(Length[varlist]-2)!}]]&/@Range[1,Length[Flatten[edgetablimlistNIpart[[k]],Length[varlist]-1]]]],{k,1,4}],(ToString[name1]<>"partialRPSE["<>ToString[i]<>","<>ToString[l]<>"]"<>".txt")],{l,1,Length[varlist]}]],{i,SecStart,SecFin}]*)


(* ::Input:: *)
(*(*input*)*)


(* ::Input:: *)
(*Table[Table[KMRPSE[i,l]=(<<((ToString[name1]<>"RPSE["<>ToString[i]<>","<>ToString[l]<>"]"<>".txt"))),{i,SecStart,SecFin}],{l,1,Length[varlist]}];*)


(* ::Input:: *)
(*Table[If[Length[GoodListSecKine[[l]]]<Round[Length[KineList]/2],Table[pKMRPSE[i,l]=(<<((ToString[name1]<>"partialRPSE["<>ToString[i]<>","<>ToString[l]<>"]"<>".txt"))),{i,SecStart,SecFin}]],{l,1,Length[varlist]}];*)


(* ::Input:: *)
(*Table[CornKMLSE[i,l]=(List[KMRPSE[i,l][[4]]/KMRPSE[i,l][[3]]]),{i,SecStart,SecFin},{l,1,Length[varlist]}]*)


(* ::Input:: *)
(*Table[If[Length[GoodListSecKine[[l]]]<Round[Length[KineList]/2],pCornKMLSE[i,l]=(List[pKMRPSE[i,l][[4]]/pKMRPSE[i,l][[3]]])],{i,SecStart,SecFin},{l,1,Length[varlist]}]*)


(* ::Input:: *)
(*Table[StartKMLSE[i,l]=(List[KMRPSE[i,l][[2]]/KMRPSE[i,l][[1]]]),{i,SecStart,SecFin},{l,1,Length[varlist]}]*)


(* ::Input:: *)
(*Table[If[Length[GoodListSecKine[[l]]]<Round[Length[KineList]/2],pStartKMLSE[i,l]=(List[pKMRPSE[i,l][[2]]/pKMRPSE[i,l][[1]]])],{i,SecStart,SecFin},{l,1,Length[varlist]}]*)


(* ::Input:: *)
(*Table[Table[(PermKMLSE[i,l]=Permutations[KMRPSE[i,l]]),{i,SecStart,SecFin}],{l,1,Length[varlist]}];*)


(* ::Input:: *)
(*Table[Table[If[Length[GoodListSecKine[[l]]]<Round[Length[KineList]/2],(pPermKMLSE[i,l]=Permutations[pKMRPSE[i,l]])],{i,SecStart,SecFin}],{l,1,Length[varlist]}];*)


(* ::Input:: *)
(*Table[Table[PermKMDLSE[i,l]=Table[(List[PermKMLSE[i,l][[j]][[2]]/PermKMLSE[i,l][[j]][[1]],PermKMLSE[i,l][[j]][[3]]/PermKMLSE[i,l][[j]][[2]],PermKMLSE[i,l][[j]][[4]]/PermKMLSE[i,l][[j]][[3]]]),{j,1,Length[PermKMLSE[i,l]]}],{i,SecStart,SecFin}],{l,1,Length[varlist]}];*)


(* ::Input:: *)
(*Table[Table[If[Length[GoodListSecKine[[l]]]<Round[Length[KineList]/2],pPermKMDLSE[i,l]=Table[(List[pPermKMLSE[i,l][[j]][[2]]/pPermKMLSE[i,l][[j]][[1]],pPermKMLSE[i,l][[j]][[3]]/pPermKMLSE[i,l][[j]][[2]],pPermKMLSE[i,l][[j]][[4]]/pPermKMLSE[i,l][[j]][[3]]]),{j,1,Length[pPermKMLSE[i,l]]}]],{i,SecStart,SecFin}],{l,1,Length[varlist]}];*)


(* ::Input:: *)
(*Table[Table[MePermKMDLSE[i,l]=Mean[Table[PermKMDLSE[i,l][[j]],{j,1,Length[PermKMLSE[i,l]]}]],{i,SecStart,SecFin}],{l,1,Length[varlist]}]*)


(* ::Input:: *)
(*Table[Table[If[Length[GoodListSecKine[[l]]]<Round[Length[KineList]/2],pMePermKMDLSE[i,l]=Mean[Table[pPermKMDLSE[i,l][[j]],{j,1,Length[pPermKMLSE[i,l]]}]]],{i,SecStart,SecFin}],{l,1,Length[varlist]}]*)


(* ::Input:: *)
(*Which[Length[varlist]<=3,(RatKMSE=Table[Which[0.75<=(CornKMLSE[i,l][[1]]/StartKMLSE[i,l][[1]])<=1.25,{1,1,1,1},AllTrue[CornKMLSE[i,l],#>2&]||AllTrue[CornKMLSE[i,l],#<0.5&],{8,4,2,1},AllTrue[CornKMLSE[i,l],1.5<=#<=2&]||AllTrue[CornKMLSE[i,l],0.5<=#<=0.75&],{4,3,2,1},AllTrue[StartKMLSE[i,l],#>2&]||AllTrue[StartKMLSE[i,l],#<0.5&],{1,2,4,8},AllTrue[StartKMLSE[i,l],1.5<=#<=2&]||AllTrue[StartKMLSE[i,l],0.5<=#<=0.75&],{1,2,3,4},AllTrue[MePermKMDLSE[i,l],1<=#<=1.5&]||AllTrue[MePermKMDLSE[i,l],0.75<=#<=1&],{1,1,1,1},AllTrue[MePermKMDLSE[i,l],1.5<=#&]||AllTrue[MePermKMDLSE[i,l],#<=0.75&],{4,3,2,1}],{i,SecStart,SecFin},{l,1,Length[varlist]}]),Length[varlist]>3,RatKMSE=Table[Which[0.75<=(CornKMLSE[i,l][[1]]/StartKMLSE[i,l][[1]])<=1.25,ConstantArray[1,4+2*(Length[varlist]-2)],AllTrue[CornKMLSE[i,l],#>2&]||AllTrue[CornKMLSE[i,l],#<0.5&],ConstantArray[1,4+2*(Length[varlist]-2)+2],AllTrue[CornKMLSE[i,l],1.5<=#<=2&]||AllTrue[CornKMLSE[i,l],0.5<=#<=0.75&],ConstantArray[1,4+2*(Length[varlist]-2)+1],AllTrue[StartKMLSE[i,l],#>2&]||AllTrue[StartKMLSE[i,l],#<0.5&],ConstantArray[1,4+2*(Length[varlist]-2)+2],AllTrue[StartKMLSE[i,l],1.5<=#<=2&]||AllTrue[StartKMLSE[i,l],0.5<=#<=0.75&],ConstantArray[1,4+2*(Length[varlist]-2)+1],AllTrue[MePermKMDLSE[i,l],1<=#<=1.5&]||AllTrue[MePermKMDLSE[i,l],0.75<=#<=1&],ConstantArray[1,4+2*(Length[varlist]-2)],AllTrue[MePermKMDLSE[i,l],1.5<=#&]||AllTrue[MePermKMDLSE[i,l],#<=0.75&],ConstantArray[1,4+2*(Length[varlist]-2)+1]],{i,1,1},{l,1,Length[varlist]}]]*)


(* ::Input:: *)
(*(*Table[If[Length[GoodListSecKine[[l]]]<Round[Length[KineList]/2],Which[Length[varlist]\[LessEqual]3,(pRatKMSE=Which[0.75\[LessEqual](pCornKMLSE[i,l][[1]]/pStartKMLSE[i,l][[1]])\[LessEqual]1.25,{1,1,1,1},AllTrue[pCornKMLSE[i,l],#>2&]||AllTrue[pCornKMLSE[i,l],#<0.5&],{8,4,2,1},AllTrue[pCornKMLSE[i,l],1.5\[LessEqual]#\[LessEqual]2&]||AllTrue[pCornKMLSE[i,l],0.5\[LessEqual]#\[LessEqual]0.75&],{4,3,2,1},AllTrue[pStartKMLSE[i,l],#>2&]||AllTrue[pStartKMLSE[i,l],#<0.5&],{1,2,4,8},AllTrue[pStartKMLSE[i,l],1.5\[LessEqual]#\[LessEqual]2&]||AllTrue[pStartKMLSE[i,l],0.5\[LessEqual]#\[LessEqual]0.75&],{1,2,3,4},AllTrue[pMePermKMDLSE[i,l],1\[LessEqual]#\[LessEqual]1.5&]||AllTrue[pMePermKMDLSE[i,l],0.75\[LessEqual]#\[LessEqual]1&],{1,1,1,1},AllTrue[pMePermKMDLSE[i,l],1.5\[LessEqual]#&]||AllTrue[pMePermKMDLSE[i,l],#\[LessEqual]0.75&],{4,3,2,1}]),Length[varlist]>3,RatKMSE=Which[0.75\[LessEqual](pCornKMLSE[i,l][[1]]/pStartKMLSE[i,l][[1]])\[LessEqual]1.25,ConstantArray[1,4+2*(Length[varlist]-2)],AllTrue[pCornKMLSE[i,l],#>2&]||AllTrue[pCornKMLSE[i,l],#<0.5&],ConstantArray[1,4+2*(Length[varlist]-2)+2],AllTrue[pCornKMLSE[i,l],1.5\[LessEqual]#\[LessEqual]2&]||AllTrue[pCornKMLSE[i,l],0.5\[LessEqual]#\[LessEqual]0.75&],ConstantArray[1,4+2*(Length[varlist]-2)+1],AllTrue[pStartKMLSE[i,l],#>2&]||AllTrue[pStartKMLSE[i,l],#<0.5&],ConstantArray[1,4+2*(Length[varlist]-2)+2],AllTrue[pStartKMLSE[i,l],1.5\[LessEqual]#\[LessEqual]2&]||AllTrue[pStartKMLSE[i,l],0.5\[LessEqual]#\[LessEqual]0.75&],ConstantArray[1,4+2*(Length[varlist]-2)+1],AllTrue[pMePermKMDLSE[i,l],1\[LessEqual]#\[LessEqual]1.5&]||AllTrue[pMePermKMDLSE[i,l],0.75\[LessEqual]#\[LessEqual]1&],ConstantArray[1,4+2*(Length[varlist]-2)],AllTrue[pMePermKMDLSE[i,l],1.5\[LessEqual]#&]||AllTrue[pMePermKMDLSE[i,l],#\[LessEqual]0.75&],ConstantArray[1,4+2*(Length[varlist]-2)+1]]]],{i,1,Length[SecList]},{l,1,Length[varlist]}]*)*)


(* ::Input:: *)
(*(*plot diagnosis*)*)


(* ::Input:: *)
(*Table[Table[Table[Table[Plot3D[Re[CAdomsec[i]/.{theta[2]->CCNIELint[[CCFinal[i]]][[3]]*#,theta[3]->CCNIELint[[CCFinal[i]]][[4]]*j,theta[4]->CCNIELint[[CCFinal[i]]][[5]]*k}/.Thread[kinenames->ResKine[[l]]]],{theta[0],0,CCNIELint[[CCFinal[i]]][[1]]},{theta[1],0,CCNIELint[[CCFinal[i]]][[2]]},PlotRange->All],{i,SecStart,SecFin}]&/@{0.1,0.5,1.0},{j,{0.1,0.5,1.0}}],{k,{0.1,0.5,1.0}}],{l,200,200}]*)


(* ::Input:: *)
(*(*Begin Calculation*)*)


(* ::Input:: *)
(*SetDirectory["/disk/data11/ttp/dhulme/FORM_Taylor/DIFFSELECTEST/I59E0/CCVarHigherPointResultsSec10/"]*)


(* ::Input:: *)
(*COMPDiscTaySeriesIntegrator[func_,varlist_?VectorQ,conflist_?VectorQ,start_,ord_,name_,sec_]:=Block[{Jac,confFunc,Num,ExpFunc,TaylorFunc,IntFunc,TaylorResult,alpha,sum,powlist,powvarlist,elist,illist,ihlist,intlimlist,e,il,ih,pow,sumindexlist,sumlimlist,k,derlist},*)
(*powlist=pow[#]&/@Range[1,Length[varlist]];*)
(*powvarlist=varlist[[#]]^(powlist[[#]])&/@Range[1,Length[varlist]];*)
(*elist=e[#]&/@Range[1,Length[varlist]];*)
(*illist=il[#]&/@Range[1,Length[varlist]];*)
(*ihlist=ih[#]&/@Range[1,Length[varlist]];*)
(*intlimlist=({varlist[[#]],illist[[#]],ihlist[[#]]})&/@Range[1,Length[varlist]];*)
(*sumindexlist=k[#]&/@Range[1,Length[varlist]];*)
(*sumlimlist=({sumindexlist[[#]],0,ord})&/@Range[1,Length[varlist]];*)
(*Jac=Times@@((Simplify[D[conflist[[#]],varlist[[#]]],TimeConstraint->3])&/@Range[1,Length[varlist]]);*)
(*confFunc=Simplify[Jac*(func/.Thread[varlist->conflist]),TimeConstraint->3];*)
(*sum=(Flatten[Table[If[start<=(Plus@@sumindexlist)<=ord,sumindexlist,alpha]//.alpha->Sequence[],Evaluate[Sequence@@sumlimlist]],Length[varlist]-1]);*)
(*Table[derlist[i]=({varlist[[#]],sum[[i]][[#]]})&/@Range[1,Length[varlist]],{i,1,Length[sum]}];*)
(*(TaylorFunc[#]=1/(Times@@(sum[[#]]!)) (D[confFunc,Evaluate[Sequence@@derlist[#]]]/.Thread[varlist->elist]/.Thread[N[varlist]->elist]))&/@Range[1,Length[sum]];*)
(*IntFunc=Integrate[Apply[Times,((powvarlist/.Thread[powlist->sum[[#]]])&/@Range[1,Length[sum]]),{1}],Evaluate[Sequence@@intlimlist]];*)
(*(TaylorResult[#]=((TaylorFunc[#]*IntFunc[[#]])))&/@Range[1,Length[sum]];*)
(*(Put[Compress[TaylorResult[#]],(ToString[name]<>ToString[sec]<>"TAYINTCO"<>"["<>ToString[sum[[#]]]<>"].h")])&/@Range[1,Length[sum]];]*)


(* ::Input:: *)
(*COMPDiscTaySlicer[pre_,varlist_?VectorQ,conflist_?VectorQ,intstartlist_,intstoplist_,namein_,nameout_,start_,ord_,sec_]:=Block[{sum,ResultListPre,ResultList,alpha,intminlist,intmaxlist,elist,illist,ihlist,e,il,ih,sumindexlist,sumlimlist,k,y,varlist2,conflistInv,explist},*)
(*varlist2=y[#]&/@Range[1,Length[varlist]];*)
(*conflistInv=Flatten[(varlist[[#]]/.Solve[conflist[[#]]==varlist2[[#]],varlist[[#]]])&/@Range[1,Length[varlist]]];*)
(*explist=(((conflistInv[[#]]/.{varlist2[[#]]->intstartlist[[#]]})+(conflistInv[[#]]/.{varlist2[[#]]->intstoplist[[#]]}))*0.5)&/@Range[1,Length[varlist]];*)
(*intminlist=((conflistInv[[#]]/.{varlist2[[#]]->intstartlist[[#]]})-explist[[#]])&/@Range[1,Length[varlist]];*)
(*intmaxlist=((conflistInv[[#]]/.{varlist2[[#]]->intstoplist[[#]]})-explist[[#]])&/@Range[1,Length[varlist]];*)
(*elist=e[#]&/@Range[1,Length[varlist]];*)
(*illist=il[#]&/@Range[1,Length[varlist]];*)
(*ihlist=ih[#]&/@Range[1,Length[varlist]];*)
(*sumindexlist=k[#]&/@Range[1,Length[varlist]];*)
(*sumlimlist=({sumindexlist[[#]],0,ord})&/@Range[1,Length[varlist]];sum=(Flatten[Table[If[start<=(Plus@@sumindexlist)<=ord,sumindexlist,alpha]//.alpha->Sequence[],Evaluate[Sequence@@sumlimlist]],Length[varlist]-1]);*)
(*If[AllTrue[sum[[#]],EvenQ],Put[Compress[(pre)*((Uncompress[(<<((ToString[namein]<>ToString[sec]<>"TAYINTCO"<>"["<>ToString[sum[[#]]]<>"].h")))])/.Thread[elist->explist]/.Thread[illist->intminlist]/.Thread[ihlist->intmaxlist])],(ToString[nameout]<>"SECTOR"<>ToString[sec]<>"DISCTAYINTCO"<>"["<>ToString[sum[[#]]]<>"].h")]]&/@Range[1,Length[sum]];*)
(*]*)


(* ::Input:: *)
(*COMPDiscTaySliceOutput[pre_,sec_,name_,varlist_,conflist_,startord_,endord_,limintlist_,ralist_]:=Block[{intstartlist,intstoplist,ratlist,delta,difflist,deltalist,intlist,tabindexlist,tablimlist,ratlist2,k,intlisttab,ThRes,KineRes,sumindexlist,sumlimlist,sum,alpha,ThResPre},*)
(*intstartlist=limintlist[[#]][[1]]&/@Range[1,Length[limintlist]];*)
(*intstoplist=limintlist[[#]][[2]]&/@Range[1,Length[limintlist]];*)
(*sumindexlist=k[#]&/@Range[1,Length[varlist]];*)
(*sumlimlist=({sumindexlist[[#]],0,endord})&/@Range[1,Length[varlist]];sum=(Flatten[Table[If[startord<=(Plus@@sumindexlist)<=endord,sumindexlist,alpha]//.alpha->Sequence[],Evaluate[Sequence@@sumlimlist]],Length[varlist]-1]);*)
(*(ratlist[#]=Append[ralist[[#]],0])&/@Range[1,Length[varlist]];*)
(*(delta[#]=Total[ratlist[#]])&/@Range[1,Length[varlist]];*)
(*(difflist[#]=Table[ratlist[#][[i]]/delta[#]*intstoplist[[#]],{i,1,Length[ratlist[#]]}])&/@Range[1,Length[varlist]];*)
(*(deltalist[#]=Table[Sum[difflist[#][[i]],{i,1,j}],{j,1,Length[difflist[#]]-1}])&/@Range[1,Length[varlist]];*)
(*(intlist[#]=Partition[Prepend[deltalist[#],intstartlist[[#]]],2,1])&/@Range[1,Length[varlist]];*)
(*tabindexlist=k[#]&/@Range[1,Length[varlist]];*)
(*tablimlist=({tabindexlist[[#]],1,Length[intlist[#]]})&/@Range[1,Length[varlist]];*)
(*intlisttab=(Flatten[Table[tabindexlist,Evaluate[Sequence@@tablimlist]],Length[varlist]-1]);*)
(*(COMPDiscTaySlicer[pre,varlist,conflist,Table[intlist[i][[intlisttab[[#]][[i]]]][[1]],{i,1,Length[varlist]}],Table[intlist[i][[intlisttab[[#]][[i]]]][[2]],{i,1,Length[varlist]}],(ToString[name]),ToString[name]<>ToString[intlisttab[[#]]],startord,endord,sec])&/@Range[1,Length[intlisttab]];*)
(*]*)


(* ::Input:: *)
(*(*we only want to # over one list when we insert the kinematic numbers*)*)


(* ::Input:: *)
(*(*this does not speed things up for one point but it DOES for multiple points!*)*)


(* ::Input:: *)
(*(*We MUST NOT multiply by the prefactor twice!*)*)


(* ::Input:: *)
(*COMPKineTABTABFlexVarIndFast3ThWheelB[sec_,name_,varlist_,conflist_,startord_,endord_,limintlist_,ralist_,knamelist_,kvallist_,knumlist_]:=Block[{ratlist,delta,difflist,deltalist,intlist,tabindexlist,tablimlist,ratlist2,k,intlisttab,ThRes,KineRes,sumindexlist,sumlimlist,sum,alpha,intstartlist,intstoplist,KineResPre,FullIDlist},*)
(*intstartlist=limintlist[[#]][[1]]&/@Range[1,Length[limintlist]];*)
(*intstoplist=limintlist[[#]][[2]]&/@Range[1,Length[limintlist]];*)
(*sumindexlist=k[#]&/@Range[1,Length[varlist]];*)
(*sumlimlist=({sumindexlist[[#]],0,endord})&/@Range[1,Length[varlist]];sum=(Flatten[Table[If[startord<=(Plus@@sumindexlist)<=endord,sumindexlist,alpha]//.alpha->Sequence[],Evaluate[Sequence@@sumlimlist]],Length[varlist]-1]);*)
(*(ratlist[#]=Append[ralist[[#]],0])&/@Range[1,Length[varlist]];*)
(*(delta[#]=Total[ratlist[#]])&/@Range[1,Length[varlist]];*)
(*(difflist[#]=Table[ratlist[#][[i]]/delta[#]*intstoplist[[#]],{i,1,Length[ratlist[#]]}])&/@Range[1,Length[varlist]];*)
(*(deltalist[#]=Table[Sum[difflist[#][[i]],{i,1,j}],{j,1,Length[difflist[#]]-1}])&/@Range[1,Length[varlist]];*)
(*(intlist[#]=Partition[Prepend[deltalist[#],intstartlist[[#]]],2,1])&/@Range[1,Length[varlist]];*)
(*tabindexlist=k[#]&/@Range[1,Length[varlist]];*)
(*tablimlist=({tabindexlist[[#]],1,Length[intlist[#]]})&/@Range[1,Length[varlist]];*)
(*intlisttab=(Flatten[Table[tabindexlist,Evaluate[Sequence@@tablimlist]],Length[varlist]-1]);*)
(*FullIDlist=Tuples[{sum,intlisttab}];*)
(*((KineResPre[#]=If[AllTrue[FullIDlist[[#]][[1]],EvenQ],((Uncompress[<<(ToString[name]<>ToString[FullIDlist[[#]][[2]]]<>"SECTOR"<>ToString[sec]<>"DISCTAYINTCO"<>"["<>ToString[FullIDlist[[#]][[1]]]<>"].h")])/.Thread[knamelist->kvallist]),0])&/@Range[1,Length[FullIDlist]]);*)
(*KineRes=Total[(KineResPre[#])&/@Range[1,Length[FullIDlist]]];*)
(*Put[KineRes,(ToString[name]<>"SECTOR"<>ToString[sec]<>"KinePoint"<>ToString[knumlist]<>"TayOrd"<>ToString[startord]<>ToString[endord]<>".txt")];*)
(*Return[KineRes]*)
(*]*)


(* ::Input:: *)
(*COMPDiscTaySeriesIntegrator[CAdomsec[#],varlist,varlist,0,4,(ToString[name]<>"OTEPS"<>ToString[epsord]),#]&/@Range[SecStart,SecFin]*)


(* ::Input:: *)
(*If[Length[GoodListSecKine[[#]]]<Round[Length[KineList]/2],COMPDiscTaySeriesIntegrator[CAdomsecSub[[#]][[PartSubFinal[#]]],SubVarFinalTab[[PartSubFinal[#]]],SubVarFinalTab[[PartSubFinal[#]]],0,4,(ToString[name]<>"OTEPS"<>ToString[epsord]),#]]&/@Range[SecStart,SecFin]*)


(* ::Input:: *)
(*COMPDiscTaySliceOutput[1,#,(ToString[name]<>"OTEPS"<>ToString[epsord]),varlist,varlist,0,4,CCNIELcalc[[CCFinal[#]]],RatKMSE]&/@Range[SecStart,SecFin]*)


(* ::Input:: *)
(*If[Length[GoodListSecKine[[#]]]<Round[Length[KineList]/2],COMPDiscTaySliceOutput[1,#,(ToString[name]<>"OTEPS"<>ToString[epsord]),SubVarFinalTab[[PartSubFinal[#]]],SubVarFinalTab[[PartSubFinal[#]]],0,4,CCNIELpartialcalc[[PartSubFinal[#]]][[PartContFinal[#]]],RatKMSE]]&/@Range[SecStart,SecFin]*)


(* ::Input:: *)
(*Table[COMPKineTABTABFlexVarIndFast3ThWheelB[i,i,(ToString[name]<>"OTEPS"<>ToString[epsord]),varlist,varlist,0,4,CCNIELcalc[[CCFinal[i]]],RatKMSE,kinenames,ResKine[[#]],#],{i,SecStart,SecFin}]&/@Range[1,Length[ResKine]]*)


(* ::Input:: *)
(*Table[If[Length[GoodListSecKine[[i]]]<Round[Length[KineList]/2],COMPKineTABTABFlexVarIndFast3ThWheelB[i,i,(ToString[name]<>"OTEPS"<>ToString[epsord]),SubVarFinalTab[[PartSubFinal[#]]],SubVarFinalTab[[PartSubFinal[#]]],0,4,CCNIELpartialcalc[[PartSubFinal[#]]][[PartContFinal[#]]],RatKMSE,kinenames,ResKine[[#]],#]],{i,SecStart,SecFin}]&/@Range[1,Length[ResKine]]*)
