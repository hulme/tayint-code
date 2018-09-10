(*Our finalised algorithm*)

<<TayIntfunctions.m


(* ::Input:: *)
(*(*the user needs to input the prefactor and the start and end order of the expansion as well as integration and kinematic variables and the numbers and the actual integrands plus the precision with which you would like to scan the surfaces to determine which ID to use*)*)


(*(*BEGIN user input*)*)
(*(*directories*)*)
{SecDecDir="/home/ttp/dhulme/SecDec-3.0.9/loop/tridom1correct/I246/I246_EUC"}
{ItSecDir="/home/ttp/dhulme/SecDec-3.0.9/loop/tridom1correct/I246/I246_EUC/numerics/together/epstothe0";}
{CalcDir="/home/ttp/dhulme/SecDec-3.0.9/loop/tridom1correct/I246/I246_EUC/TayIntAuto/calc/36"}
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
KineScalesVal={0.5*29929,-2*29929}
KineMassesVal={29929}
ResKineList=Join[KineRun,KineScalesVal,KineMassesVal]
{ResKine=N[Table[ResKineList,{i,0*29929,4*29929,4/50*29929}]];}
(*(*the variables*)*)
{Feynlist={x0,x1,x2,x23,x4,x5};}
(*(*If the user wants to request a certain number of partitions in advance - but this is not needed as we work this out as part of our algorithm*)*)
(*(*END USER INPUT*)*) 
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
PartNum=10;
(*(*new variables*)*)
varlist=theta[#-1]&/@Range[1,Length[Feynlist]]
(*the original integration limits*)
intlist={0,1.}
(*the complex trafos*)
{comptranslist=(Mean[intlist]-Mean[intlist]*Exp[-I*varlist[[#]]])&/@Range[1,Length[varlist]]}
(*(*the jacobian*)*)
compJac=Times@@(D[comptranslist[[#]],varlist[[#]]]&/@Range[1,Length[varlist]])
(*(*The complex sectors-note - our naming system is that the last names before the brackets correspond to the numbers in the brackets respectively*)*)
{(CAdomsec[#]=(compJac)*(SecList[[#]]/.Thread[Feynlist->comptranslist]))&/@Range[1,Length[SecList]];}
(*the possible contours*)
CCNIEL=Tuples[{-1,1},{Length[varlist]}]
CCNIELint=CCNIEL/.{-1->-Pi,1->Pi}
CCNIELcalc=CCNIEL/.{-1->{0,-Pi},1->{0,Pi}}
CCNIELvarcalc=Table[Prepend[CCNIELcalc[[#]][[i]],varlist[[i]]],{i,1,Length[varlist]}]&/@Range[1,Length[CCNIELint]];
(*(*the partial contours*)*)
del[in_List,(q_Integer)?Positive]:=DeleteCases[in,x_/;Length[x]<q];
del2[in_List,(q_Integer)?Positive]:=DeleteCases[in,x_/;Length[x]>=q];
del3[in_List,(q_Integer)?Positive,(r_Integer)?Positive]:=DeleteCases[in,x_/;q<=Length[x]<=r];
SubFeynList1=Drop[Subsets[Table[i,{i,1,Length[Feynlist]}]],-1]
{SubFeynList2=del2[del[SubFeynList1,Length[varlist]-2],Length[varlist]-1]}
{SubFeynList3=Table[Part[Feynlist,SubFeynList2[[i]][[#]]]&/@Range[1,Length[SubFeynList2[[i]]]],{i,1,Length[SubFeynList2]}]}
{SubVarList3=Table[Part[varlist,SubFeynList2[[i]][[#]]]&/@Range[1,Length[SubFeynList2[[i]]]],{i,1,Length[SubFeynList2]}]}
{SubVarFinalTab=(Feynlist/.Thread[SubFeynList3[[#]]->SubVarList3[[#]]])&/@Range[1,Length[SubFeynList3]]}
{SubCompTransList3=Table[Part[comptranslist,SubFeynList2[[i]][[#]]]&/@Range[1,Length[SubFeynList2[[i]]]],{i,1,Length[SubFeynList2]}]}
{compJacSub=Table[Times@@(D[comptranslist[[#]],varlist[[#]]]&/@SubFeynList2[[i]]),{i,1,Length[SubFeynList2]}]}
(*the partially mapped sectors*)
{CAdomsecSub=Table[((compJacSub[[i]])*(SecList[[#]]/.Thread[SubFeynList3[[i]]->SubCompTransList3[[i]]])),{i,1,Length[SubFeynList3]}]&/@Range[1,Length[SecList]];}
{CCNIELSub=(Tuples[{-1,1},{Length[SubFeynList2[[#]]]}])&/@Range[1,Length[SubFeynList2]]/.{1->Pi,-1->-Pi}}
{CCNIELSubFin=Take[CCNIELSub[[#]],Length[CCNIELSub[[#]]]/2]&/@Range[1,Length[SubFeynList2]]}
{CCNIELbaselist=Table[ConstantArray[ConstantArray[1,Length[Feynlist]],Length[CCNIELSubFin[[i]]]],{i,1,Length[CCNIELSubFin]}]}
{Table[(CCNIELbaselist[[i]][[#]][[SubFeynList2[[i]]]]=CCNIELSubFin[[i]][[#]])&/@Range[1,Length[CCNIELSubFin[[i]]]],{i,1,Length[SubFeynList2]}]}
CCNIELpartial=CCNIELbaselist
CCNIELpartialcalc=CCNIELpartial/.{1->{0,1},-Pi->{0,-Pi},Pi->{0,Pi}}
{Table[(CCNIELbaselist[[i]][[#]][[SubFeynList2[[i]]]]=CCNIELSubFin[[i]][[#]])&/@Range[1,Length[CCNIELSubFin[[i]]]],{i,1,Length[SubFeynList2]}]}
{CCNIELpartialvarcalcSubCont=Table[Table[If[CCNIELpartial[[#]][[i]][[j]]==1,Prepend[CCNIELpartialcalc[[#]][[i]][[j]],Feynlist[[j]]],Prepend[CCNIELpartialcalc[[#]][[i]][[j]],varlist[[j]]]],{j,1,Length[varlist]}],{i,1,Length[CCNIELSubFin[[#]]]}]&/@Range[1,Length[SubFeynList2]];}
(*(*now we make the bulk tables*)*)
tabindexlist=k[#]&/@Range[1,Length[varlist]-1]
tabindexlistNI=k[#]&/@Range[1,Length[varlist]]
(*(*we do not want to go exactly onto Pi as there are very rapid changes there which are obscure the other changes*)*)
{bulktablimlist=({tabindexlist[[#]],0,9.99*Pi/10,(9.99*Pi/10-0)*0.1})&/@Range[1,Length[varlist]-1]}
{bulktablimlistfull=({tabindexlist[[#]],0,Pi,(Pi-0)*0.1})&/@Range[1,Length[varlist]-1]}
{bulktab=(Flatten[Table[tabindexlist,Evaluate[Sequence@@bulktablimlist]],Length[varlist]-2]);}
{bulktabfull=(Flatten[Table[tabindexlist,Evaluate[Sequence@@bulktablimlistfull]],Length[varlist]-2]);}
{bulktablimlistNI=({tabindexlistNI[[#]],0,9.99*Pi/10,(9.99*Pi/10-0)*0.1})&/@Range[1,Length[varlist]]}
{bulktabpartiallimlistNI=({tabindexlistNI[[#]],0,9.99*1/10,(9.99*1/10-0)*0.1})&/@Range[1,Length[varlist]]}
{bulktablimlistfullNI=({tabindexlistNI[[#]],0,Pi,(Pi-0)*0.1})&/@Range[1,Length[varlist]]}
(*(*exclude the corners and look in more detail*)*)
{ncbulktablimlistNI=({tabindexlistNI[[#]],0,8*Pi/10,(8*Pi/10-0)*0.05})&/@Range[1,Length[varlist]]}
{bulktabNI=(Flatten[Table[tabindexlistNI,Evaluate[Sequence@@bulktablimlistNI]],Length[varlist]-1]);}
{bulktabpartialNI=(Flatten[Table[tabindexlistNI,Evaluate[Sequence@@bulktabpartiallimlistNI]],Length[varlist]-1]);}
{bulktabfullNI=(Flatten[Table[tabindexlistNI,Evaluate[Sequence@@bulktablimlistfullNI]],Length[varlist]-1]);}
{bulktabncNI=(Flatten[Table[tabindexlistNI,Evaluate[Sequence@@ncbulktablimlistNI]],Length[varlist]-1]);}
(*(*now we make the edge table*)*)
(*(*the raw integration boundary will always be Pi*)*)
(*(*the numbers here are always the same-a feature of the algorithm*)*)
quat=Table[i,{i,0,Pi,0.25*Pi}]
quatpart=Table[i,{i,0,1,0.25*1}]
qual=Partition[quat,2,1]
qualpart=Partition[quatpart,2,1]
(*(*I do not want to hit Pi exactly because there are always very rapid changes there*)*)
qualcor=ReplacePart[qual,{4,2}->9.99*Pi/10]
qualcorpart=ReplacePart[qualpart,{4,2}->9.99*1/10]
quafl=Append[qualcor[[#]],0.125*Pi]&/@Range[1,Length[qualcor]]
{quaflpart=Append[qualcorpart[[#]],0.125*1]&/@Range[1,Length[qualcorpart]]}
{(*(*the list for the quartered edge - strictly speaking I could just type this directly as it will always be the same*)*)}
quaflcor=ReplacePart[quafl,{4,3}->(9.99*Pi/10-3*Pi/4)*0.5]
quaflcorpart=ReplacePart[quaflpart,{4,3}->(9.99*1/10-3*1/4)*0.5]
(*(*this list will always be the same - it is a feature of the code*)*)
{edgl=List[{0.9*Pi,9.99*Pi/10,(9.99*Pi/10-9*Pi/10)*0.5},{0.9*Pi,9.99*Pi/10,(9.99*Pi/10-9*Pi/10)*0.5},{0.9*Pi,9.99*Pi/10,(9.99*Pi/10-9*Pi/10)*0.5},{0.9*Pi,9.99*Pi/10,(9.99*Pi/10-9*Pi/10)*0.5}];}
{edglpart=List[{0.9*1,9.99*1/10,(9.99*1/10-9*1/10)*0.5},{0.9*1,9.99*1/10,(9.99*1/10-9*1/10)*0.5},{0.9*1,9.99*1/10,(9.99*1/10-9*1/10)*0.5},{0.9*1,9.99*1/10,(9.99*1/10-9*1/10)*0.5}];}
(*(*we need to fix all but two of the variables*)*)
{(*(* I do not want to use Pi/2 exactly as that is also in the quarter list*)*)}
{fixl=ConstantArray[ConstantArray[{N[Pi/2+10^-11],N[Pi/2+10^-11]},Length[varlist]-2],4]}
{fixlpart=ConstantArray[ConstantArray[{N[1/2+10^-11],N[1/2+10^-11]},Length[varlist]-2],4]}
(*(*now we join toegther the quartered the edged and the fixed points*)*)
quaedfixl=Join[{quaflcor[[#]]},{edgl[[#]]},fixl[[#]]]&/@Range[1,4]
{quaedfixlpart=Join[{quaflcorpart[[#]]},{edglpart[[#]]},fixlpart[[#]]]&/@Range[1,4]}
(*(*the ranges over which we make the tables*)*)
{fulllimlNI=Table[Table[Prepend[quaedfixl[[i]][[j]],tabindexlistNI[[j]]],{j,1,Length[varlist]}],{i,1,Length[quaedfixl]}]}
{fulllimlNIpart=Table[Table[Prepend[quaedfixlpart[[i]][[j]],tabindexlistNI[[j]]],{j,1,Length[varlist]}],{i,1,Length[quaedfixlpart]}]}
(*(*the actual tables we will use to predict the ratios*)*)
{edgetablimlistNI=Table[((Table[tabindexlistNI,Evaluate[Sequence@@fulllimlNI[[i]]]])),{i,1,Length[fulllimlNI]}]}
{edgetablimlistNIpart=Table[((Table[tabindexlistNI,Evaluate[Sequence@@fulllimlNIpart[[i]]]])),{i,1,Length[fulllimlNIpart]}]}
{(*(*we need all possible permutations as we need to check along the edge in each variable!*)*)}
(*(*this is not trivial - I have to do the permutations in two steps*)*)
{(*(*this is because we need to group together the lists which have the quarter variable in the same position*)*)}
(*(*we need some tables*)*)
(*(*we now need to have the different numbers tabulated*)*)
(*(*these are a feature of the code so they are always the same*)*)
quaflcor
tabquaflcor=Prepend[quaflcor[[#]],k[1]]&/@Range[1,Length[quaflcor]]
{quaelems=Flatten[Table[k[1],Evaluate[Sequence@tabquaflcor[[#]]]]&/@Range[1,Length[tabquaflcor]]]}
edgelems=Table[k[2],Evaluate[Sequence@Prepend[edgl[[1]],k[2]]]]
fixelem=fixl[[1]][[1]][[1]]
(*(*if there are only three variables no further permutations are required*)*)
(*(*it is easier to permute the variables instead*)*)
{pvNI=Prepend[Permute[varlist,Cycles[{{1,#}}]]&/@Range[2,Length[varlist]],varlist]}
{ppvNI=Table[Prepend[Permute[pvNI[[i]],Cycles[{{Flatten[Position[pvNI[[i]],varlist[[2]]]][[1]],Flatten[Position[pvNI[[i]],varlist[[#]]]][[1]]}}]]&/@Range[3,Length[varlist]],pvNI[[i]]],{i,1,Length[pvNI]}]}
(*(*now we have all the tables we need to actually predict the ID*)*)
Quit
