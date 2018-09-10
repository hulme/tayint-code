(*Our finalised algorithm*)

<<TayIntfunctions.m


(* ::Input:: *)
(*(*the user needs to input the prefactor and the start and end order of the expansion as well as integration and kinematic variables and the numbers and the actual integrands plus the precision with which you would like to scan the surfaces to determine which ID to use*)*)


(*(*BEGIN user input*)*)
(*(*directories*)*)
{SecDecDir="D1"}
{ItSecDir="D2";}
{CalcDir="D3"}
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
SecList=getSectors[f,#]&/@Range[1,S1]/.Thread[SecDecList->kinenames];
(*(*series start and end order*)*)
serstart=ConstantArray[0,Length[SecList]]
serend=ConstantArray[T1,Length[SecList]]
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
(*The kinematic points at which we want to scan the different surfaces*)
KinePoint1=ResKine[[Round[Length[ResKine]*0.5]]]
KinePoint2=ResKine[[Round[Length[ResKine]*0.25]]]
KinePoint3=ResKine[[Round[Length[ResKine]*0.75]]]
KinePoint4=ResKine[[Round[Length[ResKine]*1.]]]
KineList={KinePoint1,KinePoint2,KinePoint3,KinePoint4};
KinePoint5=ResKine[[RandomReal[{1,201}]]]
(*Now we scan the surfaces to assess their smoothness*)
SetDirectory["D4"]
(*(*Sec is first index then kin is second index*)*)
{SmooThimeSecKin=(Table[Table[Timing[NIntegrate[CAdomsec[#]/.Thread[kinenames->KineList[[k]]],Evaluate[Sequence@@CCNIELvarcalc[[i]]]]][[1]],{i,1,Length[CCNIELvarcalc]/2}],{k,1,Length[KineList]}])&/@Range[1,Length[SecList]];}
{Put[SmooThimeSecKin,(ToString[name]<>ToString[epsord]<>"SmooThimeSecKin.txt")]}
{MinTimeSecKin=Table[Max[SmooThimeSecKin[[#]][[k]]],{k,1,Length[KineList]}]&/@Range[1,Length[SecList]]}
{MedTimeSecKin=Table[Median[SmooThimeSecKin[[#]][[k]]],{k,1,Length[KineList]}]&/@Range[1,Length[SecList]]}
{ContChoiSecKin=Flatten[Table[Flatten[Position[SmooThimeSecKin[[#]][[k]],MinTimeSecKin[[#]][[k]]]][[1]],{k,1,Length[KineList]}]]&/@Range[1,Length[SecList]]}
{GoodListSecKine=ConstantArray[ConstantArray[0,Length[KineList]],Length[SecList]];}
{Table[If[MinTimeSecKin[[#]][[k]]>1.5*MedTimeSecKin[[#]][[k]],GoodListSecKine[[#]][[k]]=ContChoiSecKin[[#]][[k]],GoodListSecKine[[#]][[k]]=Nothing],{k,1,Length[KineList]}]&/@Range[1,Length[SecList]]}
WorstPoint=Position[Total[MinTimeSecKin],Min[Total[MinTimeSecKin]]];
{If[Length[GoodListSecKine[[#]]]>=Round[Length[KineList]/2],CCFinal[#]=ContChoiSecKin[[#]][[WorstPoint]]]&/@Range[1,Length[SecList]]}
KinePointFinal=Position[KineList,WorstPoint]
(*Now the partial contours*)
(*(*we have to select the sub and the cont - two levels*)*)
{PartSmooThimeSecKin=If[Length[GoodListSecKine[[#]]]<Round[Length[KineList]/2],(Table[Table[Table[Timing[NIntegrate[CAdomsecSub[[#]][[i]]/.Thread[kinenames->KineList[[k]]],Evaluate[Sequence@@CCNIELpartialvarcalc[[i]][[j]]]]][[1]],{j,1,Length[CCNIELpartialvarcalcSubCont[[1]]]}],{i,1,Length[CCNIELpartialvarcalcSubCont]}],{k,1,Length[KineList]}])]&/@Range[1,Length[SecList]]}
{MinContTimeSecKin=If[Length[GoodListSecKine[[#]]]<Round[Length[KineList]/2],Table[Table[Max[PartSmooThimeSecKin[[#]][[k]][[j]]],{j,1,Length[SubFeynList2]}],{k,1,Length[KineList]}]]&/@Range[1,Length[SecList]]}
{MinSubTimeSecKin=If[Length[GoodListSecKine[[#]]]<Round[Length[KineList]/2],Table[Max[MinContTimeSecKin[[#]][[k]]],{k,1,Length[KineList]}]]&/@Range[1,Length[SecList]]}
{MedSubTimeSecKin=If[Length[GoodListSecKine[[#]]]<Round[Length[KineList]/2],Table[Median[MinContTimeSecKin[[#]][[k]]],{k,1,Length[KineList]}]]&/@Range[1,Length[SecList]]}
(*(*Below is the min cont for each sub*)*)
{PartContChoiSecKin=If[Length[GoodListSecKine[[#]]]<Round[Length[KineList]/2],Table[Table[Flatten[Position[PartSmooThimeSecKin[[#]][[k]][[j]],MinContTimeSecKin[[#]][[k]][[j]]]][[1]],{j,1,Length[SubFeynList2]}],{k,1,Length[KineList]}]]&/@Range[1,Length[SecList]]}
(*Below is the choice of substitution*)
{PartSubChoiSecKin=If[Length[GoodListSecKine[[#]]]<Round[Length[KineList]/2],Table[Flatten[Position[MinContTimeSecKin[[#]][[k]],MinSubTimeSecKin[[#]][[k]]][[1]]][[1]],{k,1,Length[KineList]}]]&/@Range[1,Length[SecList]]}
{PartGoodListSecKine=ConstantArray[ConstantArray[0,Length[KineList]],Length[SecList]];}
(*we now select the contours which are significantly smoother than average*)
{If[Length[GoodListSecKine[[#]]]<Round[Length[KineList]/2],Table[If[MinSubTimeSecKin[[#]][[k]]>1.5*MedSubTimeSecKin[[#]][[k]],PartGoodListSecKine[[#]][[k]]=PartSubChoiSecKin[[#]][[k]],PartGoodListSecKine[[#]][[k]]=Nothing],{k,1,Length[KineList]}]]&/@Range[1,Length[SecList]]}
(*Then we select the commonest contour from the ones that are smooth enough*)
(*sub*)
{If[Length[GoodListSecKine[[#]]]<Round[Length[KineList]/2],If[Length[PartGoodListSecKine[[#]]]>=Round[Length[KineList]/2],PartSubFinal[#]=Commonest[PartGoodListSecKine[[#]]][[1]]]]&/@Range[1,Length[SecList]]}
(*kine*)
{If[Length[GoodListSecKine[[#]]]<Round[Length[KineList]/2],If[Length[PartGoodListSecKine[[#]]]>=Round[Length[KineList]/2],PartKineFinal[#]=Position[PartSubChoiSecKin[[#]],PartSubFinal[#]][[1]][[1]]]]&/@Range[1,Length[SecList]]}
(*cont*)
{If[Length[GoodListSecKine[[#]]]<Round[Length[KineList]/2],If[Length[PartGoodListSecKine[[#]]]>=Round[Length[KineList]/2],PartContFinal[#]=PartContChoiSecKin[[#]][[PartKineFinal[#]]][[PartSubFinal[#]]]]]&/@Range[1,Length[SecList]]}
(*now we want to find the ratios*)
SetDirectory["D5"]
(*the tables we need to set it up*)
{Table[pvpartialNI[i]=pvNI/.Thread[varlist->SubVarFinalTab[[PartSubFinal[[i]]]]],{i,1,Length[SecList]}]}
{Table[ppvpartialNI[i]=ppvNI/.Thread[varlist->SubVarFinalTab[[PartSubFinal[[i]]]]],{i,1,Length[SecList]}]}
(*output*)
{Table[Table[Put[Table[Mean[Mean[Table[Abs[D[CAdomsec[i]/.Thread[kinenames->KinePointFinal],varlist[[l]]]/.Thread[ppvNI[[l]][[m]]->CCNIEL[[CCFinal[i]]]*Flatten[edgetablimlistNI[[k]],Length[varlist]-1][[#]]]],{m,1,(Length[varlist]-1)!/(Length[varlist]-2)!}]]&/@Range[1,Length[Flatten[edgetablimlistNI[[k]],Length[varlist]-1]]]],{k,1,4}],(ToString[name1]<>"RPSE["<>ToString[i]<>","<>ToString[l]<>"]"<>".txt")],{l,1,Length[varlist]}],{i,1,Length[SecList]}]}
(*partial*)
{Table[If[Length[GoodListSecKine[[i]]]<Round[Length[KineList]/2],Table[Put[Table[Mean[Mean[Table[Abs[D[CAdomsecSub[[i]][[PartSubFinal[i]]]/.Thread[kinenames->KinePointFinal],SubVarFinalTab[[PartSubFinal[i]]][[l]]]/.Thread[ppvpartialNI[i][[l]][[m]]->CCNIELpartial[[PartSubFinal[i]]][[PartContFinal[i]]]*Flatten[edgetablimlistNIpart[[k]],Length[varlist]-1][[#]]]],{m,1,(Length[varlist]-1)!/(Length[varlist]-2)!}]]&/@Range[1,Length[Flatten[edgetablimlistNIpart[[k]],Length[varlist]-1]]]],{k,1,4}],(ToString[name1]<>"partialRPSE["<>ToString[i]<>","<>ToString[l]<>"]"<>".txt")],{l,1,Length[varlist]}]],{i,SecStart,SecFin}]}
{Table[Table[KMRPSE[i,l]=(Import[((ToString[name1]<>"RPSE["<>ToString[i]<>","<>ToString[l]<>"]"<>".txt"))]),{i,1,Length[SecList]}],{l,1,Length[varlist]}];}
{Table[If[Length[GoodListSecKine[[i]]]<Round[Length[KineList]/2],Table[pKMRPSE[i,l]=(Import[((ToString[name1]<>"partialRPSE["<>ToString[i]<>","<>ToString[l]<>"]"<>".txt"))]),{i,1,Length[SecList]}]],{l,1,Length[varlist]}];}
{Table[CornKMLSE[i,l]=(List[KMRPSE[i,l][[4]]/KMRPSE[i,l][[3]]]),{i,1,Length[SecList]},{l,1,Length[varlist]}]}
{Table[If[Length[GoodListSecKine[[i]]]<Round[Length[KineList]/2],pCornKMLSE[i,l]=(List[pKMRPSE[i,l][[4]]/pKMRPSE[i,l][[3]]])],{i,1,Length[SecList]},{l,1,Length[varlist]}]}
{Table[If[Length[GoodListSecKine[[i]]]<Round[Length[KineList]/2],pCornKMLSE[i,l]=(List[pKMRPSE[i,l][[4]]/pKMRPSE[i,l][[3]]])],{i,1,Length[SecList]},{l,1,Length[varlist]}]}
{Table[StartKMLSE[i,l]=(List[KMRPSE[i,l][[2]]/KMRPSE[i,l][[1]]]),{i,1,Length[SecList]},{l,1,Length[varlist]}]}
{Table[If[Length[GoodListSecKine[[i]]]<Round[Length[KineList]/2],pStartKMLSE[i,l]=(List[pKMRPSE[i,l][[2]]/pKMRPSE[i,l][[1]]])],{i,1,Length[SecList]},{l,1,Length[varlist]}]}
{Table[Table[(PermKMLSE[i,l]=Permutations[KMRPSE[i,l]]),{i,1,Length[SecList]}],{l,1,Length[varlist]}];}
{Table[Table[If[Length[GoodListSecKine[[i]]]<Round[Length[KineList]/2],(pPermKMLSE[i,l]=Permutations[pKMRPSE[i,l]])],{i,1,Length[SecList]}],{l,1,Length[varlist]}];}
{Table[Table[PermKMDLSE[i,l]=Table[(List[PermKMLSE[i,l][[j]][[2]]/PermKMLSE[i,l][[j]][[1]],PermKMLSE[i,l][[j]][[3]]/PermKMLSE[i,l][[j]][[2]],PermKMLSE[i,l][[j]][[4]]/PermKMLSE[i,l][[j]][[3]]]),{j,1,Length[PermKMLSE[i,l]]}],{i,1,Length[SecList]}],{l,1,Length[varlist]}];}
{Table[Table[If[Length[GoodListSecKine[[i]]]<Round[Length[KineList]/2],pPermKMDLSE[i,l]=Table[(List[pPermKMLSE[i,l][[j]][[2]]/pPermKMLSE[i,l][[j]][[1]],pPermKMLSE[i,l][[j]][[3]]/pPermKMLSE[i,l][[j]][[2]],pPermKMLSE[i,l][[j]][[4]]/pPermKMLSE[i,l][[j]][[3]]]),{j,1,Length[pPermKMLSE[i,l]]}]],{i,1,Length[SecList]}],{l,1,Length[varlist]}];}
{Table[Table[MePermKMDLSE[i,l]=Mean[Table[PermKMDLSE[i,l][[j]],{j,1,Length[PermKMLSE[i,l]]}]],{i,1,Length[SecList]}],{l,1,Length[varlist]}]}
{Table[Table[If[Length[GoodListSecKine[[i]]]<Round[Length[KineList]/2],pMePermKMDLSE[i,l]=Mean[Table[pPermKMDLSE[i,l][[j]],{j,1,Length[pPermKMLSE[i,l]]}]]],{i,1,Length[SecList]}],{l,1,Length[varlist]}]}
{Which[Length[varlist]<=3,(RatKMSE=Table[Which[0.75<=(CornKMLSE[i,l][[1]]/StartKMLSE[i,l][[1]])<=1.25,{1,1,1,1},AllTrue[CornKMLSE[i,l],#>2&]||AllTrue[CornKMLSE[i,l],#<0.5&],{8,4,2,1},AllTrue[CornKMLSE[i,l],1.5<=#<=2&]||AllTrue[CornKMLSE[i,l],0.5<=#<=0.75&],{4,3,2,1},AllTrue[StartKMLSE[i,l],#>2&]||AllTrue[StartKMLSE[i,l],#<0.5&],{1,2,4,8},AllTrue[StartKMLSE[i,l],1.5<=#<=2&]||AllTrue[StartKMLSE[i,l],0.5<=#<=0.75&],{1,2,3,4},AllTrue[MePermKMDLSE[i,l],1<=#<=1.5&]||AllTrue[MePermKMDLSE[i,l],0.75<=#<=1&],{1,1,1,1},AllTrue[MePermKMDLSE[i,l],1.5<=#&]||AllTrue[MePermKMDLSE[i,l],#<=0.75&],{4,3,2,1}],{i,1,Length[SecList]},{l,1,Length[varlist]}]),Length[varlist]>3,RatKMSE=Table[Which[0.75<=(CornKMLSE[i,l][[1]]/StartKMLSE[i,l][[1]])<=1.25,ConstantArray[1,4+2*(Length[varlist]-2)],AllTrue[CornKMLSE[i,l],#>2&]||AllTrue[CornKMLSE[i,l],#<0.5&],ConstantArray[1,4+2*(Length[varlist]-2)+2],AllTrue[CornKMLSE[i,l],1.5<=#<=2&]||AllTrue[CornKMLSE[i,l],0.5<=#<=0.75&],ConstantArray[1,4+2*(Length[varlist]-2)+1],AllTrue[StartKMLSE[i,l],#>2&]||AllTrue[StartKMLSE[i,l],#<0.5&],ConstantArray[1,4+2*(Length[varlist]-2)+2],AllTrue[StartKMLSE[i,l],1.5<=#<=2&]||AllTrue[StartKMLSE[i,l],0.5<=#<=0.75&],ConstantArray[1,4+2*(Length[varlist]-2)+1],AllTrue[MePermKMDLSE[i,l],1<=#<=1.5&]||AllTrue[MePermKMDLSE[i,l],0.75<=#<=1&],ConstantArray[1,4+2*(Length[varlist]-2)],AllTrue[MePermKMDLSE[i,l],1.5<=#&]||AllTrue[MePermKMDLSE[i,l],#<=0.75&],ConstantArray[1,4+2*(Length[varlist]-2)+1]],{i,1,1},{l,1,Length[varlist]}]]}
(*for the partial contours*)
{Table[If[Length[GoodListSecKine[[i]]]<Round[Length[KineList]/2],Which[Length[varlist]<=3,(pRatKMSE=Which[0.75<=(pCornKMLSE[i,l][[1]]/pStartKMLSE[i,l][[1]])<=1.25,{1,1,1,1},AllTrue[pCornKMLSE[i,l],#>2&]||AllTrue[pCornKMLSE[i,l],#<0.5&],{8,4,2,1},AllTrue[pCornKMLSE[i,l],1.5<=#<=2&]||AllTrue[pCornKMLSE[i,l],0.5<=#<=0.75&],{4,3,2,1},AllTrue[pStartKMLSE[i,l],#>2&]||AllTrue[pStartKMLSE[i,l],#<0.5&],{1,2,4,8},AllTrue[pStartKMLSE[i,l],1.5<=#<=2&]||AllTrue[pStartKMLSE[i,l],0.5<=#<=0.75&],{1,2,3,4},AllTrue[pMePermKMDLSE[i,l],1<=#<=1.5&]||AllTrue[pMePermKMDLSE[i,l],0.75<=#<=1&],{1,1,1,1},AllTrue[pMePermKMDLSE[i,l],1.5<=#&]||AllTrue[pMePermKMDLSE[i,l],#<=0.75&],{4,3,2,1}]),Length[varlist]>3,RatKMSE=Which[0.75<=(pCornKMLSE[i,l][[1]]/pStartKMLSE[i,l][[1]])<=1.25,ConstantArray[1,4+2*(Length[varlist]-2)],AllTrue[pCornKMLSE[i,l],#>2&]||AllTrue[pCornKMLSE[i,l],#<0.5&],ConstantArray[1,4+2*(Length[varlist]-2)+2],AllTrue[pCornKMLSE[i,l],1.5<=#<=2&]||AllTrue[pCornKMLSE[i,l],0.5<=#<=0.75&],ConstantArray[1,4+2*(Length[varlist]-2)+1],AllTrue[pStartKMLSE[i,l],#>2&]||AllTrue[pStartKMLSE[i,l],#<0.5&],ConstantArray[1,4+2*(Length[varlist]-2)+2],AllTrue[pStartKMLSE[i,l],1.5<=#<=2&]||AllTrue[pStartKMLSE[i,l],0.5<=#<=0.75&],ConstantArray[1,4+2*(Length[varlist]-2)+1],AllTrue[pMePermKMDLSE[i,l],1<=#<=1.5&]||AllTrue[pMePermKMDLSE[i,l],0.75<=#<=1&],ConstantArray[1,4+2*(Length[varlist]-2)],AllTrue[pMePermKMDLSE[i,l],1.5<=#&]||AllTrue[pMePermKMDLSE[i,l],#<=0.75&],ConstantArray[1,4+2*(Length[varlist]-2)+1]]]],{i,1,Length[SecList]},{l,1,Length[varlist]}]}
(*now we store the setup variables that we need to reuse*)
SetDirectory["D6"]
{Put[CAdomsec[#],ToString[name]<>ToString[epsord]<>"CAdomsec"<>ToString[#]<>".txt"]&/@Range[1,Length[SecList]]}
Put[varlist,ToString[name]<>ToString[epsord]<>"varlist.txt"]
Put[SecList,ToString[name]<>ToString[epsord]<>"SecList.txt"]
Put[CAdomsecSub,ToString[name]<>ToString[epsord]<>"CAdomsecSub.txt"]
{Put[PartSubFinal[#],ToString[name]<>ToString[epsord]<>"PartSubFinal"<>ToString[#]<>".txt"]&/@Range[1,Length[SecList]]}
{Put[SubVarFinalTab,ToString[name]<>ToString[epsord]<>"SubVarFinalTab.txt"]}
{Put[CCFinal[#],ToString[name]<>ToString[epsord]<>"CCFinal"<>ToString[#]<>".txt"]&/@Range[1,Length[SecList]]}
{Put[CCNIELcalc,ToString[name]<>ToString[epsord]<>"CCNIELcalc.txt"]}
{Put[RatKMSE,ToString[name]<>ToString[epsord]<>"RatKMSE.txt"]}
{Put[CCNIELpartialcalc,ToString[name]<>ToString[epsord]<>"CCNIELpartialcalc.txt"]}
{Put[PartContFinal[#],ToString[name]<>ToString[epsord]<>"PartContFinal"<>ToString[#]<>".txt"]&/@Range[1,Length[SecList]]}
{Put[kinenames,ToString[name]<>ToString[epsord]<>"kinenames.txt"]}
{Put[ResKine,ToString[name]<>ToString[epsord]<>"ResKine.txt"]}
{Put[KineList,ToString[name]<>ToString[epsord]<>"KineList.txt"]}
{Put[GoodListSecKine,ToString[name]<>ToString[epsord]<>"GoodListSecKine.txt"]}
Quit
