{FlexBTTaylorVarIndIntNewNumFlexConfTbTCoArbExp[func_,varlist_?VectorQ,conflist_?VectorQ,intstartlist_?VectorQ,intstoplist_?VectorQ,start_,ord_,name_,num_]:=Block[{Jac,confFunc,Num,ExpFunc,TaylorFunc,IntFunc,TaylorResult,alpha,sum,powlist,powvarlist,intlimlist,e,il,ih,pow,sumindexlist,sumlimlist,k,derlist,intminlist,intmaxlist,varlist2,conflistInv,y,explist},
powlist=pow[#]&/@Range[1,Length[varlist]];
powvarlist=varlist[[#]]^(powlist[[#]])&/@Range[1,Length[varlist]];
varlist2=y[#]&/@Range[1,Length[varlist]];
conflistInv=Flatten[(varlist[[#]]/.Solve[conflist[[#]]==varlist2[[#]],varlist[[#]]])&/@Range[1,Length[varlist]]];
explist=(((conflistInv[[#]]/.{varlist2[[#]]->intstartlist[[#]]})+(conflistInv[[#]]/.{varlist2[[#]]->intstoplist[[#]]}))*1/2)&/@Range[1,Length[varlist]];
intminlist=((conflistInv[[#]]/.{varlist2[[#]]->intstartlist[[#]]})-explist[[#]])&/@Range[1,Length[varlist]];
intmaxlist=((conflistInv[[#]]/.{varlist2[[#]]->intstoplist[[#]]})-explist[[#]])&/@Range[1,Length[varlist]];
intlimlist={varlist[[#]],intminlist[[#]],intmaxlist[[#]]}&/@Range[1,Length[varlist]];
sumindexlist=k[#]&/@Range[1,Length[varlist]];
sumlimlist=({sumindexlist[[#]],0,ord})&/@Range[1,Length[varlist]];
Jac=Times@@((Simplify[D[conflist[[#]],varlist[[#]]],TimeConstraint->3])&/@Range[1,Length[varlist]]);
confFunc=Simplify[Jac*(func/.Thread[varlist->conflist]),TimeConstraint->3];
{sum=(Flatten[Table[ReplaceRepeated[alpha->Sequence[]][If[start<=(Plus@@sumindexlist)<=ord,sumindexlist,alpha]],Evaluate[Sequence@@sumlimlist]],Length[varlist]-1]);}
Table[derlist[i]=({varlist[[#]],sum[[i]][[#]]})&/@Range[1,Length[varlist]],{i,1,Length[sum]}];
(TaylorFunc[#]=1/(Times@@(sum[[#]]!)) (D[confFunc,Evaluate[Sequence@@derlist[#]]]/.Thread[varlist->explist]))&/@Range[1,Length[sum]];
IntFunc=Integrate[Times@@@((powvarlist/.Thread[powlist->sum[[#]]])&/@Range[1,Length[sum]]),Evaluate[Sequence@@intlimlist]];
(TaylorResult[#]=((TaylorFunc[#]*IntFunc[[#]])))&/@Range[1,Length[sum]];
(Put[Compress[TaylorResult[#]],(ToString[num]<>"TAYINTCO"<>ToString[name]<>"["<>ToString[sum[[#]]]<>"].h")])&/@Range[1,Length[sum]];]}
{FlexBTTaylorVarIndIntNewNumFlexConfTbTCoArbExpIN[pre_,varlist_?VectorQ,knamelist_?VectorQ,kvallist_?VectorQ,namein_,nameout_,start_,ord_,secstart_,secnum_]:=Block[{sum,ResultListPre,ResultList,alpha,intminlist,intmaxlist,sumindexlist,sumlimlist,k},
sumindexlist=k[#]&/@Range[1,Length[varlist]];
sumlimlist=({sumindexlist[[#]],0,ord})&/@Range[1,Length[varlist]];
{sum=(Flatten[Table[ReplaceRepeated[alpha->Sequence[]][If[start<=(Plus@@sumindexlist)<=ord,sumindexlist,alpha]],Evaluate[Sequence@@sumlimlist]],Length[varlist]-1]);}
Put[(pre)*Sum[((Uncompress[(Import[(ToString[i]<>"TAYINTCO"<>ToString[namein]<>"["<>ToString[sum[[#]]]<>"].h")])])/.Thread[knamelist->kvallist]),{i,secstart,secnum}],("KINETAYINTCO"<>ToString[nameout]<>"["<>ToString[sum[[#]]]<>"].h")]&/@Range[1,Length[sum]];
Put[Total[Import[(("KINETAYINTCO"<>ToString[nameout]<>"["<>ToString[sum[[#]]]<>"].h"))]&/@Range[1,Length[sum]]],("TOTKINETAYINTCO"<>ToString[nameout]<>".txt")];
ResultList=Import[("TOTKINETAYINTCO"<>ToString[nameout]<>".txt")];
Return[ResultList]]}
Clear[PlainTay]
Clear[PlainTayKine]
Clear[varlist]
{SetDirectory["/home/ttp/dhulme/SecDec-3.0.9/loop/tridom1correct/I246/I246_EUC/TayIntAuto/"]}
<<CompressFlexVarIndbtTaylorCode.m
<<TayIntfunctions.m
{ItSecDir="/home/ttp/dhulme/SecDec-3.0.9/loop/tridom1correct/I246/I246_SEC/numerics/together/epstothe0/"}
{name=I246}
{kineinv={q23s,q123s,q12s}}
{kinemass={ms}}
{kinenames=Join[kineinv,kinemass]}
{Feynlist={x0,x1,x2,x3,x4}}
ThreshPosList=#^2&/@Range[0, 11]
Table[ThreshFacList[i]=#&/@Range[-1, 11,0.25],{i,1,Length[KineMassesVal]}]
KineScalesVal={-2*29929,0.5*29929}
{KineMassesVal={29929}}
{Table[ThreshList[i]=Join[{#*KineMassesVal[[i]]},KineScalesVal,KineMassesVal]&/@Range[-1, 11,0.25],{i,1,Length[KineMassesVal]}]}
{intlowlist=ConstantArray[0,Length[Feynlist]]}
{inthighlist=ConstantArray[1,Length[Feynlist]]}
SetDirectory[ToString[ItSecDir]]
{SecDecInv=esx[#]&/@Range[0,Length[kineinv]-1]}
{SecDecMass=em[#]&/@Range[0,Length[kinemass]-1]}
{SecDecList=Join[SecDecInv,SecDecMass]}
{SecList=getSectors[f,#]&/@Range[1,36]/.Thread[SecDecList->kinenames]}
{SetDirectory["/home/ttp/dhulme/SecDec-3.0.9/loop/tridom1correct/I246/I246_SEC/TayIntAuto/Thresholds/Calc/"]}
{Evaluate[FlexBTTaylorVarIndIntNewNumFlexConfTbTCoArbExp[SecList[[#]],Feynlist,Feynlist,intlowlist,inthighlist,0,0,ToString[name]<>"thresh",#]]&/@Range[1,Length[SecList]]}
{Evaluate[FlexBTTaylorVarIndIntNewNumFlexConfTbTCoArbExp[SecList[[#]],Feynlist,Feynlist,intlowlist,inthighlist,2,2,ToString[name]<>"thresh",#]]&/@Range[1,Length[SecList]]}
{Table[TayList2[i]=PlainTayKine[Feynlist,kinenames,ThreshList[i][[#]],ToString[name]<>"thresh",ToString[name]<>"threshkin",2,2,1,Length[SecList]]&/@Range[1,Length[ThreshList[i]]],{i,1,Length[KineMassesVal]}]}
{Table[TayList0[i]=PlainTayKine[Feynlist,kinenames,ThreshList[i][[#]],ToString[name]<>"thresh",ToString[name]<>"threshkin",0,0,1,Length[SecList]]&/@Range[1,Length[ThreshList[i]]],{i,1,Length[KineMassesVal]}]}
{Table[TayListRat[i]=TayList2[i]/TayList0[i],{i,1,Length[KineMassesVal]}]}
(*bunch search*)
{Table[BunchPos[i]=ReplaceRepeated[Null->Nothing][ReplaceRepeated[Indeterminate->10000000][If[TayListRat[i][[#]]>1&&TayListRat[i][[#]]>TayListRat[i][[#-1]]&&TayListRat[i][[#+1]]>TayListRat[i][[#-1]],Position[TayListRat[i],TayListRat[i][[#]]][[1]][[1]]]]]&/@Range[1,Length[TayListRat[i]]-1],{i,1,Length[KineMassesVal]}]}
{Table[SepBunch[i]=If[BunchPos[i][[#+1]]==(BunchPos[i][[#]]+1),BunchPos[i][[#]]]&/@Range[1,Length[BunchPos[i]]-1],{i,1,Length[KineMassesVal]}]}
{Table[NullPos[i]=Append[Prepend[Flatten[Position[SepBunch[i],Null]],1],Length[SepBunch[i]]],{i,1,Length[KineMassesVal]}]}
{Table[ThreshBunches[i]=ReplaceRepeated[{}->Nothing][ReplaceRepeated[Null->Nothing][Take[SepBunch[i],{NullPos[i][[#]],NullPos[i][[#+1]]}]]]&/@Range[1,Length[NullPos[i]]-1],{i,1,Length[KineMassesVal]}]}
{Table[ThreshFac[i]=Part[ThreshFacList[i],ThreshBunches[i][[#]]]&/@Range[1,Length[ThreshBunches[i]]],{i,1,Length[KineMassesVal]}]}
{Table[ThreshIntFac[i]=Flatten[Round[ThreshFac[i]]],{i,1,Length[KineMassesVal]}]}
{Table[ActThreshFac[i]=Intersection[ThreshIntFac[i],ThreshPosList],{i,1,Length[KineMassesVal]}]}
{Table[ActThresh[i]=N[ActThreshFac[i]*29929],{i,1,Length[KineMassesVal]}]}
