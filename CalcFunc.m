(* ::Package:: *)

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
