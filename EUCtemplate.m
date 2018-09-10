(* ::Package:: *)

(* ::Input:: *)
(*(*I59 EUC TEMPLATE*)*)


(* ::Input:: *)
(*getSectors[name_,i_]:=Block[{functiontooptimize,SimplifiedSectorList,q},*)
(*<<(ToString[name]<>ToString[i]<>".m");*)
(*SimplifiedSectorList=Rationalize[Simplify[functiontooptimize,TimeConstraint->3]];*)
(*Return[SimplifiedSectorList]]*)


(* ::Input:: *)
(*(*let us first input the iterated sectors*)*)


(* ::Input:: *)
(*SetDirectory["/home/ttp/dhulme/SecDec-3.0.9/loop/tridom1correct/I59/I59_sEUC/numerics/together/epstothe0/"]*)


(* ::Input:: *)
(*I59S=getSectors[f,#]&/@Range[1,22]/.{em[0]->ms,esx[0]->q12s,esx[1]->q23s,esx[2]->q123s};*)


(* ::Input:: *)
(*(*user input*)*)


(* ::Input:: *)
(*(*variables*)*)


(* ::Input:: *)
(*varlistI59=List[x0,x1,x2,x3,x4]*)


(* ::Input:: *)
(*(*kinematics*)*)


(* ::Input:: *)
(*(*points*)*)


(* ::Input:: *)
(*kinenamesI59={q12s,q23s,q123s,ms}*)


(* ::Input:: *)
(*(*we then generate the list of mappings*)*)


(* ::Input:: *)
(*mapI59[x_]:=(-1-x)/x*)


(* ::Input:: *)
(*maplistI59=mapI59@varlistI59*)


(* ::Input:: *)
(*varspec1maplistI59=mapI59@varlistI59/.{maplistI59[[Length[varlistI59]]]->varlistI59[[Length[varlistI59]]]}*)


(* ::Input:: *)
(*secvarspec1I59=Join[ConstantArray[varspec1maplistI59,1],ConstantArray[maplistI59,21]]*)


(* ::Input:: *)
(*SetDirectory["/disk/data11/ttp/dhulme/FORM_Taylor/I59/EUC/FirstPoint/"]*)


(* ::Input:: *)
(*(*integrated output*)*)


(* ::Input:: *)
(*(*with a plain mapping*)*)


(* ::Input:: *)
(*(*with a sector specific mapping*)*)


(* ::Input:: *)
(*FlexBTTaylorVarIndIntNewNumFlexConfTbTCoArbExp[I59S[[#]],varlistI59,secvarspec1I59[[#]],{0,0,0,0,0},{1,1,1,1,1},0,6,"I59mapsecvarspec1INTEPS0",#]&/@Range[1,Length[I59S]]*)


(* ::Input:: *)
(*(*SDtakelist*)*)


(* ::Input:: *)
(*SDtakelist={1,11,21,31,41,51,61,71,81,91,101};*)


(* ::Input:: *)
(*(*now we try a full range of values*)*)


(* ::Input:: *)
(*EUCsvalI59=Table[{i,-2*29929,-2*29929,29929},{i,0,-10*29929,-0.1*29929}];*)


(* ::Input:: *)
(*EUCerrsvalI59=Table[{i,-2*29929,-2*29929,29929},{i,0,-10*29929,-1*29929}];*)


(* ::Input:: *)
(*EUCuvalI59=Table[{-2*29929,-2*29929,i,29929},{i,0,-10*29929,-0.1*29929}];*)


(* ::Input:: *)
(*EUCmhvalI59=Table[{-2*29929,i,-2*29929,29929},{i,0,-10*29929,-0.1*29929}];*)


(* ::Input:: *)
(*(*output to SecDec*)*)


(* ::Input:: *)
(*SDEUCsvalI59=Prepend[EUCsvalI59[[#]],p[#]]&/@Range[1,Length[EUCsvalI59]];*)


(* ::Input:: *)
(*SDEUCuvalI59=Prepend[EUCuvalI59[[#]],p[#]]&/@Range[1,Length[EUCuvalI59]];*)


(* ::Input:: *)
(*SDEUCmhvalI59=Prepend[EUCmhvalI59[[#]],p[#]]&/@Range[1,Length[EUCmhvalI59]];*)


(* ::Input:: *)
(*SetDirectory["/home/ttp/dhulme/SecDec-3.0.9/loop/tridom1correct/I59/"]*)


(* ::Input:: *)
(*Export["kinem_sEUC_I59.csv",SDEUCsvalI59]*)


(* ::Input:: *)
(*Export["kinem_uEUC_I59.csv",SDEUCuvalI59]*)


(* ::Input:: *)
(*Export["kinem_mhEUC_I59.csv",SDEUCmhvalI59]*)


(* ::Input:: *)
(*(*calculate results*)*)


(* ::Input:: *)
(*SetDirectory["/disk/data11/ttp/dhulme/FORM_Taylor/I59/EUC/FirstPoint/"]*)


(* ::Input:: *)
(*(*MISTAKE - the prefactor for I59 is actually 1 NOT -1*)*)


(* ::Input:: *)
(*(*order 8*)*)


(* ::Input:: *)
(*I59EUCE0skineres=FlexBTTaylorVarIndIntNewNumFlexConfTbTCoArbExpIN[-1,varlistI59,kinenamesI59,EUCsvalI59[[#]],"I59mapsecvarspec1INTEPS0","I59mapsecvarspec1INTEPS0sKINE"<>ToString[#],0,8,1,22]&/@Range[1,Length[EUCsvalI59]]*)


(* ::Input:: *)
(*(*the TayInt errors*)*)


(* ::Input:: *)
(*I59EUCerrE0skineres=FlexBTTaylorVarIndIntNewNumFlexConfTbTCoArbExpIN[-1,varlistI59,kinenamesI59,EUCsvalI59[[#]],"I59mapsecvarspec1INTEPS0","I59mapsecvarspec1INTEPS0errsKINE"<>ToString[#],8,8,1,22]&/@Range[1,Length[EUCerrsvalI59]]*)


(* ::Input:: *)
(*(*the subset of my results that I want to compare against SecDec*)*)


(* ::Input:: *)
(*SDCOI59EUCE0skineres=-I59EUCE0skineres[[SDtakelist[[#]]]]&/@Range[1,Length[SDtakelist]]*)


(* ::Input:: *)
(*(*normalised TayInt errors*)*)


(* ::Input:: *)
(*I59EUCnerrE0skineres=Abs[I59EUCerrE0skineres]/Abs[SDCOI59EUCE0skineres]*)


(* ::Input:: *)
(*(*SecDec results*)*)


(* ::Input:: *)
(*SetDirectory["/home/ttp/dhulme/SecDec-3.0.9/loop/tridom1correct/I59/I59_sEUC/results/cut_res/"]*)


(* ::Input:: *)
(*SDI59EUCE0skineres=(<<("cut_eps0_p"<>ToString[SDtakelist[[#]]]))&/@Range[1,Length[SDtakelist]]*)


(* ::Input:: *)
(*(*SecDec errors*)*)


(* ::Input:: *)
(*SetDirectory["/home/ttp/dhulme/SecDec-3.0.9/loop/tridom1correct/I59/I59_sEUC/results/cut_err/"]*)


(* ::Input:: *)
(*SDI59EUCE0skinerr=(<<("err_fullcut_eps0_p"<>ToString[SDtakelist[[#]]]))&/@Range[1,Length[SDtakelist]]*)


(* ::Input:: *)
(*(*normalised SecDec errors*)*)


(* ::Input:: *)
(*SDI59EUCE0skinnerr=SDI59EUCE0skinerr/SDI59EUCE0skineres*)


(* ::Input:: *)
(*(*Ratios*)*)


(* ::Input:: *)
(*RATI59EUCE0skineres=SDI59EUCE0skineres/SDCOI59EUCE0skineres*)


(* ::Input:: *)
(*(*output to GNU*)*)


(* ::Input:: *)
(*SetDirectory["/home/ttp/dhulme/GNUplot/I59/EUC/"]*)


(* ::Input:: *)
(*AccountingForm[I59EUCE0skineres[[1]],10]*)


(* ::Input:: *)
(*I59EUCE0TayIntDat={Chop[EUCsvalI59[[#]][[1]],2],AccountingForm[-I59EUCE0skineres[[#]],10][[1]]}&/@Range[1,101];*)


(* ::Input:: *)
(*I59EUCE0SecDecDat={N[EUCerrsvalI59[[#]][[1]],2],AccountingForm[SDI59EUCE0skineres[[#]],10][[1]],N[RATI59EUCE0skineres[[#]]],N[I59EUCnerrE0skineres[[#]]],N[SDI59EUCE0skinnerr[[#]]]}&/@Range[1,Length[EUCerrsvalI59]];*)


(* ::Input:: *)
(*Export["I59EUCE0TayIntDat.dat",I59EUCE0TayIntDat]*)


(* ::Input:: *)
(*Export["I59EUCE0SecDecDat.dat",I59EUCE0SecDecDat]*)
