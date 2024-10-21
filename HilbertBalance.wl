(* ::Package:: *)

BeginPackage["HilbertBalance`"]

Unprotect@@Names["HilbertBalance`*"];
ClearAll@@Names["HilbertBalance`*"];


(*
Author: Zhang Zeying
Email: zhangzeyingvv@gmail.com
*)


(* ::Subsection:: *)
(*Usage*)


extractChem ::usage = "Init the program"
toChem ::usage = "Get the unsymmetried Hamiltonian"

HilbertBasis::usage = "Get the symmetried Hamiltonian in convenition I"
balanceMatrix ::usage = "Get the unsymmetried Hamiltonian"
NormalizPath ::usage = "Full name of normaliz.exe, e.g. E:\\Mathematica\\BalanceBand\\normaliz-3.9.4\\normaliz.exe"
HilbertBalance ::usage = "HilbertBalance[str], str is the balanced  chemical equation, e.g. HilbertBalance[\"C+O2->C+O2+CO+CO2->CO\"]"


Begin["`Private`"]


(* ::Subsection:: *)
(*Basic Utilities*)


extractChem[chem_] := 
 If[#[[2]] == "", {#[[1]], 1}, {#[[1]], 
     ToExpression[#[[2]]]}] & /@ (First@
      StringCases[#, 
       RegularExpression["([A-Z][a-z]?)(\\d*)"] -> {"$1", "$2"}] & /@ 
    StringCases[chem, RegularExpression["[A-Z][a-z]?\\d*"]]);
Options[toChem]={
  "Format"->"Mathematica"
  };
toChem[Eles_,OptionsPattern[]]:=Module[{textC,textM,textS,format=OptionValue["Format"]},
      textC = StringJoin[
    "\\text{" <> #[[1]] <> "}" <> 
       If[#[[2]] == 1, "", 
        "_{" <> ToString[#[[2]]] <> "}"] & /@ #] &;
      textM = StringJoin[If[#[[2]] == 1, #[[1]], \!\(\*
TagBox[
StyleBox[
RowBox[{"\n", "      ", 
RowBox[{"\"\<\\!\\(\\*SubscriptBox[\\(\>\"", "<>", 
RowBox[{"ToString", "[", 
RowBox[{"Part", "[", 
RowBox[{"#", ",", "1"}], "]"}], "]"}], "<>", "\"\<\\), \\(\>\"", "<>", 
RowBox[{"ToString", "[", 
RowBox[{"Part", "[", 
RowBox[{"#", ",", "2"}], "]"}], "]"}], "<>", "\"\<\\)]\\)\>\""}]}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\)] & /@ #] &;
      textS = StringJoin[If[#[[2]] == 1, #[[1]], \!\(\*
TagBox[
StyleBox[
RowBox[{"\n", "      ", 
RowBox[{
RowBox[{"ToString", "[", 
RowBox[{"Part", "[", 
RowBox[{"#", ",", "1"}], "]"}], "]"}], "<>", 
RowBox[{"ToString", "[", 
RowBox[{"Part", "[", 
RowBox[{"#", ",", "2"}], "]"}], "]"}]}]}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\)] & /@ #] &;
      Which[
      format=="String",textS@Eles,
      format=="TeX",textC@Eles,
      format=="Mathematica",textM@Eles,
      True,textS@Eles
      ]
];



Options[toChem2]={
  "Format"->"Mathematica"
  };
toChem2[Ass_,OptionsPattern[]]:=Module[{str,format=OptionValue["Format"]},
str="";
Do[
If[k=="Charge",
Which[Ass[k]>0,
str=str<>"\!\(\*SuperscriptBox[\(\), \(+"<>ToString[Ass[k]]<>"\)]\)",
Ass[k]<0,
str=str<>"\!\(\*SuperscriptBox[\(\), \("<>ToString[Ass[k]]<>"\)]\)"]
,
Which[Ass[k]==1,str=str<>k,
Ass[k]>1,str=str<>"\!\(\*SubscriptBox[\("<>k<>"\), \("<>ToString[Ass[k]]<>"\)]\)"
]]
,{k,Keys[Ass]}];
str
]


Options[toChemArrow]={
  "Format"->"Mathematica"
  };
toChemArrow[Compounds_,OptionsPattern[]] := Module[
    {part,compounds,format=OptionValue["Format"],k,str},
    part=Length[Compounds];
(*    compounds=Values[Keys/@Compounds];
    compounds=Map[toChem[extractChem[#],"Format"->format]&,compounds,{-1}];*)
    compounds=Values/@Values@Map[toChem2,Compounds,{2}];
     Which[
     format=="String",StringRiffle[Map[StringRiffle[#,"+"]&,compounds,{1}],"->"],
     format=="Mathematica",RightArrow@@Map[StringRiffle[#,"+"]&,compounds,{1}],
     format=="TeX",StringRiffle[Map[StringRiffle[#,"+"]&,compounds,{1}],"\\rightarrow"],
     True,RightArrow@@Map[StringRiffle[#,"+"]&,compounds,{1}]]

   ];
Options[toChemEqu]={
  "Format"->"Mathematica"
  };
toChemEqu[basis_, Compounds_,OptionsPattern[]] := Module[
    {part,compounds,format=OptionValue["Format"],k,str},
    part=Length[Compounds];
(*    compounds=Values[Keys/@Compounds];
    compounds=Map[toChem[extractChem[#],"Format"->format]&,compounds,{-1}];*)
    compounds=Values/@Values@Map[toChem2,Compounds,{2}];
    k=0;
    str=Table[
      Table[
      k+=1;
      {j,basis[[k]]}
      ,{j,i}]
    ,{i,compounds}];
    str=Map[Which[
       #[[2]] == 0, Nothing,
       #[[2]] == 1, #[[1]],
       True, ToString[#[[2]]] <> #[[1]]]&,str,{2}];
   
     str=Map[StringRiffle[#,"+"]&,str,{1}];
     
     Which[
    format=="TeX",StringRiffle[str,"&="],
    True,StringRiffle[str,"="]
    ]
   

   ];



(* ::Subsection:: *)
(*Normaliz IO*)


NormalizPath= "Z:\\path\\to\\normaliz.exe";
NormalizPath= "E:\\Mathematica\\BalanceBand\\normaliz-3.9.4\\normaliz.exe";
readNout[file_] := Module[
   {filestr, filestrs, nHilb, Hilb, nHilb2, pos, pos2, polytope, high,
     res},
   
   nHilb2 = 0;
   (*polytope=0;*)
   pos2 = None;
   filestr = Import[file];
   nHilb = 
    ToExpression@
     First@StringCases[filestr, 
       RegularExpression["(\\d+)\\sHilbert\\sbasis\\selements\\n"] -> 
        "$1"];
   polytope = nHilb;
   filestrs = StringSplit[filestr, "\n"];
   pos = 
    First@FirstPosition[filestrs, 
      ToString@nHilb <> " Hilbert basis elements:"];
   If[pos == "NotFound",
    polytope = 
     ToExpression@
      First@StringCases[filestr, 
        RegularExpression[
          "(\\d+)\\slattice\\spoints\\sin\\spolytope\\s\\(Hilbert\\\
sbasis\\selements\\sof\\sdegree\\s1\\)"] -> "$1"];
    pos = 
     First@FirstPosition[filestrs, 
       ToString@polytope <> 
        " lattice points in polytope (Hilbert basis elements of \
degree 1):"];];
   res = filestrs[[pos + 1 ;; pos + nHilb]];
   (*Print[pos];*)
   If[nHilb != polytope, Print["2D Hilb", file];
    nHilb2 = 
     ToExpression@
      First@StringCases[filestr, 
        RegularExpression[
          "(\\d+)\\sfurther\\sHilbert\\sbasis\\selements\\sof\\\
shigher\\sdegree"] -> "$1"];
    pos2 = 
     First@FirstPosition[filestrs, 
       ToString@nHilb2 <> 
        " further Hilbert basis elements of higher degree:"];
    res = 
     filestrs[[pos + 1 ;; pos + polytope]]~Join~
      filestrs[[pos2 + 1 ;; pos2 + nHilb2]];
    ];
   
   Association["N" -> nHilb, 
    "Hilb" -> ToExpression[StringSplit /@ res]]
   ];

tonormaliz[mat_] := "amb_space " <> ToString[Length[mat]] <>
     "\nconstraints " <> ToString[Length[mat[[1]]]] <> "\n" <>
     StringRiffle[#, "\n"] &@(StringRiffle[#, {"", " ", " >= 0"}] & /@
      Transpose[mat]);
toconenormaliz[mat_] := "amb_space " <> ToString[Length[mat[[1]]]] <>
     "\ncone " <> ToString[Length[mat(*[[1]]*)]] <> "\n" <>
     StringRiffle[#, 
      "\n"] &@(StringRiffle[#, {"", " ", ""}] & /@(*Transpose@*)mat);
      
HilbertBasis[Null_]:=Module[{Hilb,Hilb2,res,sign,OS},
  OS=StringTake[$OperatingSystem,3];
  If[Null=={},Return[<|"N"->0,"Hilb"->{}|>]];
  (*If[Length[Null]==1,
  If[Xnor@@(Sign/@First[Null]/.{1->True,-1->False,0->Nothing}),
  Return[<|"N"->1,"Hilb"->(Sign/@First[Null]/.{0->Nothing})[[1]] Null|>],
  Return[<|"N"->0,"Hilb"->{}|>]
  ]
  ];*)
  
  
  Export[FileNameJoin[{$TemporaryDirectory,"5.in"}],tonormaliz[Null],"Text"];
  sign=Which[
  OS=="Win",RunProcess[{"cmd","/c",NormalizPath<>" "<>FileNameJoin[{$TemporaryDirectory,"5.in"}]<>"/t"}],
  OS=="Mac",RunProcess[{NormalizPath,$TemporaryDirectory<>"/5.in"}],
  True, RunProcess[{NormalizPath,$TemporaryDirectory<>"/5.in"}]
  ];
  If[sign["ExitCode"]!=0,Print["Error! Please set the correct normaliz path.",sign,NormalizPath];Abort[]];
  Hilb=readNout[FileNameJoin[{$TemporaryDirectory,"5.out"}]];
  If[Hilb["Hilb"]=={},Return[<|"N"->0,"Hilb"->{}|>]];
  (*Print[Hilb];*)
  res=Hilb["Hilb"] . Null;
  Export[FileNameJoin[{$TemporaryDirectory,"6.in"}],toconenormaliz[res],"Text"];
  
  sign=Which[
  OS=="Win",RunProcess[{"cmd","/c",NormalizPath<>" "<>FileNameJoin[{$TemporaryDirectory,"6.in"}]<>"/t"}],
  OS=="Mac",RunProcess[{NormalizPath,$TemporaryDirectory<>"/6.in"}],
  True,RunProcess[{NormalizPath,$TemporaryDirectory<>"/6.in"}]
  ];
  If[sign["ExitCode"]!=0,Print["Error! Something is wrong in normaliz package."];Abort[]];
  Hilb2=readNout[FileNameJoin[{$TemporaryDirectory,"6.out"}]]
  
  ]


(* ::Subsection:: *)
(*Balance Chem*)


Options[balanceMatrix]={
  "Charge"->None
  };

balanceMatrix[chem_, OptionsPattern[]]:=Module[
    {parts,compounds,elements,balenceM,occ,npart,null,hilb,i,l,s,t,ll,res,
    Charge=OptionValue["Charge"],firstc
    },
    parts=StringSplit[chem,"->"];
    (*Print[parts];*)
    elements=Association[];
    compounds=Association[];
    (*parts=Partition[parts,2,1];*)
    Do[
    compounds[i]=extractChem/@StringSplit[parts[[i]],"+"];
    
    elements[i]=Union[Flatten[compounds[i],1][[;;,1]]];
    compounds[i]=Association[toChem[#,"Format"->"String"] -> 
    Association[(#1 -> #2 & @@@ #)] & /@ compounds[i]];
    
    ,{i,Length[parts]}];
    (*Print[elements];*)
    If[ListQ[Charge],
    elements=#~Join~{"Charge"}&/@elements;
    i=1;
      Do[
      Do[
      compounds[n][c]["Charge"]=Charge[[i]];
      i+=1;
      ,{c,Keys[compounds[n]]}]
      ,{n,Keys[compounds]}]
      ];
    
    If[Not@(Equal@@Values[elements]),Return["Can not be balanced"]];
    elements=elements[1];
    balenceM=Table[0,{i,(Length[parts]-1)Length[elements]},{j,Total[Length/@compounds]}];
    (*Print[MatrixForm[balenceM]];*)
    npart=Length[parts]-1;
    (*Print[elements];Print@compounds;*)
    Do[
      Do[
      l=0;
        Do[
        ll=Accumulate@(Length/@Values@compounds);
        If[i!=0,l=ll[[k-1]]];
          Do[
          l+=1;
            occ=compounds[k][s][elements[[j]]];
            If[MissingQ[occ],occ=0];
            If[k==i+2,occ=-occ];
            balenceM[[i Length[elements] +j,l]]=occ
             ,{s,Keys@compounds[k]}]
          ,{k,{i+1,i+2}}]
      ,{j,Length[elements]}]
    ,{i,0,npart-1}];
    (*Print[compounds,elements];*)
    
(*    If[ListQ[Charge],
      If[Length[Length/@Values@compounds]==2, 
      firstc=Length[Values@compounds[[1]]];
      Table[Charge[[i]]=-Charge[[i]],{i,firstc}];
      Charge=-Charge
      ];
    balenceM= Append[balenceM,Charge]];*)
    null=NullSpace[balenceM];
    hilb=HilbertBasis[null];
    
    res=Association["M"->balenceM,"Elements"->elements,"Compounds"->compounds,
        "TableHead"->{Flatten@Values[Keys/@compounds],Flatten@Table[elements,npart]},
        "Null"->null,"HilbertBasis"->hilb]
    
]


Options[HilbertBalance]={
  "Format"->"Mathematica",
  "Charge"->None
  };

HilbertBalance[str_,OptionsPattern[]]:=Module[
    {HilbertC,compounds,hilb,format=OptionValue["Format"],
    Charge=OptionValue["Charge"]
    },
    HilbertC=balanceMatrix[str,"Charge"->Charge];
    compounds=HilbertC["Compounds"];
    hilb=HilbertC["HilbertBasis"];
    Print[toChemArrow[compounds,"Format"->format]];
    (*Print[HilbertC];*)
    If[hilb["Hilb"]=={},Print@"Can not be balanced.";Return[]];
    Which[
    format=="TeX",Print@StringRiffle[toChemEqu[#,compounds,"Format"->format]&/@hilb["Hilb"],"\\\\\n"],
    True,Print@StringRiffle[toChemEqu[#,compounds,"Format"->format]&/@hilb["Hilb"],"\n"]
    ]
    ]


(* ::Subsection:: *)
(*Graph*)


toGraphInput[bal_, balenceMatrix_] := 
 Module[{ele, Compounds, nCompounds, left, right, comp,
   label, lr
   },
  ele = balenceMatrix["Eel"];
  Compounds = Keys /@ Values@balenceMatrix["Compounds"];
  comp = Map[extractChem, Compounds, {-1}];
  {left, right} = Length /@ Compounds;
  nCompounds = left + right;
  comp = Map[Table[#[[1]], #[[2]]] &, comp, {-2}];
  comp = Flatten[Map[Flatten, comp, {-3}], 1];
  comp = {#[[;; left]], #[[left + 1 ;;]]} &@Table[
     Table[comp[[i]], bal[[i]]]
     ,
     {i, Length[bal]}];
  comp = Flatten[#, 1] & /@ comp;
  
  label = Thread[Range[Length[#]] -> #] &@Flatten[Table[
      Table[Flatten[Compounds][[i]], bal[[i]]]
      ,
      {i, Length[bal]}]];
  {left, right} = Length /@ comp;
  {comp, label, {left, right}}
  ];





 End[] 
EndPackage[]
