(* ::Package:: *)

Inverso::usage="Inverso[x,c] inverso de x en Zc";
Generatriz::usage="Generatriz[A,mod] Matriz Est\[AAcute]ndar de A con mod c";
Generadora::usage="Generadora[H,c] recibe una matriz de paridad y m\[OAcute]dulo, y devuelve una generadora";
GeneradoraEstandar::usage="GeneradoraEstandar[H,c] recibe una matriz de paridad y el m\[OAcute]dulo, y devuelve una generadora est\[AAcute]ndar";
Paridad::usage="Paridad[G,c] recibe generadora y m\[OAcute]dulo, y devuelve una Paridad";
Codigo::usage="Codigo[G,m] Recibe una matriz generadora y el m\[OAcute]dulo y devuelve el c\[OAcute]digo lineal";
Decodificar::usage="Decodificar[y,G,c] Recibe palabra y, generadora y m\[OAcute]dulo y devuelve una palabra-c\[OAcute]digo como decodificaci\[OAcute]n (pesos) y True si es \[UAcute]nica o False si no lo es";
Decodificar2::usage="Decodificar2[y,G,c] Recibe palabra y, generadora y m\[OAcute]dulo y devuelve una palabra-c\[OAcute]digo como decodificaci\[OAcute]n (dist H) y True si es \[UAcute]nica o False si no lo es";
DistanciaMinima::usage="DistanciaMinima[G,c] Recibe generadora y m\[OAcute]dulo y devuelve la distancia m\[IAcute]nima de C";
DecodificarGrobnerBinario::usage="DecodificarGrobnerBinario[y_,G_] recibe una palabra 'y' y una matriz generadora del c\[OAcute]digo y decodifica 'y' con base de Gr\[ODoubleDot]bner";
DecodificacionCompleta::usage="DecodificacionCompleta[y_,G_,mod_] recibe una palabra 'y', la matriz generadora del c\[OAcute]digo y su m\[OAcute]dulo y devuelve la palabra originalmnte enviada, D(y)";


Begin["variableslocales`"];


Inverso[x_,c_]:=Module[{i},
If[Length[FactorInteger[c]]!=1 || FactorInteger[c][[1,2]]!=1,Print["EL N\[CapitalUAcute]MERO " ,c," NO es primo" ];
];
If[x==0,Return[x]];
For[i=1,i<c,i++,
If[Mod[x*i,c]==1, Return[i]]
];
];
EncuentraNumero1[A_,col_]:=Module[{i}, (* devuelve la fila donde est\[AAcute] o cero si no hay*)

For[i=col,i<Length[A]+1,i++,
If[A[[i,col]]!=0,Return[i]];
];
Return[0];
];
EncuentraNumero2[A_,col_]:=Module[{i}, (* devuelve la fila donde est\[AAcute] o cero si no hay*)

For[i=col,i<Length[A]+1,i++,
If[A[[i,Length[A[[1]]]-Length[A]+col]]!=0,Return[i]];
];
Return[0];
];
CambiaFila[A_,fila1_,fila2_]:=Module[{B},
B=A;
B[[fila1]]=A[[fila2]];
B[[fila2]]=A[[fila1]];
Return[B];
];
HacerCerosColumna1[A_,col_]:=Module[{B,i},
B=A;
For[i=col+1,i<Length[A]+1,i++,
B[[i]]=A[[i]]-A[[i,col]]*A[[col]];
];
For[i=col-1,i>0,i--,
B[[i]]=A[[i]]-A[[i,col]]*A[[col]];
];
Return[B];
];
HacerCerosColumna2[A_,col_]:=Module[{B,i},
B=A;
For[i=col+1,i<Length[A]+1,i++,
B[[i]]=A[[i]]-A[[i,Length[A[[1]]]-Length[A]+col]]*A[[col]];
];
For[i=col-1,i>0,i--,
B[[i]]=A[[i]]-A[[i,Length[A[[1]]]-Length[A]+col]]*A[[col]];
];
Return[B];
];

Generatriz[A_,mod_]:=Module[{B},
B=RowReduce[A,Modulus->mod];
Return[B];
];

Paridad[A_,c_]:=Module[{B,H,i,j},(*Recibe una matriz generadora de un c\[OAcute]digo y el m\[OAcute]dulo considerado y devuelve una matriz de paridad de dicho c\[OAcute]digo*)
B=Generatriz[A,c];
H=Table[0,{i,1,Length[A]},{j,1,Length[A[[1]]]-Length[A]}];
For[i=1,i<Length[A]+1,i++,
For[j=Length[A]+1,j<Length[A[[1]]]+1,j++,
H[[i,j-Length[A]]]=B[[i,j]];
]
];
H=-Transpose[H];
For[i=1,i<Length[A[[1]]]-Length[A]+1,i++,
H[[i]]=Join[H[[i]],IdentityMatrix[Length[A[[1]]]-Length[A]][[i]]]
];
Return[Mod[H,c]];
];
(* Generadora *)
Generadora[H_,m_]:=Module[{B},(*Recibe una matriz de paridad de un c\[OAcute]digo y el m\[OAcute]dulo considerado y devuelve una matriz generadora de dicho c\[OAcute]digo*)
B=NullSpace[H,Modulus->m];
Return[B];
];

(* GeneradoraEstandar *)

GeneradoraEstandar[H_,c_]:=Module[{B},(*Recibe una matriz de paridad de un c\[OAcute]digo y el m\[OAcute]dulo considerado y devuelve una matriz generadora est\[AAcute]ndar de dicho c\[OAcute]digo*)
B=Generadora[H,c];
B=Generatriz[B,c];
Return[B];
];

(* C\[OAcute]digo *)

Codigo[G_,mod_]:=Module[{cod,m,i},
m=Tuples[Range[0,mod-1],Length[G]];
cod={};
For[i=1,i<=Length[m],i++,cod=AppendTo[cod,Mod[m[[i]].G,mod]]];
Return[cod]
];

(* Distancia M\[IAcute]nima*)
DistanciaMinima[G_,mod_]:=Module[{cod,i,d},
cod=Codigo[G,mod];
d=Count[cod[[2]],0];
For[i=3,i<=Length[cod],i++,
If[Count[cod[[i]],0]>d,d=Count[cod[[i]],0]]];
Return[Length[G[[1]]]-d];
];

(*Decodificaci\[OAcute]n por peso*)
Decodificar[y1_,G_,mod_]:=Module[{coset,e,i,c,y,cod,r,unica},
y=Mod[y1,mod];
cod=Codigo[G,mod];
coset=Table[Mod[y+cod[[i]],mod],{i,1,Length[cod]}];
e=coset[[1]];
c=Count[coset[[1]],0];
For[i=2,i<=Length[coset],i++,
If[Count[coset[[i]],0]>c,{e=coset[[i]],c=Count[coset[[i]],0]}]];
r=Mod[y-e,mod];
If[HammingDistance[y,r]<= (DistanciaMinima[G,mod]-1)/2,unica=True,unica=False];
Return[{r,unica}];
];

(*Decodificaci\[OAcute]n por distancia m\[IAcute]nima*)
Decodificar2[y1_,G_,mod_]:=Module[{e,i,d,y,cod,unica},
y=Mod[y1,mod];
cod=Codigo[G,mod];
e=cod[[1]];
d=HammingDistance[y,e];
For[i=2,i<=Length[cod],i++,
If[HammingDistance[y,cod[[i]]]<d,{d=HammingDistance[y,cod[[i]]],e=cod[[i]]}]];
If[HammingDistance[y,e]<= (DistanciaMinima[G,mod]-1)/2,unica=True,unica=False];
Return[{e,unica}];
];

(* Decodificaci\[OAcute]n binario con base de Gr\[ODoubleDot]bner *)

DecodificarGrobnerBinario[y_,G_]:=Module[{yp=1,i,X,f=1,F={},j,l={}},
X=Table[Subscript[x, i],{i,1,Length[G[[1]]]}];
For[i=1,i<=Length[G[[1]]],i++,
yp=yp*Subscript[x, i]^y[[i]]];
For[i=1,i<=Length[G],i++,
For[j=1,j<=Length[G[[1]]],j++,
f=f* Subscript[x, j]^G[[i,j]];
If[j==Length[G[[1]]],{F=Append[F,f-1],f=1}];]];
For[i=1,i<=Length[G[[1]]],i++,
F=Append[F,Subscript[x, i]^2-1]];
e=PolynomialReduce[yp,GroebnerBasis[F, X,MonomialOrder->DegreeReverseLexicographic],X][[2]];
For[i=1,i<=Length[G[[1]]],i++,
If[PolynomialReduce[Subscript[x, i],e,X][[2]]==Subscript[x, i],l=Append[l,0]];
If[PolynomialReduce[Subscript[x, i],e,X][[2]]==0,l=Append[l,1]];
];
Return[Mod[l-y,2]];
];

DecodificacionCompleta[y_,G_,mod_]:=Module[{a,Var,sol,d},
a=Decodificar[y,G,mod][[1]];
Var=Table[Subscript[U, i],{i,1,FullSimplify[Log[Length[Codigo[G,mod]]]/Log[mod]]}];
sol=Solve[Var.G==a,Var,Modulus->mod];
d=(Var/.sol)[[1]];
Return[d];
];


End[];

Print["Se definieron Generatriz, Generadora, GeneradoraEstandar, Paridad, Decodificar, Decodificar2, DistanciaMinima, Codigo,  DecodificarGrobnerBinario y DecodificacionCompleta"];
