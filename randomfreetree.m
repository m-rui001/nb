(* outputs a random free tree by Wilf's algorithm, J Algorithms 2 (1981) 204 *)
(* the output is in my rectree format, see MATULA-BIG *)

stream="stdout" (* you can change this *)

alf[n_,q_]:=Block[{k=1,s=Series[1/(1-z),{z,0,n}]},
          While[k<q,s/=Series[(1-z^(k+1))^s[[3]][[k+1]],{z,0,n}];k++];s][[3]]
(* [[m+1]] gives rooted m-node forests with no subtree of more than q nodes *)
nn=0
setup[n_]:=If[n!=nn,nn=n;q=Floor[(n-1)/2];a=alf[n-1,q]]

startstack:=stack={}; startstack
push[elt_]:=stack={elt,stack}
pop:=With[{elt=First[stack]},stack=Last[stack];elt]

print[string_]:=WriteString[stream,string]
newline:=Write[stream]
writespec[spec_]:=With[{size=spec[[1]],offset=spec[[2]]},
           WriteString[stream,"T",size,"_",offset]]
printtop:=writespec[First[stack]]
randint[t_]:=Random[Integer,{1,t}]

deftree[spec_]:=Block[{size=spec[[1]],offset=spec[[2]],m,c}, If[size>2,
     writespec[spec];print["="];m=size-1;offset++;
     While[m>1,
      jd=choosejd[m];launch[jd,offset];c=jd[[1]]*jd[[2]];
      m-=c;offset+=c];
     If[m>0,launch[{1,1},offset]];
     print[".\n"]]]

freetree[n_,seed_]:=Block[{r,t},
    SeedRandom[seed];setup[n];startstack;
    print["% This is freetree["<>ToString[n]<>","<>ToString[seed]<>"]\n"];
    If[Mod[n,2]==1,push[{n,0}],
     t=a[[n/2]];r=randint[t(t+1)+2a[[n]]];
     If[r<=t,print["2"];push[{n/2,0}],
      If[r<=t(t+1),push[{n/2,0}];printtop;print[","];push[{n/2,n/2}],
       push[{n,0}]]]];
    printtop;print[".\n"];finish]

choosejd[m_]:=Block[{r,d,j,c},r=randint[m*a[[m+1]]];
   For[d=q;found=False, !found, d--,
    For[j=1;p=d;c=d*a[[d]], p<=m, j++;p+=d,
     If[r<=c*a[[m-p+1]], found=True;Break[],
        r-=c*a[[m-p+1]]]]];
   {j,d+1}]

launch[jd_,offset_]:=With[{j=jd[[1]],d=jd[[2]]},
    print["+"<>ToString[j]<>"T"<>ToString[d]<>"_"<>ToString[offset]];
    push[{d,offset}]]

finish:=While[stack!={},deftree[pop]]


         