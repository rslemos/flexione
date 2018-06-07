
%------------------------------------------------------------------------
%
%  Universidade Federal do Rio Grande do Sul
%  Instituto de Informática Teórica
%  Bacharelado em Ciências da Computação
%  Aluno: Emiliano Gomes Padilha
%
%  Projeto de Graduação			Semestre 93/2
%  Título: Conjugação e Composição Verbal da Língua Portuguesa
%  Área: Inteligência Artificial
%  Campo: Processamento de Língua Natural
%  Orientadora: Prof. Rosa Maria Viccari
%
%------------------------------------------------------------------------
% Nota: este arquivo possui caracteres acentuados da tabela de caracteres
% do SunOS. É importante que os acentos sejam visíveis neste programa, pois
% ele gera palavras portuguesas (naturalmente) acentuadas. Corrija através
% de um editor de textos (recurso de busca-e-troca) com os caracteres da
% tabela do seu programa/sistema operacional.
% Os caracteres acentuados utilizados aqui são:
%            á  -  a com acento agudo
%            é  -  e com acento agudo
%            í  -  i com acento agudo
%            ó  -  o com acento agudo
%            ú  -  u com acento agudo
%            ã  -  a com til
%            õ  -  o com til
%            â  -  a com acento circunflexo
%            ê  -  e com acento circunflexo
%            ô  -  o com acento circunflexo
%            à  -  a com acento grave (crase)
%            ü  -  u com trema
%            ç  -  c cedilha
%            Á  -  A com acento agudo
%            É  -  E com acento agudo
%--------------------------------------------------------------------------


membro(X,[X|_]).
membro(X,[_|Y]):- membro(X,Y).


%________separador de palavras (modificado)

l --> [C], { var(C) }, !, { " "=[C] }.
l([],[]).
l([X|L],[X|L]):- \+ number(X), !.
l --> [C], { branco(C) }, l2.

l2 --> [C], { branco(C) }, l2.
l2 --> [].

branco(C):- C >= 0, C =< 32 ; C = 127.


%__________________pronomes átonos____________________

n(_+p) --> "s".
n(_+s) --> "".

o(m+N) --> "o", n(_+N).
o(f+N) --> "a", n(_+N).

pro(_+s+1+_,0+0) --> "me".
pro(_+s+2+_,0+0) --> "te".
pro(_+p+1+_,0+1) --> "nos".
pro(_+p+2+_,0+1) --> "vos".
pro( GN+3+i,0+0) --> "lhe", n(GN).
pro(  _+3+r,0+0) --> "se".
pro( GN+3+o,1  ) --> o(GN).


%________próclise, ênclise, mesóclise

v(V,I,p(P)) --> pro(P,_), l, imp(V,I,M), { \+ M=1 }, l.
v(V,I,MODO) --> imp(V,I,M), posclise(I,M,MODO), l.
v(V,I,e(P)) --> imp(V,_+i(i),M), 
		{ \+ membro(V,[aprazer,dizer,fazer,trazer]) }, 
		"-", mesoclise(P,M,I), l.
v(V,I,e(P)) --> imp(V,_+f,M), "-", mesoclise(P,M,I), l.

posclise(I,M,e(P)) --> "-", enclise(P,M,I).
posclise(_,M,   -) --> { \+ M=1 }.

mesoclise(P,M,I+fp) --> enclise(P,M,_), "-", fp(I,W), { \+ W=1 }.
mesoclise(P,M,I+fi) --> enclise(P,M,_), "-", fi(I,W), { \+ W=1 }.

enclise(P,-,_) -->      pro(P,_).
enclise(P,n,_) -->      pro(P,0+_).
enclise(P,n,_) --> "n", pro(P,1).
enclise(P,0,_) -->      pro(P,0+0).
enclise(P,1,_) --> "l", pro(P,1).
enclise(P,0,I+T) -->    pro(P,0+1), { T=i(i) ; I \== p+1 }, !.
enclise(P,1,I+T) -->    pro(P,0+1), { \+ T=i(i), I ==  p+1 }.



%__________________verbos com imperativo____________________
imp(V,I,M) --> vrb(V,I,M).
imp(ser,s+2+imp(a),-) --> "sê".
imp(ser,p+2+imp(a),-) --> "sede".
imp(  V,p+2+imp(a),-) --> { \+ V=ser }, vrb(V,p+2+pr,1).
imp(  V,s+2+imp(a),M) --> { \+ V=ser }, vrb(V,s+3+pr,M).
imp(  V,N+3+imp(a),M) --> vrb(V,N+3+sp,M).
imp(  V,p+1+imp(a),M) --> vrb(V,p+1+sp,M).
imp(  V, NP+imp(n),M) --> vrb(V,NP+sp,M), { \+ NP=s+1, \+ M=1 }.



%__________________composição verbal (nível sintático)____________________

%________equivalência entre gerúndio e infinitivo
v(V,I+g(T),A) --> "a", l, v(V,I+i(T),A).

%________voz passiva
lv([ser,A,J+p(i,GN),V,A2],I) --> v(ser,I,A), v(V,J+p(i,GN),A2).

%________tempos compostos
lv([  ter,A,J+p(r,m+s)|L],I) -->   v(ter,I,A), lv(L,J+p(r,m+s)).
lv([haver,A,J+p(r,m+s)|L],I) --> v(haver,I,A), lv(L,J+p(r,m+s)).

%________auxiliares de gerúndio e particípio (verbos de ligação)
lv([V,A,J|L],I) --> v(V,I,A), { aux(V) }, lv(L,J), { membro(J,[_+p(i,_),_+g(_)]) }.

aux(estar).	aux(viver).	aux(continuar).
aux(andar).	aux(ficar).	aux(permanecer).

%________auxiliares de infinitivo
lv([V,A,J+i(T)|L],I) --> v(V,I,A), { aux(V,P) }, inl(P), lv(L,J+i(T)).

in([L|R]) --> [L], in(R).
in([]) --> [].

inl("") --> [], !.
inl(P) --> in(P), l.

aux(poder,"").		aux(saber,"").		aux(dever,"").
aux(querer,"").		aux(ir,"").		aux(costumar,"").
aux(ousar,"").		aux(desejar,"").	aux(vir,"").
aux(principiar,"a").	aux(comecar,"a").	aux(tornar,"a").
aux(acabar,"de").	aux(cessar,"de").	aux(gostar,"de").
aux(haver,"de").

%________voz ativa (e último verbo de uma locução)
lv([V,A],I) --> v(V,I,A).




%__________________conjugação verbal regular____________________

%________verificação de consoantes s/r/z finais
s(0) --> "s".
s(1) --> "".

r(0) --> "r".
r(1) --> "".

z(0) --> "z".	% aparece somente nos verbos terminados em "zir" e nas
z(1) --> "".	% formas "apraz", "diz", "traz", "faz", "fez" e "fiz".

%________acentuação das vogais (temáticas)
a(_,0) --> "a".
a(a,1) --> "á".

e(_,0) --> "e".
e(f,1) --> "ê".
e(a,1) --> "é".

i(_,0) --> "i".
i(a,1) --> "í".

o(_,0) --> "o".
o(f,1) --> "ô".
o(a,1) --> "ó".

u(_,0) --> "u".
u(a,1) --> "ú".
u(t,1) --> "ü".

%________desinências regulares
de(p+1,M) --> "mo", s(M).
de(p+2,M) --> "i", s(M).
de(p+3,n) --> "m".
de(s+1,-) --> "".
de(s+3,-) --> "".
de(s+2,M) --> s(M).

%________desinências com vogal temática
de2(K+T,NP,M) --> { vt(K+T,NP) }, de(NP), de(NP,M).
de3(K+T,NP,M) --> { vt(K+T,NP) }, de(NP,M).

%________acentuação da vogal temática em p+1/p+2
vt(0+1,p+P):- membro(P,[1,2]).
vt(0+0,N+P):- membro(N+P,[s+_,p+3]).
vt(1+1,_).

%________vogal da desinência de p+2 dos tempos pi/pm
de(p+2) --> "e".
de(N+P) --> "a", { \+ N+P=p+2 }.


%________desinências do presente do indicativo
pr(_+"a") --> "a".  % louv+a
pr(0+"e") --> "e".  % vend+e
pr(1+"e") --> "i".  % dó+i (dó+e)
pr(0+"i") --> "e".  % part+e
pr(1+"i") --> "i".  % ca+i (ca+e)

%________desinências do presente do subjuntivo
sp("a") --> "e".	% louv+e
sp("e") --> "a".	% vend+a
sp("i") --> "a".	% part+a

%________acentuação do "i" como vogal temática nos tempos pr/pp
vo(K+"i") --> !, i(a,K).
vo(_+[V]) --> [V].


%________desinências do futuro do indicativo
fp(s+1,-) --> "ei".
fp(p+3,n) --> "ão".
fp(s+P,M) --> "á", de(s+P,M), { membro(P,[2,3]) }.
fp(p+P,M) --> "e", de(p+P,M), { membro(P,[1,2]) }.
fi(N+P,M) --> i(a,T), de2(0+T,N+P,M).


%________infinitivo impessoal e futuro do subjuntivo (s+1, s+3)
inf(_+"a",M) --> a(a,M), r(M).  % matá-lo
inf(_+"e",M) --> e(f,M), r(M).  % dizê-lo
inf(_+"é",M) --> e(a,M), r(M).  % verbos irregulares (no fut.subj.)
inf(0+"i",M) --> i(a,0), r(M).  % feri-lo
inf(1+"i",M) --> i(a,M), r(M).  % traí-lo
inf(0+"o",M) --> o(f,M), r(M).  % for, fô-lo (somente)
inf(1+"o",M) --> o(f,1), r(M).  % pôr, pô-lo (somente)

%________infinitivo pessoal e futuro do subjuntivo
inf(_+[V],p+2,M) --> [V], "rde", s(M).
inf(_+[V],p+1,M) --> [V], "r", de(p+1,M).
inf(_+"a",N+P,M) --> a(a,0), "re", de(N+P,M), { membro(N+P,[s+2,p+3]) }.
inf(_+"e",N+P,M) --> e(f,0), "re", de(N+P,M), { membro(N+P,[s+2,p+3]) }.
inf(K+"i",N+P,M) --> i(a,K), "re", de(N+P,M), { membro(N+P,[s+2,p+3]) }.
inf(_+"o",N+P,M) --> o(f,0), "re", de(N+P,M), { membro(N+P,[s+2,p+3]) }.


%________desinências dos verbos regulares

reg(_+[V],  _+g(-),-) --> [V], "ndo".
reg(_+"a",_+p(_,I),+) --> "ad",        o(I).
reg(K+"e",_+p(_,I),+) --> i(a,K), "d", o(I).
reg(K+"i",_+p(_,I),+) --> i(a,K), "d", o(I).
reg(K+[V],  _+i(i),M) --> inf(K+[V],M).
reg(K+[V],s+P+i(f),M) --> inf(K+[V],M), { membro(P,[1,3]) }.
reg(K+[V], NP+i(f),M) --> inf(K+[V],NP,M).

reg(_+[_],s+1+pr,M) --> "o", de(s+1,M).
reg(K+[V],s+P+pr,M) --> pr(K+[V]), de(s+P,M), { membro(P,[2,3]) }.
reg(_+[V],p+3+pr,M) --> pr(0+[V]), de(p+3,M).
reg(K+[V],p+1+pr,M) --> vo(K+[V]), de(p+1,M).
reg(_+"a",p+2+pr,M) --> "a", de(p+2,M).
reg(_+"e",p+2+pr,M) --> "e", de(p+2,M).
reg(K+"i",p+2+pr,M) --> i(a,K), s(M).

reg(_+"a",s+1+pp,-) --> "ei".
reg(K+"e",s+1+pp,-) --> i(a,K).
reg(K+"i",s+1+pp,-) --> i(a,K).
reg(_+"a",s+3+pp,-) --> "ou".
reg(_+"e",s+3+pp,-) --> "eu".
reg(_+"i",s+3+pp,-) --> "iu".
reg(K+[V],p+1+pp,M) --> vo(K+[V]), de(p+1,M).
reg(K+[V],p+3+pp,M) --> vo(K+[V]), "ra", de(p+3,M).
reg(K+[V],s+2+pp,-) --> vo(K+[V]), "ste".
reg(K+[V],p+2+pp,M) --> vo(K+[V]), "ste", s(M).

reg(_+"a",NP+pi,M) --> a(a,T), "v", de2(0+T,NP,M).
reg(K+"e",NP+pi,M) --> i(a,T),      de2(K+T,NP,M).
reg(K+"i",NP+pi,M) --> i(a,T),      de2(K+T,NP,M).

reg(_+"a",NP+pm,M) --> a(a,T), "r", de2(0+T,NP,M).
reg(_+"e",NP+pm,M) --> e(f,T), "r", de2(0+T,NP,M).
reg(K+"i",NP+pm,M) --> i(a,T), "r", de2(K+T,NP,M).
reg(_+"é",NP+pm,M) --> e(a,T), "r", de2(0+T,NP,M). %verbos irregulares
reg(_+"o",NP+pm,M) --> o(f,T), "r", de2(0+T,NP,M). %verbos irregulares

reg(_+[V],NP+fp,+) --> [V], "r", fp(NP,M), { \+ M=1 }.
reg(_+[V],NP+fi,+) --> [V], "r", fi(NP,M), { \+ M=1 }.

reg(_+[V],NP+sp,M) --> sp([V]), de(NP,M).

reg(_+"a",NP+si,M) --> a(a,T), "sse", de3(0+T,NP,M).
reg(_+"e",NP+si,M) --> e(f,T), "sse", de3(0+T,NP,M).
reg(K+"i",NP+si,M) --> i(a,T), "sse", de3(K+T,NP,M).
reg(_+"é",NP+si,M) --> e(a,T), "sse", de3(0+T,NP,M). %verbos irregulares
reg(_+"o",NP+si,M) --> o(f,T), "sse", de3(0+T,NP,M). %verbos irregulares

reg(K+[V],s+P+sf,M) --> inf(K+[V],M), { membro(P,[1,3]) }.
reg(K+"é", NP+sf,M) --> !, inf(K+"e",NP,M).	%verbos irregulares
reg(K+[V], NP+sf,M) --> inf(K+[V],NP,M).



%__________________outras irregularidades____________________
co(1,I,L):- membro(I,L).
co(0,I,L):- \+ membro(I,L).

%________alterações ortográficas próximo às desinências verbais
c_(0) --> "ç".	  c_(1) --> "c".	% de ...çar, ...cer, ...cir
k(0) --> "c".	  k(1) --> "qu".	% de ...car, ...quer, ...quir
g(0) --> "g".	  g(1) --> "gu".	% de ...gar, ...guer, ...guir
j(0) --> "g".	  j(1) --> "j".		% de ...ger, ...gir
i(0) --> "".      i(1) --> "i".         % de ...ear, ...air

ei(0) --> "i".	  ei(1) --> "ei".	% só em ansiar/incendiar/odiar
ui(0) --> "u".	  ui(1) --> "ó".	% só em construir/destruir
ca(0) --> "d".	  ca(1) --> "c".	% só em perder
lh(0) --> "l".	  lh(1) --> "lh".	% só em valer
d(0) --> "d".	  d(1) --> "ç".		% só em medir/pedir
v(0) --> "v".	  v(1) --> "ç".		% só em ouvir

%________regras das alterações ortográficas próximo às desinências
ort3(["ca","ça","ce","ci","ga","ge","gi","zi","oe","oa","éa","ea","ai"]).

ort("ca",I,M) --> k(K), reg(0+"a",I,M), { co(K,I,[s+1+pp,_+sp]) }.
ort("ça",I,M) --> c_(K), reg(0+"a",I,M), { co(K,I,[s+1+pp,_+sp]) }.
ort("ce",I,M) --> c_(K), reg(0+"e",I,M), { \+ co(K,I,[s+1+pr,_+sp]) }.
ort("ci",I,M) --> c_(K), reg(0+"i",I,M), { \+ co(K,I,[s+1+pr,_+sp]) }.

ort("ga",I,M) --> g(K), reg(0+"a",I,M), { co(K,I,[s+1+pp,_+sp]) }.
ort("ge",I,M) --> j(K), reg(0+"e",I,M), { co(K,I,[s+1+pr,_+sp]) }.
ort("gi",I,M) --> j(K), reg(0+"i",I,M), { co(K,I,[s+1+pr,_+sp]) }.
ort("zi",I,M) --> "z",  reg(0+"i",I,M), { \+ I=s+3+pr } ; z(M), { I = s+3+pr }.

ort("oe",I,M) --> o(a,1), reg(1+"e",I,M), { membro(I,[s+2+pr,_+3+pr]) }.
ort("oe",I,M) --> o(f,K), reg(1+"e",I,M), { co(K,I,[s+1+pr]), \+ membro(I,[s+2+pr,_+3+pr]) }.
ort("oa",I,M) --> o(f,K),       reg(1+"a",I,M), { co(K,I,[s+1+pr]) }.
ort("éa",I,M) --> e(a,K), i(K), reg(1+"a",I,M), { co(K,I,[s+_+pr,p+3+pr,s+_+sp,p+3+sp]) }.
ort("ea",I,M) --> "e",    i(K), reg(1+"a",I,M), { co(K,I,[s+_+pr,p+3+pr,s+_+sp,p+3+sp]) }.
ort("ai",I,M) --> "a",    i(K), reg(1+"i",I,M), { co(K,I,[s+1+pr,_+sp]) }.

% (existem verbos terminados em aer/ier/uer/eir/oir??)

%________alterações ortográficas das combinações de gu/qu
ort4(["ue","ui","ua","üi","úa","úi"]).

ort("q","ue",I,M) --> k(K), reg(0+"e",I,M), { \+ co(K,I,[s+1+pr,_+sp]) }.
ort("q","ui",I,M) --> k(K), reg(0+"i",I,M), { \+ co(K,I,[s+1+pr,_+sp]) }.
ort("g","ue",I,M) --> g(K), reg(0+"e",I,M), { \+ co(K,I,[s+1+pr,_+sp]) }.
ort("g","ui",I,M) --> g(K), reg(0+"i",I,M), { \+ co(K,I,[s+1+pr,_+sp]) }.
ort([C],"ua",I,M) --> [C], u(t,K), reg(1+"a",I,M), { co(K,I,[s+1+pp,_+sp]) }.
ort([C],"üi",I,M) --> [C], u(t,K), reg(1+"i",I,M), { \+ co(K,I,[s+1+pr,_+sp]) }.
ort([C],"úa",I,M) --> [C], u(a,1), reg(1+"a",I,M), { membro(I,[s+_+pr,p+3+pr,s+_+sp,p+3+sp]) }.
ort([C],"úa",I,M) --> ort([C],"ua",I,M),        { \+ membro(I,[s+_+pr,p+3+pr,s+_+sp,p+3+sp]) }.
ort([C],"úi",I,M) --> [C], u(a,1), reg(1+"i",I,M), { membro(I,[s+_+pr,p+3+pr,s+_+sp,p+3+sp]) }.
ort([C],"úi",I,M) --> ort([C],"üi",I,M),        { \+ membro(I,[s+_+pr,p+3+pr,s+_+sp,p+3+sp]) }.


%________alterações ortográficas específicas (apenas alguns verbos)
esp("ia",I,M) --> ei(K), reg(1+"a",I,M), { co(K,I,[s+_+pr,p+3+pr,s+_+sp,p+3+sp]) }.
esp("ui",I,M) --> ui(K), reg(1+"i",I,M), { co(K,I,[s+2+pr,_+3+pr]) }.
esp("de",I,M) --> ca(K), reg(0+"e",I,M), { co(K,I,[s+1+pr,_+sp]) }.
esp("le",I,M) --> lh(K), reg(0+"e",I,M), { co(K,I,[s+1+pr,_+sp]) }.
esp("di",I,M) --> d(K),  reg(0+"i",I,M), { co(K,I,[s+1+pr,_+sp]) }.
esp("vi",I,M) --> v(K),  reg(0+"i",I,M), { co(K,I,[s+1+pr,_+sp]) }.


%________mutações vocálicas no radical (de verbos de 3a. conjugação)
e_i(0) --> "e".		e_i(1) --> "i".
o_u(0) --> "o".		o_u(1) --> "u".

%________regras de mutações vocálicas
mut("e",R,I,M) --> e_i(K), cjg(R,I,M), { co(K,I,[s+1+pr,_+sp]) }.
mut("ë",R,I,M) --> e_i(K), cjg(R,I,M), { co(K,I,[s+_+pr,p+3+pr,_+sp]) }.
mut("i",R,I,M) --> e_i(K), cjg(R,I,M), { \+ co(K,I,[s+2+pr,_+3+pr]) }.
mut("o",R,I,M) --> o_u(K), cjg(R,I,M), { co(K,I,[s+1+pr,_+sp]) }.
mut("u",R,I,M) --> o_u(K), cjg(R,I,M), { \+ co(K,I,[s+2+pr,_+3+pr]) }.

%________irregularidades funcionalmente idênticas à mutação vocálica
mut("á",R,I,M) --> a(a,K), cjg(R,I,M), { co(K,I,[s+_+pr,p+3+pr,s+_+sp,p+3+sp]) }.
mut("é",R,I,M) --> e(a,K), cjg(R,I,M), { co(K,I,[s+_+pr,p+3+pr,s+_+sp,p+3+sp]) }.
mut("í",R,I,M) --> i(a,K), cjg(R,I,M), { co(K,I,[s+_+pr,p+3+pr,s+_+sp,p+3+sp]) }.
mut("ó",R,I,M) --> o(a,K), cjg(R,I,M), { co(K,I,[s+_+pr,p+3+pr,s+_+sp,p+3+sp]) }.
mut("ú",R,I,M) --> u(a,K), cjg(R,I,M), { co(K,I,[s+_+pr,p+3+pr,s+_+sp,p+3+sp]) }.



%________cjg/5: predicado de uso geral
vogais(V,1):- membro([V],["a","e","i","o","u"]), !.
vogais(_,0).

cjg([94,V|R],I,M)  --> !, mut([V],R,I,M).      % 94: código de ^
cjg([47|R],I,M)    --> !, esp(R,I,M).          % 47: código de /
cjg([L,V],I,M)     --> { ort3(Cs), membro([L,V],Cs) }, !, ort([L,V],I,M).
cjg([L,V],I,M)     --> !, [L], { vogais(L,K) }, reg(K+[V],I,M).
cjg([103,L,V],I,M) --> { ort4(Cs), membro([L,V],Cs) }, !, ort([103],[L,V],I,M).
cjg([113,L,V],I,M) --> { ort4(Cs), membro([L,V],Cs) }, !, ort([113],[L,V],I,M).
cjg([L|R],I,M)     --> [L], cjg(R,I,M).        %103,113: códigos de g/q

%________particípios irregulares
cjg(_+L,_+p(i,I),+) --> nom(L,I).
cjg(L+_,_+p(r,I),+) --> cjg(L,_+p(_,I),+).
cjg(L+_,       I,M) --> cjg(L,I,M), { \+ I=_+p(_,_) }.
cjg(_-L,_+p(_,I),+) --> nom(L,I).
cjg(L-_,       I,M) --> cjg(L,I,M), { \+ I=_+p(_,_) }.

nom([L|R],I) --> [L], nom(R,I).
nom("o",I)   --> o(I).
nom("e",I)   --> "e", n(I).



%__________________conjugação de verbos (muito) irregulares_____________________

%________estar

estar(s+1+pr,-) --> "ou".
estar(p+3+pr,n) --> "ão".
estar(s+P+pr,M) --> "á", de(s+P,M), { membro(P,[2,3]) }.
estar(p+P+pr,M) --> reg(0+"a",p+P+pr,M), { membro(P,[1,2]) }.

estar(NP+sp,M) --> "ej", reg(0+"e",NP+sp,M).

estar(NP+pi    ,M) --> reg(0+"a",NP+pi,M).
estar(NP+fp    ,M) --> reg(0+"a",NP+fp,M).
estar(NP+fi    ,M) --> reg(0+"a",NP+fi,M).
estar(NP+i(I)  ,M) --> reg(0+"a",NP+i(I),M).
estar(NP+g(I)  ,M) --> reg(0+"a",NP+g(I),M).
estar(NP+p(_,I),M) --> reg(0+"a",NP+p(_,I),M).

estar(NP+pp,M) --> ter(0,NP+pp,M).
estar(NP+pm,M) --> ter(0,NP+pm,M).
estar(NP+si,M) --> ter(0,NP+si,M).
estar(NP+sf,M) --> ter(0,NP+sf,M).

%________dar

dar(s+1+pr,-) --> "ou".
dar(p+3+pr,n) --> "ão".
dar(s+P+pr,M) --> "á", de(s+P,M), { membro(P,[2,3]) }.
dar(p+P+pr,M) --> reg(0+"a",p+P+pr,M), { membro(P,[1,2]) }.

dar(s+P+sp,M) --> "ê", de(s+P,M).
dar(p+3+sp,M) --> "êe", de(p+3,M).
dar(p+P+sp,M) --> reg(0+"a",p+P+sp,M), { membro(P,[1,2]) }.

dar(NP+pi    ,M) --> reg(0+"a",NP+pi,M).
dar(NP+fp    ,M) --> reg(0+"a",NP+fp,M).
dar(NP+fi    ,M) --> reg(0+"a",NP+fi,M).
dar(NP+i(I)  ,M) --> reg(0+"a",NP+i(I),M).
dar(NP+g(I)  ,M) --> reg(0+"a",NP+g(I),M).
dar(NP+p(_,I),M) --> reg(0+"a",NP+p(_,I),M).

dar(s+1+pp,M) --> reg(0+"a",s+1+pp,M).
dar( NP+pp,M) --> reg(0+"e",NP+pp,M), { \+ NP=s+1 }.

dar(NP+pm,M) --> reg(0+"é",NP+pm,M).
dar(NP+si,M) --> reg(0+"é",NP+si,M).
dar(NP+sf,M) --> reg(0+"é",NP+sf,M).

%________ser

ser(s+1+pr,-) --> "s", "ou".
ser(p+3+pr,n) --> "s", "ão".
ser(s+P+pr,M) -->      "é", de(s+P,M), { membro(P,[2,3]) }.
ser(p+P+pr,M) --> "s", "o", de(p+P,M), { membro(P,[1,2]) }.

ser(NP+sp,M) --> "s", "ej", reg(0+"e",NP+sp,M).

ser(NP+pi,M) --> reg(0+"é",NP+pm,M).

ser(NP+fp    ,M) --> "s", reg(0+"e",NP+fp,M).
ser(NP+fi    ,M) --> "s", reg(0+"e",NP+fi,M).
ser(NP+i(I)  ,M) --> "s", reg(0+"e",NP+i(I),M).
ser(NP+g(I)  ,M) --> "s", reg(0+"e",NP+g(I),M).
ser(NP+p(_,I),M) --> "s", { I=m+s }, reg(0+"e",NP+p(_,I),M).

ser(s+1+pp,-) --> "f", "ui".
ser(s+3+pp,-) --> "f", "oi".
ser( NP+pp,M) --> "f", reg(0+"o",NP+pp,M), { membro(NP,[s+2,p+_]) }.

ser(NP+pm,M) --> "f", reg(0+"o",NP+pm,M).
ser(NP+si,M) --> "f", reg(0+"o",NP+si,M).
ser(NP+sf,M) --> "f", reg(0+"o",NP+sf,M).

%________ter (e derivados)

ter(I,M) --> "t", ter(1,I,M).

ter(_,s+1+pr,M) --> "enh", reg(0+"e",s+1+pr,M).
ter(_,p+1+pr,M) --> reg(0+"e",p+1+pr,M).
ter(_,p+3+pr,M) --> "ê", de(p+3,M).
ter(K,s+3+pr,n) --> e(a,K), "m".
ter(K,s+2+pr,1) --> e(a,K), "m".
ter(_,s+2+pr,0) --> "e",    "ns".
ter(_,p+2+pr,M) --> "e",    "nde", s(M).

ter(_,NP+sp,M) --> "enh", reg(0+"e",NP+sp,M).

ter(_,NP+pi,M) --> i(a,T), "nh", de2(0+T,NP,M).

ter(_,NP+fp    ,M) --> reg(0+"e",NP+fp,M).
ter(_,NP+fi    ,M) --> reg(0+"e",NP+fi,M).
ter(_,NP+i(I)  ,M) --> reg(0+"e",NP+i(I),M).
ter(_,NP+g(I)  ,M) --> reg(0+"e",NP+g(I),M).
ter(_,NP+p(_,I),M) --> reg(0+"e",NP+p(_,I),M).

ter(_,s+1+pp,-) --> "ive".
ter(_,s+3+pp,-) --> "eve".
ter(_, NP+pp,M) --> "iv", reg(0+"e",NP+pp,M), { membro(NP,[s+2,p+_]) }.

ter(_,NP+pm,M) --> "iv", reg(0+"é",NP+pm,M).
ter(_,NP+si,M) --> "iv", reg(0+"é",NP+si,M).
ter(_,NP+sf,M) --> "iv", reg(0+"é",NP+sf,M).

%________haver

haver( NP+pr,M) -->       fp(NP,M), { membro(NP,[s+_,p+3]) }.
haver(p+P+pr,M) --> "av", fp(p+P,M), { membro(P,[1,2]) }.

haver(NP+sp,M) --> "aj", reg(0+"e",NP+sp,M).

haver(NP+pi    ,M) --> "av", reg(0+"e",NP+pi,M).
haver(NP+fp    ,M) --> "av", reg(0+"e",NP+fp,M).
haver(NP+fi    ,M) --> "av", reg(0+"e",NP+fi,M).
haver(NP+i(I)  ,M) --> "av", reg(0+"e",NP+i(I),M).
haver(NP+g(I)  ,M) --> "av", reg(0+"e",NP+g(I),M).
haver(NP+p(_,I),M) --> "av", reg(0+"e",NP+p(_,I),M).

haver(s+1+pp,-) --> "ouv", "e".
haver(s+3+pp,-) --> "ouv", "e".
haver( NP+pp,M) --> "ouv", reg(0+"e",NP+pp,M), { membro(NP,[s+2,p+_]) }.

haver(NP+pm,M) --> "ouv", reg(0+"é",NP+pm,M).
haver(NP+si,M) --> "ouv", reg(0+"é",NP+si,M).
haver(NP+sf,M) --> "ouv", reg(0+"é",NP+sf,M).

%________pôr (e derivados)

por(I,M) --> "p", por(0,I,M).

por(_,s+1+pr,M) --> "onh", reg(0+"e",s+1+pr,M).
por(_,s+3+pr,n) --> "õe".
por(_, NP+pr,M) --> "õe", de(NP,M), { membro(NP,[s+2,p+3]) }.
por(_,p+1+pr,M) --> "o",  de(p+1,M).
por(_,p+2+pr,M) --> "o", "nde", s(M).

por(_,NP+sp,M) --> "onh", reg(0+"e",NP+sp,M).

por(_,NP+pi,M) --> u(a,T), "nh", de2(0+T,NP,M).

por(_,NP+fp  ,M) --> reg(0+"o",NP+fp,M).
por(_,NP+fi  ,M) --> reg(0+"o",NP+fi,M).
por(_,NP+g(I),M) --> reg(0+"o",NP+g(I),M).

por(K,NP+i(I),M) --> reg(K+"o",NP+i(I),M).

por(_, _+p(_,I),+) --> "o", "st", o(I).

por(_,s+1+pp,M) --> "u", s(M).
por(_,s+3+pp,M) --> "ô", s(M).
por(_, NP+pp,M) --> "us", reg(0+"e",NP+pp,M), { membro(NP,[s+2,p+_]) }.

por(_,NP+pm,M) --> "us", reg(0+"é",NP+pm,M).
por(_,NP+si,M) --> "us", reg(0+"é",NP+si,M).
por(_,NP+sf,M) --> "us", reg(0+"é",NP+sf,M).

%________ir/transir

ir(s+1+pr,-) --> "v", "ou".
ir(p+3+pr,n) --> "v", "ão".
ir(s+P+pr,M) --> "v", "ai", de(s+P,M), { membro(P,[2,3]) }.
ir(p+1+pr,M) --> "v", "a",  de(p+1,M).
ir(p+2+pr,M) --> "i", "de", s(M).

ir(s+P+sp,M) --> "vá", de(s+P,M).
ir(p+1+sp,M) --> "va", de(p+1,M).
ir(p+3+sp,n) --> "v", "ão".
ir(p+2+sp,M) --> "va", "de", s(M).

ir(NP+pi    ,M) --> reg(0+"i",NP+pi,M).
ir(NP+fp    ,M) --> reg(0+"i",NP+fp,M).
ir(NP+fi    ,M) --> reg(0+"i",NP+fi,M).
ir(NP+i(I)  ,M) --> reg(1+"i",NP+i(I),M). %% í-lo, íres, írem ??
ir(NP+g(I)  ,M) --> reg(0+"i",NP+g(I),M).
ir(NP+p(_,I),M) --> reg(0+"i",NP+p(_,I),M).

ir(NP+pp,M) --> ser(NP+pp,M).
ir(NP+pm,M) --> ser(NP+pm,M).
ir(NP+si,M) --> ser(NP+si,M).
ir(NP+sf,M) --> ser(NP+sf,M).

transir(NP+i(I)  ,M) --> reg(0+"i",NP+i(I),M).
transir(NP+p(_,J),M) --> reg(0+"i",NP+p(_,J),M).

%________vir (e derivados)

vir(I,M) --> "v", vir(1,I,M).

vir(_,s+1+pr,M) --> "enh", reg(0+"i",s+1+pr,M).
vir(_,p+1+pr,M) --> reg(0+"i",p+1+pr,M).
vir(_,p+3+pr,M) --> "ê", de(p+3,M).
vir(K,s+3+pr,n) --> e(a,K), "m".
vir(K,s+2+pr,1) --> e(a,K), "m".
vir(_,s+2+pr,0) --> "e",    "ns".
vir(_,p+2+pr,M) --> "i",    "nde", s(M).

vir(_,NP+sp,M) --> "enh", reg(0+"i",NP+sp,M).

vir(_,NP+pi,M) --> i(a,T), "nh", de2(0+T,NP,M).

vir(_,NP+fp  ,M) --> reg(0+"i",NP+fp,M).
vir(_,NP+fi  ,M) --> reg(0+"i",NP+fi,M).
vir(_,NP+i(I),M) --> reg(0+"i",NP+i(I),M).
vir(_,NP+g(I),M) --> reg(0+"i",NP+g(I),M).

vir(_, _+p(_,I),+) --> "i", "nd", o(I).

vir(_,s+1+pp,n) --> "im".
vir(_,s+3+pp,-) --> "eio".
vir(_, NP+pp,M) --> "i", reg(1+"e",NP+pp,M), { membro(NP,[s+2,p+_]) }.

vir(_,NP+pm,M) --> "i", reg(1+"é",NP+pm,M).
vir(_,NP+si,M) --> "i", reg(1+"é",NP+si,M).
vir(_,NP+sf,M) --> "i", reg(1+"é",NP+sf,M).

%________rir (0), sorrir (1)

rir(_,s+1+pr,M) --> "i", reg(0+"i",s+1+pr,M).
rir(_,p+1+pr,M) --> reg(0+"i",p+1+pr,M).
rir(_,s+P+pr,M) --> "i",  de(s+P,M), { membro(P,[2,3]) }.
rir(_,p+3+pr,M) --> "ie", de(p+3,M).
rir(0,p+2+pr,M) --> "i", "de", s(M).
rir(1,p+2+pr,M) --> reg(0+"i",p+2+pr,M).

rir(_,NP+sp,M) --> "i", reg(0+"i",NP+sp,M).

rir(_,NP+pi    ,M) --> reg(0+"i",NP+pi,M).
rir(_,NP+fp    ,M) --> reg(0+"i",NP+fp,M).
rir(_,NP+fi    ,M) --> reg(0+"i",NP+fi,M).
rir(_,NP+i(I)  ,M) --> reg(0+"i",NP+i(I),M).
rir(_,NP+g(I)  ,M) --> reg(0+"i",NP+g(I),M).
rir(_,NP+p(_,I),M) --> { I=m+s }, reg(0+"i",NP+p(_,I),M).
rir(_,NP+pp    ,M) --> reg(0+"i",NP+pp,M).
rir(_,NP+pm    ,M) --> reg(0+"i",NP+pm,M).
rir(_,NP+si    ,M) --> reg(0+"i",NP+si,M).
rir(_,NP+sf    ,M) --> reg(0+"i",NP+sf,M).

%________ler, crer

xxer(s+1+pr,M) --> "ei", reg(1+"e",s+1+pr,M).
xxer(p+1+pr,M) --> reg(0+"e",p+1+pr,M).
xxer(s+P+pr,M) --> "ê",  de(s+P,M), { membro(P,[2,3]) }.
xxer(p+3+pr,M) --> "êe", de(p+3,M).
xxer(p+2+pr,M) --> "e", "de", s(M).

xxer(NP+sp,M) --> "ei", reg(1+"e",NP+sp,M).

xxer(NP+pi    ,M) --> reg(0+"e",NP+pi,M).
xxer(NP+fp    ,M) --> reg(0+"e",NP+fp,M).
xxer(NP+fi    ,M) --> reg(0+"e",NP+fi,M).
xxer(NP+i(I)  ,M) --> reg(0+"e",NP+i(I),M).
xxer(NP+g(I)  ,M) --> reg(0+"e",NP+g(I),M).
xxer(NP+p(_,I),M) --> reg(0+"e",NP+p(_,I),M).
xxer(NP+pp    ,M) --> reg(0+"e",NP+pp,M).
xxer(NP+pm    ,M) --> reg(0+"e",NP+pm,M).
xxer(NP+si    ,M) --> reg(0+"e",NP+si,M).
xxer(NP+sf    ,M) --> reg(0+"e",NP+sf,M).

%________caber (0), saber (1)

xaber(1,s+1+pr,-) --> "ei".
xaber(0,s+1+pr,M) --> "aib", reg(0+"e",s+1+pr,M).
xaber(_, NP+pr,M) --> "ab",  reg(0+"e",NP+pr,M), { \+ NP=s+1 }.

xaber(_,NP+sp,M) --> "aib", reg(0+"e",NP+sp,M).

xaber(_,NP+pi    ,M) --> "ab", reg(0+"e",NP+pi,M).
xaber(_,NP+fp    ,M) --> "ab", reg(0+"e",NP+fp,M).
xaber(_,NP+fi    ,M) --> "ab", reg(0+"e",NP+fi,M).
xaber(_,NP+i(I)  ,M) --> "ab", reg(0+"e",NP+i(I),M).
xaber(_,NP+g(I)  ,M) --> "ab", reg(0+"e",NP+g(I),M).
xaber(_,NP+p(_,I),M) --> "ab", reg(0+"e",NP+p(_,I),M).

xaber(_,s+1+pp,-) --> "oub", "e".
xaber(_,s+3+pp,-) --> "oub", "e".
xaber(_, NP+pp,M) --> "oub", reg(0+"e",NP+pp,M), { membro(NP,[s+2,p+_]) }.

xaber(_,NP+pm,M) --> "oub", reg(0+"é",NP+pm,M).
xaber(_,NP+si,M) --> "oub", reg(0+"é",NP+si,M).
xaber(_,NP+sf,M) --> "oub", reg(0+"é",NP+sf,M).

%________ver (0), prover (1)

ver(_,s+1+pr,M) --> "ej", reg(0+"e",s+1+pr,M).
ver(_,p+1+pr,M) --> reg(0+"e",p+1+pr,M).
ver(_,s+P+pr,M) --> "ê",  de(s+P,M), { membro(P,[2,3]) }.
ver(_,p+3+pr,M) --> "êe", de(p+3,M).
ver(_,p+2+pr,M) --> "e", "de", s(M).

ver(_,NP+sp,M) --> "ej", reg(0+"e",NP+sp,M).

ver(_,NP+pi  ,M) --> reg(0+"e",NP+pi,M).
ver(_,NP+fp  ,M) --> reg(0+"e",NP+fp,M).
ver(_,NP+fi  ,M) --> reg(0+"e",NP+fi,M).
ver(_,NP+i(I),M) --> reg(0+"e",NP+i(I),M).
ver(_,NP+g(I),M) --> reg(0+"e",NP+g(I),M).

ver(0, _+p(_,I),+) --> "i", "st", o(I).
ver(1,NP+p(_,I),+) --> reg(0+"e",NP+p(_,I),+).

ver(K,NP+pp,M) --> { ver(K,T) }, reg(0+T,NP+pp,M).
ver(K,NP+pm,M) --> { ver(K,T) }, reg(0+T,NP+pm,M).
ver(K,NP+si,M) --> { ver(K,T) }, reg(0+T,NP+si,M).
ver(K,NP+sf,M) --> { ver(K,T) }, reg(0+T,NP+sf,M).

ver(0,"i").
ver(1,"e").

%________querer (0), requerer (1)

querer(_,s+3+pr,M) --> e(a,M), r(M).
querer(K,s+1+pr,M) --> "e", i(K), "r", reg(0+"e",s+1+pr,M).
querer(_, NP+pr,M) --> "er",  reg(0+"e", NP+pr,M), { membro(NP,[s+2,p+_]) }.

querer(_,NP+sp,M) --> "eir", reg(0+"e",NP+sp,M).

querer(_,NP+pi    ,M) --> "er", reg(0+"e",NP+pi,M).
querer(_,NP+fp    ,M) --> "er", reg(0+"e",NP+fp,M).
querer(_,NP+fi    ,M) --> "er", reg(0+"e",NP+fi,M).
querer(_,NP+i(I)  ,M) --> "er", reg(0+"e",NP+i(I),M).
querer(_,NP+g(I)  ,M) --> "er", reg(0+"e",NP+g(I),M).
%%querer(_,NP+p(_,I),M) --> "i", "st", o(I).
querer(_,NP+p(_,I),M) --> "er", reg(0+"e",NP+p(_,I),M).

querer(0,s+P+pp,M) --> "i", s(M), { membro(P,[1,3]) }.
querer(0, NP+pp,M) --> "is", reg(0+"e",NP+pp,M), { membro(NP,[s+2,p+_]) }.

querer(0,NP+pm,M) --> "is", reg(0+"é",NP+pm,M).
querer(0,NP+si,M) --> "is", reg(0+"é",NP+si,M).
querer(0,NP+sf,M) --> "is", reg(0+"é",NP+sf,M).

querer(1,NP+pp,M) --> "er", reg(0+"e",NP+pp,M).
querer(1,NP+pm,M) --> "er", reg(0+"e",NP+pm,M).
querer(1,NP+si,M) --> "er", reg(0+"e",NP+si,M).
querer(1,NP+sf,M) --> "er", reg(0+"e",NP+sf,M).

%________poder

poder(s+1+pr,M) --> "oss", reg(0+"e",s+1+pr,M).
poder( NP+pr,M) --> "od",  reg(0+"e",NP+pr,M), { \+ NP=s+1 }.

poder(NP+sp,M) --> "oss", reg(0+"e",NP+sp,M).

poder(NP+pi    ,M) --> "od", reg(0+"e",NP+pi,M).
poder(NP+fp    ,M) --> "od", reg(0+"e",NP+fp,M).
poder(NP+fi    ,M) --> "od", reg(0+"e",NP+fi,M).
poder(NP+i(I)  ,M) --> "od", reg(0+"e",NP+i(I),M).
poder(NP+g(I)  ,M) --> "od", reg(0+"e",NP+g(I),M).
poder(NP+p(_,I),M) --> "od", reg(0+"e",NP+p(_,I),M).

poder(s+1+pp,-) --> "ude".
poder(s+3+pp,-) --> "ôde".
poder( NP+pp,M) --> "ud", reg(0+"e",NP+pp,M), { membro(NP,[s+2,p+_]) }.

poder(NP+pm,M) --> "ud", reg(0+"é",NP+pm,M).
poder(NP+si,M) --> "ud", reg(0+"é",NP+si,M).
poder(NP+sf,M) --> "ud", reg(0+"é",NP+sf,M).

%________aprazer/comprazer

aprazer(s+3+pr,M) --> "a", z(M).
aprazer( NP+pr,M) --> "az", reg(0+"e",NP+pr,M), { membro(NP,[s+2,p+_]) }.

aprazer(NP+pi    ,M) --> "az", reg(0+"e",NP+pi,M).
aprazer(NP+i(I)  ,M) --> "az", reg(0+"e",NP+i(I),M).
aprazer(NP+g(I)  ,M) --> "az", reg(0+"e",NP+g(I),M).
aprazer(NP+p(_,I),M) --> "az", reg(0+"e",NP+p(_,I),M).

aprazer(_+f,M) --> "a", r(M).
aprazer( NP+fp,+) --> aprazer(_+f,0), fp(NP,M), { \+ M=1 }.
aprazer( NP+fi,+) --> aprazer(_+f,0), fi(NP,M), { \+ M=1 }.

aprazer(s+1+pp,-) --> "uv", "e".
aprazer(s+3+pp,-) --> "ouv", "e".
aprazer( NP+pp,M) --> "ouv", reg(0+"e",NP+pp,M), { membro(NP,[s+2,p+_]) }.

aprazer(NP+pm,M) --> "ouv", reg(0+"é",NP+pm,M).
aprazer(NP+si,M) --> "ouv", reg(0+"é",NP+si,M).
aprazer(NP+sf,M) --> "ouv", reg(0+"é",NP+sf,M).

comprazer(I,M) --> "a", z(M), { I=s+3+pr }.
comprazer(I,M) --> "az", reg(0+"e",I,M), { \+ membro(I,[s+3+pr,s+1+pr,_+sp]) }.

%________dizer

dizer(s+1+pr,M) --> "ig", reg(0+"e",s+1+pr,M).
dizer(s+3+pr,M) --> "i", z(M).
dizer( NP+pr,M) --> "iz", reg(0+"e",NP+pr,M), { membro(NP,[s+2,p+_]) }.

dizer(NP+sp,M) --> "ig", reg(0+"e",NP+sp,M).

dizer(NP+pi  ,M) --> "iz", reg(0+"e",NP+pi,M).
dizer(NP+i(I),M) --> "iz", reg(0+"e",NP+i(I),M).
dizer(NP+g(I),M) --> "iz", reg(0+"e",NP+g(I),M).

dizer( _+p(_,I),+) --> "it", o(I).

dizer(_+f,M) --> "i", r(M).
dizer( NP+fp,+) --> dizer(_+f,0), fp(NP,M), { \+ M=1 }.
dizer( NP+fi,+) --> dizer(_+f,0), fi(NP,M), { \+ M=1 }.

dizer(s+1+pp,-) --> "iss", "e".
dizer(s+3+pp,-) --> "iss", "e".
dizer( NP+pp,M) --> "iss", reg(0+"e",NP+pp,M), { membro(NP,[s+2,p+_]) }.

dizer(NP+pm,M) --> "iss", reg(0+"é",NP+pm,M).
dizer(NP+si,M) --> "iss", reg(0+"é",NP+si,M).
dizer(NP+sf,M) --> "iss", reg(0+"é",NP+sf,M).

%________fazer

fazer(s+1+pr,M) --> "aç", reg(0+"e",s+1+pr,M).
fazer(s+3+pr,M) --> a(a,M), z(M).
fazer( NP+pr,M) --> "az", reg(0+"e",NP+pr,M), { membro(NP,[s+2,p+_]) }.

fazer(NP+sp,M) --> "aç", reg(0+"e",NP+sp,M).

fazer(NP+pi  ,M) --> "az", reg(0+"e",NP+pi,M).
fazer(NP+i(I),M) --> "az", reg(0+"e",NP+i(I),M).
fazer(NP+g(I),M) --> "az", reg(0+"e",NP+g(I),M).

fazer( _+p(_,I),+) --> "eit", o(I).

fazer(_+f,M) --> a(a,M), r(M).
fazer( NP+fp,+) --> fazer(_+f,0), fp(NP,M), { \+ M=1 }.
fazer( NP+fi,+) --> fazer(_+f,0), fi(NP,M), { \+ M=1 }.

fazer(s+1+pp,M) --> "i", z(M).
fazer(s+3+pp,M) --> e(f,M), z(M).
fazer( NP+pp,M) --> "iz", reg(0+"e",NP+pp,M), { membro(NP,[s+2,p+_]) }.

fazer(NP+pm,M) --> "iz", reg(0+"é",NP+pm,M).
fazer(NP+si,M) --> "iz", reg(0+"é",NP+si,M).
fazer(NP+sf,M) --> "iz", reg(0+"é",NP+sf,M).

%________trazer

trazer(s+1+pr,M) --> "ag", reg(0+"e",s+1+pr,M).
trazer(s+3+pr,M) --> a(a,M), z(M).
trazer( NP+pr,M) --> "az", reg(0+"e",NP+pr,M), { membro(NP,[s+2,p+_]) }.

trazer(NP+sp,M) --> "ag", reg(0+"e",NP+sp,M).

trazer(NP+pi    ,M) --> "az", reg(0+"e",NP+pi,M).
trazer(NP+i(I)  ,M) --> "az", reg(0+"e",NP+i(I),M).
trazer(NP+g(I)  ,M) --> "az", reg(0+"e",NP+g(I),M).
trazer(NP+p(_,I),M) --> "az", reg(0+"e",NP+p(_,I),M).

trazer(_+f,M) --> a(a,M), r(M).
trazer( NP+fp,+) --> trazer(_+f,0), fp(NP,M), { \+ M=1 }.
trazer( NP+fi,+) --> trazer(_+f,0), fi(NP,M), { \+ M=1 }.

trazer(s+1+pp,-) --> "oux", "e".
trazer(s+3+pp,-) --> "oux", "e".
trazer( NP+pp,M) --> "oux", reg(0+"e",NP+pp,M), { membro(NP,[s+2,p+_]) }.

trazer(NP+pm,M) --> "oux", reg(0+"é",NP+pm,M).
trazer(NP+si,M) --> "oux", reg(0+"é",NP+si,M).
trazer(NP+sf,M) --> "oux", reg(0+"é",NP+sf,M).


%__________________________fim de verbos.pl_______________________________
