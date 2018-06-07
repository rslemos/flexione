
%------------------------------------------------------------------------
%
%  Universidade Federal do Rio Grande do Sul
%  Instituto de Inform�tica Te�rica
%  Bacharelado em Ci�ncias da Computa��o
%  Aluno: Emiliano Gomes Padilha
%
%  Projeto de Gradua��o			Semestre 93/2
%  T�tulo: Conjuga��o e Composi��o Verbal da L�ngua Portuguesa
%  �rea: Intelig�ncia Artificial
%  Campo: Processamento de L�ngua Natural
%  Orientadora: Prof. Rosa Maria Viccari
%
%  CoVer - Conjuga��o e Composi��o Verbal da L�ngua Portuguesa
%------------------------------------------------------------------------
% Nota: este arquivo possui caracteres acentuados da tabela de caracteres
% do SunOS. � importante que os acentos sejam vis�veis, pois o programa de
% verbos gera palavras portuguesas (naturalmente) acentuadas. Corrija
% atrav�s de um editor de textos (recurso de busca-e-troca) com os
% caracteres da tabela do seu programa/sistema operacional.
% Os caracteres acentuados utilizados aqui s�o:
%            �  -  a com acento agudo
%            �  -  e com acento agudo
%            �  -  i com acento agudo
%            �  -  o com acento agudo
%            �  -  u com acento agudo
%            �  -  a com til
%            �  -  o com til
%            �  -  e com acento circunflexo
%            �  -  c cedilha
%            �  -  A com acento agudo
%            �  -  E com acento agudo
%--------------------------------------------------------------------------


conc([],L,L).
conc([X|L1],L2,[X|L3]):- conc(L1,L2,L3).

inverte([X|L1],L2,L3):- inverte(L1,[X|L2],L3).
inverte([],L,L).

%------------------------------------------------------------------------
% verbo(): mostra conjuga��es de verbos, locu��es, c/ pronomes �tonos, etc.

pronome(0,_+s+1,"(eu)    ").
pronome(_,_+s+2,"(tu)    ").
pronome(0,m+s+3,"(ele)   ").
pronome(0,f+s+3,"(ela)   ").
pronome(1,_+s+3,"(voc�)  ").
pronome(_,_+p+1,"(n�s)   ").
pronome(_,_+p+2,"(v�s)   ").
pronome(0,m+p+3,"(eles)  ").
pronome(0,f+p+3,"(elas)  ").
pronome(1,_+p+3,"(voc�s) ").

tempo(0,mi,pr,   'Presente').
tempo(0,mi,pp,   'Pret�rito perfeito').
tempo(0,mi,pi,   'Pret�rito imperfeito').
tempo(0,mi,pm,   'Pret�rito mais-que-perfeito').
tempo(0,mi,fp,   'Futuro do presente').
tempo(0,mi,fi,   'Futuro do pret�rito').
tempo(0,ms,sp,   'Presente').
tempo(0,ms,si,   'Pret�rito imperfeito').
tempo(0,ms,sf,   'Futuro do pret�rito').
tempo(0,fn,i(f), 'Infinitivo pessoal').
tempo(i,fn,i(i), 'Infinitivo impessoal').
tempo(i,fn,g(-), 'Ger�ndio').
tempo(i,fn,p(_,_), 'Partic�pio').
tempo(1,mp,imp(a), 'Imperativo afirmativo').
tempo(1,mp,imp(n), 'Imperativo negativo').

modo(mi,'Modo Indicativo').
modo(ms,'Modo Subjuntivo').
modo(mp,'Modo Imperativo').
modo(fn,'Formas Nominais').

w(L,N):- w(L), length(L,W), T is N-W, (T>0, tab(T) ; true), !.
w([C|R]):- put(C), w(R).
w([]).

pertence(X,C):- membro(X,C), !.

membro(L,P,[L|_],[P|_]).
membro(L,P,[_|LR],[_|PR]):- membro(L,P,LR,PR).

t_w(_,Verbo):- clause(t_w(W),_), !, w(Verbo,W).
t_w(A,Verbo):- length(Verbo,V), W is V+A, assert(t_w(W)), t_w(A,Verbo).
t_w:- retract(t_w(_)), fail.
t_w.

%conjuga(X,T,_,_,_,_,LV):- number(X), \+ lv(X,LV,_+T,_,""), !.
conjuga(X,T,Tempo,NP,G,LPs,LVs):- 
	number(X), nl, tab(2), write(Tempo), 
	pronome(X,G+N+P,Pro), pertence(N+P,NP), 
	nl, tab(3), (T=imp(_)-> (membro([_|Lv],Lp,LVs,LPs) ; w(Pro), fail)
			;        w(Pro), membro([_|Lv],Lp,LVs,LPs) ),
	inicPros(Lp,G+N+P), lv(Lv,N+P+T,Verbo,""), t_w(6,Verbo).
%conjuga(i,T,_,_,_,_,LV):- \+ lv(0,LV,_+T,_,""), !.
conjuga(i,p(R,T),Participio,_,G,LPs,LVs):-
	nl, tab(2), write(Participio), o(T,_,_), 
	nl, tab(3), membro([_|Lv],Lp,LVs,LPs), inicPros(Lp,G+s+3),
	lv(Lv,_+p(R,T),Verbo,""), t_w(2,Verbo).
conjuga(i,T,Tempo,_,G,LPs,LVs):- 
	\+ T=p(_,_), nl, tab(2), write(Tempo), 
	nl, tab(3), membro([_|Lv],Lp,LVs,LPs), inicPros(Lp,G+s+3),
	lv(Lv,_+T,Verbo,""), t_w(2,Verbo).
conjuga(_,_,_,_,_,_,_):- nl, t_w, get0_noecho(_).

% get0_noecho(): predicado espec�fico do Arity/Prolog.
% No C-Prolog (Sun), o resultado � que ele falha (pois n�o est� definido), 
% fazendo com que a conjuga��o se d� sem paradas, o que � o desej�vel.

 get0_noecho(_):- fail.     % vers�o SICStus (e C-Prolog)


inicPros([Pro-P|R],Pv):- pronomes(Pro,P,Pv), inicPros(R,Pv).
inicPros([],_).

pronomes(Pro,P  ,_):- Pro=..['.',N,_], number(N), !, pro(P,_,Pro,"").
pronomes(Pro,P  ,R):- membro(Px,Pro), pronomes(Px,P,R).
pronomes(I+T,I+T,_).
pronomes(o  ,P+o,P).
pronomes(i  ,P+i,P).
pronomes(r  ,P+r,P).

titulo(-):- !.
titulo(+):- tab(4), write('CoVer - Conjuga��o e Composi��o Verbal da L�ngua Portuguesa'), 
	 nl, tab(4), write('(C)1993  Emiliano Gomes Padilha  -  Grupo de IA / UFRGS'), 
	 nl.

numeroPessoa(NP,NP):- var(NP), !.
numeroPessoa(NP,[NP]):- \+ functor(NP,'.',2), !.
numeroPessoa(NPs,NPs).

modosTempos(V,Tipo,T,Tempo):- 
	var(V), !, modosTempos([mi,ms,mp,fn],Tipo,T,Tempo).
modosTempos(0,Tipo,T,Tempo):- 
	!, modosTempos([mi,ms,fn],Tipo,T,Tempo).
modosTempos(MTs,Tipo,T,Tempo):- 
	membro(MT,MTs), modosTempos(MT,Tipo,T,Tempo).
modosTempos(T,Tipo,T,Tempo):- tempo(Tipo,M,T,Tempo), mostraModo(M).
modosTempos(M,Tipo,T,Tempo):- 
	modo(M,Modo), nl, write(Modo), nl, tempo(Tipo,M,T,Tempo).

mostraModo(M):- clause(modo_(M),_), !.
mostraModo(_):- retract(modo_(_)), fail.
mostraModo(M):- asserta(modo_(M)), modo(M,Modo), nl, write(Modo), nl.

av(['!',Verbo|R],[[],Lv|LR],[[],[P|Rp]|RR]):-
	!, avPro(Verbo,V,P), av(R,[Rv|LR],[Rp|RR]), conc(V,Rv,Lv).
av([a,Verbo|R],[Lv|LR],[[P|Rp]|RR]):-
	!, avPro(Verbo,[_+i(i)|V],P), av(R,[Rv|LR],[Rp|RR]),
	conc([_+g(i)|V],Rv,Lv).
av([Verbo|R],[Lv|LR],[[P|Rp]|RR]):- 
	!, avPro(Verbo,V,P), av(R,[Rv|LR],[Rp|RR]), conc(V,Rv,Lv).
av([],[[]],[[]]):- !.
av(Verbo,[Lv],[[P]]):- avPro(Verbo,Lv,P).

avPro(Pro-Verbo,LV,Pro-P):- 
	pronomes(Pro,_,_), !, avVerbo(Verbo,V), conc(V,[p(P)],LV).
avPro(Verbo-Pro,LV,Pro-P):- 
	pronomes(Pro,_,_), !, avVerbo(Verbo,V), conc(V,[e(P)],LV).
avPro(Verbo,LV,[o]-_):- 
	avVerbo(Verbo,V), conc(V,[-],LV).

avVerbo(Verbo,[_+i(i),Verbo]):- atomic(Verbo), clause(vrb(Verbo,_,_,_,_),_), !.
avVerbo(Verbo,[_+g(-),V]):- vrb(V,_+g(-),-,Verbo,""), !.
avVerbo(Verbo,[_+p(_,J),V]):- vrb(V,_+p(_,J),+,Verbo,""), !.
avVerbo(Verbo,[_+i(i),V]):- vrb(V,_+i(i),0,Verbo,"").

verbo(_,_,_,_,_):- retract(modo_(_)), fail.
verbo(Vx,Mx,Px,Gx,Titulo):-
	titulo(Titulo), av(Vx,LV,LP), numeroPessoa(Px,NP), 
	modosTempos(Mx,Tipo,T,Tempo), t_w, 
	conjuga(Tipo,T,Tempo,NP,Gx,LP,LV), fail.
verbo(_,_,_,_,_).

verbo(Vx,Mx,Px,Gx):- verbo(Vx,Mx,Px,Gx,+).
verbo(Vx,Mx,Px):- verbo(Vx,Mx,Px,m).
verbo(Vx,Mx):- verbo(Vx,Mx,_).
verbo(Vx):- verbo(Vx,_).
verbo:- nl, nl, titulo(+), nl.

%------------------------------------------------------------------------
%------------------------------------------------------------------------
% conjuga.

conjuga:- repeat, write('

Entre com o infinitivo dos verbos a conjugar (separe-os por v�rgulas e coloque ponto-final: exemplos ''ser.'' ''vir,ver.'', etc), ou digite ''fim.'' para encerrar:

'),
        read(V), nl, nl, conjugaVrb(V), V == fim, !.


conjugaVrb(fim):- !.
conjugaVrb(L):- lista(_,V,L), \+ verifVrb(V,V,vrb), !.
conjugaVrb(L):- nl, write('>> Conjuga��o de '), writeVar(L), nl, convVrb(L,V),
        leTempos(Te), convLista(Te,T),
        lePessoa(Pe), convLista(Pe,P), nl, verbo(V,T,P).

lista(+,V,(V,_)).
lista(O,V,(_,L)):- lista(O,V,L).
lista(-,V,V):- \+ V = (_,_).

convVrb((V1,V2),[V1,!,V2]):- \+ V2 = (_,_), !.
convVrb((V,L),[V,!|L2]):- !, convVrb(L,L2).
convVrb(V,[V]).

convLista(V,W):- var(V), var(W), !.
convLista((V,L),[V|L2]):- convLista(L,L2).
convLista(V,[V]):- \+ V = (_,_).


verifVrb(VP,V,T):- vrbExiste(V,Cham,T), !, lista(-,C,Cham), verifVrb2(VP,C).
verifVrb(VP,V,T):- nl, falaVrbInexiste(VP,V), pergAprende, aprende(V,T).

verifVrb2(VP,vrb(V,_,_,_,_)):- !, verifVrb(VP,V,vrb).
verifVrb2(VP,vrb_(V,_,_,_,_)):- !, verifVrb(VP,V,vrb_).
verifVrb2(_,_).

vrbExiste(V,Cham,Tipo):-
        findall(C1,clause(vrb(V,_,_,_,_),C1),LC1),
        findall(C2,clause(vrb_(V,_,_,_,_),C2),LC2),
	verifRepet(V,Cham,Tipo,LC1,LC2).

verifRepet(_,Cham,vrb,[Cham],[]):- !.
verifRepet(_,Cham,vrb_,[],[Cham]):- !.
verifRepet(V,Cham,vrb,[Cham|_],[]):- !,
        write('**Aviso! H� mais de uma cl�usula vrb/5 para '''), write(V),
        write(''' em Basev! Deve haver apenas uma para cada verbo.').
verifRepet(V,Cham,vrb_,[],[Cham|_]):- !,
        write('**Aviso! H� mais de uma cl�usula vrb_/5 para '''), write(V),
        write(''' em Basev! Deve haver apenas uma para cada verbo.').
verifRepet(V,Cham,vrb,[Cham|_],[_|_]):-
        write('**Aviso! '''), write(V), write(''' est� definido com '),
        write('cl�usulas vrb/5 e vrb_/5 simultaneamente em Basev. '),
        write('Deve haver apenas uma cl�usula para cada verbo.').

falaVrbInexiste(V,V):- !, write('**Aten��o! Verbo '''), write(V),
        write(''' n�o est� definido em Basev.').
falaVrbInexiste(VP,V):- write('**Aten��o! Verbo '''), write(V),
        write(''', do qual '''), write(VP),
        write(''' deriva, n�o est� definido em Basev.').

pergAprende:- write(' Deseja aprend�-lo ? (s./n.) '), read(s).

%--------------

writeVar(V):- var(V), !, write('_').
writeVar(T):- atomic(T), !, write(T).
writeVar(A+B):- !, writeVar(A), write(+), writeVar(B).
writeVar(A-B):- !, writeVar(A), write(-), writeVar(B).
writeVar((A,B)):- !, writeVar(A), write(','), writeVar(B).
writeVar([N|L]):- number(N), N > 31, N < 256, !,
        write('"'), writel([N|L]), write('"').
writeVar(E):- E =.. [F|L], write(F), write('('),
              convLista(LA,L), writeVar(LA), write(')'). 

writel([]).
writel([C|R]):- put(C), writel(R).

%--------------

leTempos(T):- nl, write('Entre com (i.)nfo, (d.)efault {'),
        tempoAt(Td), writeVar(Td), write('} ou os Tempos/modos a conjugar: '),
        read(E), execTempos(E,T), nl.

execTempos(E,T):- E == i, !, tInfoTempo, leTempos(T).
execTempos(E,T):- E == d, !, tempoAt(T).
execTempos(L,_):- nonvar(L), lista(_,I,L),
   \+ membro(I,[mi,ms,mp,fn,pr,pp,pi,pm,fp,fi,sp,si,sf,imp(a),imp(n),g(_),
                p(r,m+s),p(r,m+p),p(r,f+s),p(r,f+p),
                p(i,m+s),p(i,m+p),p(i,f+s),p(i,f+p),i(i),i(f)]),
   !, write('**Erro! "'), writeVar(I), write('" n�o � um tempo/modo v�lido. '),
   write('Veja os c�digos v�lidos em (i.)nfo.'), nl, fail.
execTempos(_,_):- retract(tempo_At(_)), fail.
execTempos(T,T):- asserta(tempo_At(T)).

tInfoTempo:- write('

C�digos para os tempos/formas verbais:
  pr, pp, pi, pm, fp, fi (tempos do indicativo)
  sp, si, sf             (tempos do subjuntivo)'), write('
  imp(a), imp(n)         (imperativo afirmativo e negativo)
  g(_), p(_,_), i(_)     (ger�ndio, partic�pio e infinitivos)

C�digos para os modos:'), write('
  mi, ms, mp, fn  (indicativo, subjuntivo, imperativo e formas nominais)

Pode-se especificar v�rios tempos e/ou modos separando-os por v�rgulas:
     ''mi,sp,imp(n).''     ''g(_),ms,mp,pr,pp.''     ''fp,fi,sf.''

!! N�o esquecer de terminar sempre com ponto (".") !!

').

%--------------

:- dynamic(tempo_At/1).   % SICStus, Quintus Prolog, etc.
:- dynamic(pessoa_At/1).

tempoAt(T):- clause(tempo_At(T),_), !.
tempoAt((mi,ms,mp,fn)):- asserta(tempo_At((mi,ms,mp,fn))).

pessoaAt(T):- clause(pessoa_At(T),_), !.
pessoaAt((s+_,p+_)):- asserta(pessoa_At((s+_,p+_))).

%--------------

lePessoa(P):- nl, write('Entre com (i.)nfo, (d.)efault {'),
   pessoaAt(Pd), writeVar(Pd), write('} ou os N�meros/pessoas a mostrar: '),
   read(E), execPessoa(E,P), nl.

execPessoa(E,P):- E == i, !, tInfoPessoa, lePessoa(P).
execPessoa(E,P):- E == d, !, pessoaAt(P).
execPessoa(L,_):- nonvar(L), lista(_,I,L),
   \+ (I=N+P, membro(N,[s,p]), membro(P,[1,2,3])), !, write('**Erro! "'),
   writeVar(I), write('" incorreto. Veja os valores certos em (i.)nfo.'),
   nl, fail.
execPessoa(_,_):- retract(pessoa_At(_)), fail.
execPessoa(P,P):- asserta(pessoa_At(P)).

tInfoPessoa:- write('

Codifica��o para n�mero e pessoa:  N + P
Onde N pode ser  s, p, _     (singular, plural ou ambos), e
     P pode ser  1, 2, 3, _  (1a, 2a, 3a pessoas, ou todas)'), write('

Pode-se colocar v�rios pares n�mero/pessoa separados por v�rgulas:
        ''s+2,p+3.''    ''_+1,p+_.''

').

%--------------

aprende(V,_):- nl, nl, write('Aprendizagem de '), write(V), write(':'), fail.
aprende(V,T):- var(T), !, write('

Este � um verbo normal (a ser definido com vrb/5) ?? (s./n.)
(Se ele for uma raiz primitiva, a ser definida com vrb_/5, informe n.)

'), read(Resp), tipoVerbo(Resp,T), aprende(V,T).

aprende(V,T):- write('

- Ele � derivado de outro verbo (como ''compor'' � derivado de ''p�r'') ??  Se sim, d� o infinitivo desse outro verbo, sen�o coloque ''n.''
'),
        read(VI), deriva(V,VI,T).


tipoVerbo(s,vrb):- !.
tipoVerbo(_,vrb_).


deriva(V,n,T):- !, aprende2(V,T).
deriva(D,V,TD):- prefixo(D,V,Pre), verifVrb(V,V,TV), !,
        derivaCasos(D,TD,V,TV,Pre).

derivaCasos(D,T,ter,_,Pre):- !, ensinaVrb(D,T,ter,Pre).
derivaCasos(D,T,vir,_,Pre):- !, ensinaVrb(D,T,vir,Pre).
derivaCasos(D,T,por,_,Pre):- !, ensinaVrb(D,T,por,Pre).
derivaCasos(D,TD,V,TV,Pre):- ensinaVrb(D,TD,V,TV,Pre).

prefixo(D,V,Pre):- name(D,Dl), name(V,Vl), inverte(Dl,[],Ld),
        inverte(Vl,[],Lv), conc(Lv,Erp,Ld), !, inverte(Erp,[],Pre).
prefixo(D,V,_):- write('

**Aten��o! '''), write(V), write(''' n�o pode ser o originador de '''),
        write(D), write('''!
Verifique sua resposta e tente novamente.
'), fail.



aprende2(V,T):- name(V,V0), altOrto(V0,D1), mutVocalica(D1,DL), !,
        cjg(DL,_+p(r,m+s),+,Part,[]),
        write('

- Seu partic�pio normal (regular) �: '), writel(Part), write('.
Ele possui partic�pio irregular ??
Se sim, entre com essa forma caso o verbo aceite tamb�m a forma regular (no masculino-singular e terminada com ponto-final).

Se sim, mas o verbo aceita apenas um partic�pio irregular (e n�o a forma regular mostrada acima), informe-o **precedido pelo sinal ''-''** (no masculino-singular e com ponto-final).

Sen�o, apenas responda ''n.''.

'),
        read(PI), detParticipio(PI,DL,Nd), ensinaCjg(V,T,Nd).


detParticipio(n,D,D):- !.
detParticipio(-F,D,D-FL):- atom(F), !, name(F,FL).
detParticipio(+F,D,D+FL):- atom(F), !, name(F,FL).
detParticipio( F,D,D+FL):- atom(F), \+ F = s, name(F,FL).
detParticipio(_,_,_):- write('

**Erro! Apenas os seguintes valores seriam v�lidos aqui:
''n.'' se o verbo apenas possui partic�pio regular,
''-<Part.Irregular>.'' para a forma irregular �nica, ou
''<Part.Irregular>.'' para a forma irregular adicional.
Tente ensinar esse verbo novamente.
'), fail.

%--------------

altOrto(VL,D):- findall(D0,defLexica(VL,D0),L), escolhaDef(L,D).

defLexica("guar","gua").
defLexica("guar","g�a"):- !.
defLexica("quar","qua").
defLexica("quar","q�a"):- !.
defLexica("guir","gui").
defLexica("guir","g�i").
defLexica("guir","g�i"):- !.
defLexica("quir","qui").
defLexica("quir","q�i").
defLexica("quir","q�i"):- !.
defLexica([V,R],[V]):- [R]="r", [V]="a", !.
defLexica([V,R],[V]):- [R]="r", [V]="e", !.
defLexica([V,R],[V]):- [R]="r", [V]="i", !.
defLexica([L|R],[L|R2]):- defLexica(R,R2).


escolhaDef([D],D):- !.
escolhaDef([],_):- !, write('

**Erro! A palavra que voc� digitou n�o pode ser o infinitivo de um verbo! Verifique e tente novamente.

'), fail.

escolhaDef(LD,D):- prepTemp(1,LD,LV), convLista(L0,LV), convVrb(L0,LVs),
        write('

- Identifique qual conjuga��o abaixo est� mais correta; se for a primeira, responda com ''1.'', se for a segunda, com ''2.'' e assim por diante:
'),
        verbo(LVs,pr,_,m,-), nl, read(Esc), desfazTemp(Esc,D,LV), \+ D = !.

escolhaDef(_,_):- write('

**Erro! Voce deve informar apenas o n�mero correspondente � conjuga��o correta. Tente ensinar esse verbo novamente.

'), fail.

prepTemp(N,[D|R],[N|R2]):-
        expand_term((vrb(N,I,M) --> cjg(D,I,M)),Claus),
        asserta(Claus), N2 is N+1,
        prepTemp(N2,R,R2).
prepTemp(_,[],[]).

desfazTemp(V,D,[Vrb|R]):- V == Vrb, !, retract((vrb(Vrb,_,_,_,_):- Corpo)),
        lista(-,cjg(D,_,_,_,_),Corpo),
        desfazTemp(!,_,R).
desfazTemp(V,D,[Vrb|R]):- retract((vrb(Vrb,_,_,_,_):- _)),
        desfazTemp(V,D,R).
desfazTemp(_,!,[]).


mutVocalica(D,ND):- convLista(DL,D), lista(-,I,DL), [I]="i", !,
        findall(D0,mutacoesVoc(-,D,D0),LM), escolhaDef(LM,ND).
mutVocalica(D,D).

mutacoesVoc(_,[L,V],[L,V]):- !.
mutacoesVoc(_,[_],[_]):- !, fail.
mutacoesVoc(_,[],[]):- !, fail.
mutacoesVoc(-,[E|R],[S,E|R2]):- [E]="e", [S]="^", mutacoesVoc(+,R,R2).
mutacoesVoc(-,[E|R],[S,W|R2]):- [E]="e", [W]="�", [S]="^", mutacoesVoc(+,R,R2).
mutacoesVoc(-,[O|R],[S,O|R2]):- [O]="o", [S]="^", mutacoesVoc(+,R,R2).
mutacoesVoc(-,[U|R],[S,U|R2]):- [U]="u", [S]="^", mutacoesVoc(+,R,R2).
mutacoesVoc(F,[L|R],[L|R2]):- mutacoesVoc(F,R,R2).

%--------------

ensinaCjg(V,T,DL):- write('

Aprendizagem de '''), write(V), write(''' conclu�da.
>> Regra resultante a ser adicionada a Basev:

    '), write(T), write('('), write(V), write(',I,M) --> cjg('),
        writeVar(DL), write(',I,M).


'),
        Cab =.. [T,V,I,M], expand_term((Cab-->cjg(DL,I,M)),Claus),
        assert(Claus).


ensinaVrb(V,T,V2,T2,Pre):- write('

Aprendizagem de '''), write(V), write(''' conclu�da.
>> Regra resultante a ser adicionada a Basev:

    '), write(T), write('('), write(V), write(',I,M) --> '),
        writeVar(Pre), write(', '), write(T2), write('('), write(V2),
        write(',I,M).


'),
        Cab =.. [T,V,I,M], Cham =.. [T2,V2,I,M],
        expand_term((Cab --> Pre, Cham),Claus),
        assert(Claus).


ensinaVrb(V,T,V2,Pre):- write('

Aprendizagem de '''), write(V), write(''' conclu�da.
>> Regra resultante a ser adicionada a Basev:

    '), write(T), write('('), write(V), write(',I,M) --> '),
        writeVar(Pre), write(', '), write(V2), write('(I,M).


'),
        Cab =.. [T,V,I,M], Cham =.. [V2,I,M],
        expand_term((Cab --> Pre, Cham),Claus),
        assert(Claus).

%------------------------------------------------------------------------
