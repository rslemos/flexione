
%------------------------------------------------------------------------
%
%  Universidade Federal do Rio Grande do Sul
%  Instituto de Informática Teórica
%  Bacharelado em Ciências da Computação
%  Aluno: Emiliano Gomes Padilha
%
%  Projeto de Graduação                 Semestre 93/2
%  Título: Conjugação e Composição Verbal da Língua Portuguesa
%  Área: Inteligência Artificial
%  Campo: Processamento de Língua Natural
%  Orientadora: Prof. Rosa Maria Viccari
%
%------------------------------------------------------------------------
% Nota: este arquivo possui caracteres acentuados da tabela de caracteres
% do SunOS. É importante que os acentos sejam visíveis, pois o programa de
% verbos gera palavras portuguesas (naturalmente) acentuadas. Corrija
% através de um editor de textos (recurso de busca-e-troca) com os
% caracteres da tabela do seu programa/sistema operacional.
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



  :- dynamic vrb/5.     % SICStus Prolog, Quintus Prolog
  :- dynamic vrb_/5.    %


%__________________Lista de verbos (definições léxicas)_________________

%________verbos totalmente regulares
vrb(amar        ,I,M) -->  cjg("ama",I,M).
vrb(andar       ,I,M) -->  cjg("anda",I,M).
vrb(continuar   ,I,M) -->  cjg("continua",I,M).
vrb(falar       ,I,M) -->  cjg("fala",I,M).
vrb(mediar      ,I,M) -->  cjg("media",I,M). /*não é como ansiar??*/
vrb(rezar       ,I,M) -->  cjg("reza",I,M).
vrb(tentar      ,I,M) -->  cjg("tenta",I,M).
vrb(usar        ,I,M) -->  cjg("usa",I,M).
vrb_(cordar     ,I,M) -->  cjg("corda",I,M).
vrb_(parar      ,I,M) -->  cjg("para",I,M).

vrb(sustentar   ,I,M) --> "sus",        vrb(tentar,I,M).
vrb(abusar      ,I,M) --> "ab",         vrb(usar,I,M).
vrb(acordar     ,I,M) --> "a",          vrb_(cordar,I,M).
vrb(discordar   ,I,M) --> "dis",        vrb_(cordar,I,M).
vrb(recordar    ,I,M) --> "re",         vrb_(cordar,I,M).
vrb(comparar    ,I,M) --> "com",        vrb_(parar,I,M).
vrb(disparar    ,I,M) --> "dis",        vrb_(parar,I,M).
vrb(preparar    ,I,M) --> "pre",        vrb_(parar,I,M).
% (aparar, deparar, reparar ...)

vrb(bater       ,I,M) -->  cjg("bate",I,M).
vrb(correr      ,I,M) -->  cjg("corre",I,M).
vrb(dever       ,I,M) -->  cjg("deve",I,M).
vrb(viver       ,I,M) -->  cjg("vive",I,M).
vrb_(ceber      ,I,M) -->  cjg("cebe",I,M).

vrb(combater    ,I,M) --> "com",        vrb(bater,I,M).
vrb(concorrer   ,I,M) --> "con",        vrb(correr,I,M).
vrb(percorrer   ,I,M) --> "per",        vrb(correr,I,M).
vrb(socorrer    ,I,M) --> "so",         vrb(correr,I,M).
% (acorrer, decorrer, discorrer, escorrer, incorrer,
%  recorrer, transcorrer, ...)
vrb(sobreviver  ,I,M) --> "sobre",      vrb(viver,I,M).
vrb(conceber    ,I,M) --> "con",        vrb_(ceber,I,M).
vrb(perceber    ,I,M) --> "per",        vrb_(ceber,I,M).
vrb(receber     ,I,M) --> "re",         vrb_(ceber,I,M).

vrb(existir     ,I,M) -->  cjg("existi",I,M).
vrb(fundir      ,I,M) -->  cjg("fundi",I,M).
vrb(partir      ,I,M) -->  cjg("parti",I,M).
vrb(possuir     ,I,M) -->  cjg("possui",I,M).
vrb_(sistir     ,I,M) -->  cjg("sisti",I,M).
vrb_(struir     ,I,M) -->  cjg("strui",I,M).

vrb(coexistir   ,I,M) --> "co",         vrb(existir,I,M).
vrb(confundir   ,I,M) --> "con",        vrb(fundir,I,M).
vrb(difundir    ,I,M) --> "di",         vrb(fundir,I,M).
vrb(desistir    ,I,M) --> "de",         vrb_(sistir,I,M).
vrb(insistir    ,I,M) --> "in",         vrb_(sistir,I,M).
vrb(resistir    ,I,M) --> "re",         vrb_(sistir,I,M).
% (assistir, consistir, persistir, subsistir, ...)
vrb(instruir    ,I,M) --> "in",         vrb_(struir,I,M).
vrb(obstruir    ,I,M) --> "ob",         vrb_(struir,I,M).
% (construir e destruir têm alterações ortográficas específicas)



%________os 20(!!) verbos (muito) irregulares
vrb(dar         ,I,M) --> "d",    dar(I,M).
vrb(estar       ,I,M) --> "est",  estar(I,M).
vrb(ser         ,I,M) -->         ser(I,M).
vrb(haver       ,I,M) --> "h",    haver(I,M).
vrb(ter         ,I,M) --> "t",    ter(0,I,M).
vrb(ver         ,I,M) --> "v",    ver(0,I,M).
vrb(vir         ,I,M) --> "v",    vir(0,I,M).
vrb(ir          ,I,M) -->         ir(I,M).
vrb(rir         ,I,M) --> "r",    rir(0,I,M).
vrb(por         ,I,M) --> "p",    por(1,I,M).
vrb(ler         ,I,M) --> "l",    xxer(I,M).
vrb(crer        ,I,M) --> "cr",   xxer(I,M).
vrb(saber       ,I,M) --> "s",    xaber(1,I,M).
vrb(caber       ,I,M) --> "c",    xaber(0,I,M).
vrb(querer      ,I,M) --> "qu",   querer(0,I,M).
vrb(poder       ,I,M) --> "p",    poder(I,M).
vrb(dizer       ,I,M) --> "d",    dizer(I,M).
vrb(fazer       ,I,M) --> "f",    fazer(I,M).
vrb(trazer      ,I,M) --> "tr",   trazer(I,M).
vrb(aprazer     ,I,M) --> "apr",  aprazer(I,M).

%________derivados destes verbos (muito) irregulares
vrb(transir     ,I,M) --> "trans",      transir(I,M).
vrb(comprazer   ,I,M) --> "compr",      comprazer(I,M).
vrb(sorrir      ,I,M) --> "sorr",       rir(1,I,M).
vrb(requerer    ,I,M) --> "requ",       querer(1,I,M).
vrb(prover      ,I,M) --> "prov",       ver(1,I,M).
% (derivados de ver, exceto prover, definidos normalmente)
vrb(antever     ,I,M) --> "ante",       vrb(ver,I,M).
vrb(entrever    ,I,M) --> "entre",      vrb(ver,I,M).
vrb(prever      ,I,M) --> "pre",        vrb(ver,I,M).
vrb(rever       ,I,M) --> "re",         vrb(ver,I,M).
% (outros derivados de ver ??)
vrb(conter      ,I,M) --> "con",        ter(I,M).
vrb(deter       ,I,M) --> "de",         ter(I,M).
vrb(manter      ,I,M) --> "man",        ter(I,M).
vrb(obter       ,I,M) --> "ob",         ter(I,M).
% (abster, ater, entreter, reter, ...)
vrb(compor      ,I,M) --> "com",        por(I,M).
vrb(decompor    ,I,M) --> "de",         vrb(compor,I,M).
vrb(dispor      ,I,M) --> "dis",        por(I,M).
vrb(indispor    ,I,M) --> "in",         vrb(dispor,I,M).
vrb(predispor   ,I,M) --> "pre",        vrb(dispor,I,M).
vrb(expor       ,I,M) --> "ex",         por(I,M).
vrb(opor        ,I,M) --> "o",          por(I,M).
vrb(supor       ,I,M) --> "su",         por(I,M).
% (antepor, apor, contrapor, depor, entrepor, impor, interpor,
%  justapor, propor, repor, sobrepor, transpor, ...)
vrb(avir        ,I,M) --> "a",          vir(I,M).
vrb(desavir     ,I,M) --> "des",        vrb(avir,I,M).
vrb(convir      ,I,M) --> "con",        vir(I,M).
vrb(provir      ,I,M) --> "pro",        vir(I,M).
% (advir, intervir, sobrevir, ...)
vrb(condizer    ,I,M) --> "con",        vrb(dizer,I,M).
vrb(contradizer ,I,M) --> "contra",     vrb(dizer,I,M).
vrb(predizer    ,I,M) --> "pre",        vrb(dizer,I,M).
% (bendizer, desdizer, maldizer, ...)
vrb(desfazer    ,I,M) --> "des",        vrb(fazer,I,M).
vrb(satisfazer  ,I,M) --> "satis",      vrb(fazer,I,M).
% (perfazer, refazer, ...)
vrb(descrer     ,I,M) --> "des",        vrb(crer,I,M).
vrb(reler       ,I,M) --> "re",         vrb(ler,I,M).



%________verbos com alterações ortográficas regulares
vrb(doer        ,I,M) -->  cjg("doe",I,M).
vrb(moer        ,I,M) -->  cjg("moe",I,M).
vrb(roer        ,I,M) -->  cjg("roe",I,M).

vrb(doar        ,I,M) -->  cjg("doa",I,M).
vrb(magoar      ,I,M) -->  cjg("magoa",I,M).
vrb(voar        ,I,M) -->  cjg("voa",I,M).
vrb(perdoar     ,I,M) --> "per",        vrb(doar,I,M).

vrb(basear      ,I,M) -->  cjg("basea",I,M).
vrb(golpear     ,I,M) -->  cjg("golpea",I,M).
vrb(passear     ,I,M) -->  cjg("passea",I,M).
vrb(estrear     ,I,M) -->  cjg("estréa",I,M).
% formas rizotônicas acentuadas: indicar em cjg/5 (estrear)

vrb(cair        ,I,M) -->  cjg("cai",I,M).
vrb(esvair      ,I,M) -->  cjg("esvai",I,M).
vrb(sair        ,I,M) -->  cjg("sai",I,M).
vrb(trair       ,I,M) -->  cjg("trai",I,M).
vrb(atrair      ,I,M) --> "a",          vrb(trair,I,M).
vrb(distrair    ,I,M) --> "dis",        vrb(trair,I,M).
vrb(extrair     ,I,M) --> "ex",         vrb(trair,I,M).
% (abstrair, contrair, retrair, subtrair, ...)

vrb(averiguar   ,I,M) -->  cjg("averigúa",I,M).
vrb(obliquar    ,I,M) -->  cjg("obliqúa",I,M).
vrb(adequar     ,I,M) -->  cjg("adeqúa",I,M).
% verbos quar/guar: formas rizotônicas acentuadas: "qúa"/"gúa"
% senão deve-se colocar "qua"/"gua" (ver aguar/minguar)

vrb(arguir      ,I,M) -->  cjg("argúi",I,M).
vrb(redarguir   ,I,M) -->  cjg("redargui",I,M).
% verbos qüir/güir: formas rizotônicas acentuadas: "qúi"/"gúi"
% senão deve-se colocar "qüi"/"güi"

vrb(ficar       ,I,M) -->  cjg("fica",I,M).
vrb(pescar      ,I,M) -->  cjg("pesca",I,M).
vrb(tocar       ,I,M) -->  cjg("toca",I,M).
vrb_(plicar     ,I,M) -->  cjg("plica",I,M).
vrb(aplicar     ,I,M) --> "a",          vrb_(plicar,I,M).
vrb(explicar    ,I,M) --> "ex",         vrb_(plicar,I,M).
vrb(implicar    ,I,M) --> "im",         vrb_(plicar,I,M).
% (complicar, replicar, suplicar, ...)

vrb(alcancar    ,I,M) -->  cjg("alcança",I,M).
vrb(cacar       ,I,M) -->  cjg("caça",I,M).
vrb(comecar     ,I,M) -->  cjg("começa",I,M).
vrb(forcar      ,I,M) -->  cjg("força",I,M).

vrb(conhecer    ,I,M) -->  cjg("conhece",I,M).
vrb(crescer     ,I,M) -->  cjg("cresce",I,M).
vrb(parecer     ,I,M) -->  cjg("parece",I,M).
vrb(permanecer  ,I,M) -->  cjg("permanece",I,M).
vrb_(quecer     ,I,M) -->  cjg("quece",I,M).
vrb(acrescer    ,I,M) --> "a",          vrb(crescer,I,M).
vrb(decrescer   ,I,M) --> "de",         vrb(crescer,I,M).
vrb(aparecer    ,I,M) --> "a",          vrb(parecer,I,M).
vrb(aquecer     ,I,M) --> "a",          vrb_(quecer,I,M).
vrb(esquecer    ,I,M) --> "es",         vrb_(quecer,I,M).

vrb(carregar     ,I,M) -->  cjg("carrega",I,M).
vrb(julgar       ,I,M) -->  cjg("julga",I,M).
vrb(negar        ,I,M) -->  cjg("nega",I,M).
vrb(rogar        ,I,M) -->  cjg("roga",I,M).
vrb(descarregar  ,I,M) --> "des",        vrb(carregar,I,M).
vrb(encarregar   ,I,M) --> "en",         vrb(carregar,I,M).
vrb(recarregar   ,I,M) --> "re",         vrb(carregar,I,M).
vrb(sobrecarregar,I,M) --> "sobre",      vrb(carregar,I,M).
vrb(renegar      ,I,M) --> "re",         vrb(negar,I,M).

vrb(erguer      ,I,M) -->  cjg("ergue",I,M).
vrb(reerguer    ,I,M) --> "re",         vrb(erguer,I,M).
vrb(soerguer    ,I,M) --> "so",         vrb(erguer,I,M).

vrb(proteger    ,I,M) -->  cjg("protege",I,M).
vrb(reger       ,I,M) -->  cjg("rege",I,M).
vrb(desproteger ,I,M) --> "des",        vrb(proteger,I,M).

vrb(agir        ,I,M) -->  cjg("agi",I,M).
vrb(dirigir     ,I,M) -->  cjg("dirigi",I,M).
vrb(fingir      ,I,M) -->  cjg("fingi",I,M).
vrb(interagir   ,I,M) --> "inter",      vrb(agir,I,M).
vrb(reagir      ,I,M) --> "re",         vrb(agir,I,M).
vrb(retroagir   ,I,M) --> "retro",      vrb(agir,I,M).

vrb(luzir       ,I,M) -->  cjg("luzi",I,M).
vrb_(duzir      ,I,M) -->  cjg("duzi",I,M).
vrb(reluzir     ,I,M) --> "re",         vrb(luzir,I,M).
vrb(conduzir    ,I,M) --> "con",        vrb_(duzir,I,M).
vrb(induzir     ,I,M) --> "in",         vrb_(duzir,I,M).
vrb(produzir    ,I,M) --> "pro",        vrb_(duzir,I,M).
vrb(traduzir    ,I,M) --> "tra",        vrb_(duzir,I,M).
% (aduzir, deduzir, introduzir, reduzir, seduzir, ...)


%________alterações ortográficas específicas de alguns verbos
vrb(ansiar      ,I,M) -->  cjg("ans/ia",I,M).
vrb(incendiar   ,I,M) -->  cjg("incend/ia",I,M).
vrb(odiar       ,I,M) -->  cjg("od/ia",I,M).

vrb(construir   ,I,M) -->  cjg("constr/ui",I,M).
vrb(destruir    ,I,M) -->  cjg("destr/ui",I,M).
vrb(reconstruir ,I,M) --> "re",         vrb(construir,I,M).

vrb(ouvir       ,I,M) -->  cjg("ou/vi",I,M).
vrb(perder      ,I,M) -->  cjg("per/de",I,M).
vrb(valer       ,I,M) -->  cjg("va/le",I,M).

vrb(medir       ,I,M) -->  cjg("me/di",I,M).
vrb(pedir       ,I,M) -->  cjg("pe/di",I,M).
vrb(comedir     ,I,M) --> "co",         vrb(medir,I,M).
vrb(desmedir    ,I,M) --> "des",        vrb(medir,I,M).
vrb(despedir    ,I,M) --> "des",        vrb(pedir,I,M).
vrb(expedir     ,I,M) --> "ex",         vrb(pedir,I,M).
vrb(impedir     ,I,M) --> "im",         vrb(pedir,I,M).
vrb(desimpedir  ,I,M) --> "des",        vrb(impedir,I,M).



%________verbos com mutação vocálica
vrb(repetir     ,I,M) -->  cjg("rep^eti",I,M).
vrb(seguir      ,I,M) -->  cjg("s^egui",I,M).
vrb(sentir      ,I,M) -->  cjg("s^enti",I,M).
vrb_(vertir     ,I,M) -->  cjg("v^erti",I,M).
vrb(conseguir   ,I,M) --> "con",        vrb(seguir,I,M).
vrb(perseguir   ,I,M) --> "per",        vrb(seguir,I,M).
vrb(prosseguir  ,I,M) --> "pros",       vrb(seguir,I,M).
vrb(advertir    ,I,M) --> "ad",         vrb_(vertir,I,M).
vrb(divertir    ,I,M) --> "di",         vrb_(vertir,I,M).
% (outros semelhantes: aderir, despir, ferir, ..ferir: aferir,
%  conferir, deferir, desferir, inferir, interferir, preferir,
%  proferir, referir, transferir, gerir: digerir, ingerir, sugerir,
%  refletir, servir, vestir, ..vergir: convergir, divergir)

vrb(dormir      ,I,M) -->  cjg("d^ormi",I,M).
vrb(engolir     ,I,M) -->  cjg("eng^oli",I,M).
vrb(tossir      ,I,M) -->  cjg("t^ossi",I,M).

vrb(agredir     ,I,M) -->  cjg("agr^ëdi" ,I,M).
vrb(denegrir    ,I,M) -->  cjg("den^ëgri",I,M).
vrb(prevenir    ,I,M) -->  cjg("prev^ëni",I,M).
vrb_(gredir     ,I,M) -->  cjg("gr^ëdi",I,M).
vrb(progredir   ,I,M) --> "pro",        vrb_(gredir,I,M).
vrb(regredir    ,I,M) --> "re",         vrb_(gredir,I,M).
vrb(transgredir ,I,M) --> "trans",      vrb_(gredir,I,M).

vrb(cuspir      ,I,M) -->  cjg("c^uspi",I,M).
vrb(fugir       ,I,M) -->  cjg("f^ugi",I,M).
vrb(sacudir     ,I,M) -->  cjg("sac^udi",I,M).
vrb(subir       ,I,M) -->  cjg("s^ubi",I,M).
% (outros semelhantes: acudir, bulir, escapulir)


%________verbos com acentuações no radical (nas formas rizotônicas)
vrb(apoiar  ,I,M) -->  cjg("ap^óia",I,M).
vrb(boiar   ,I,M) -->  cjg("b^óia",I,M).
% (provavelmente todos terminados em ..oiar)
vrb(reunir  ,I,M) -->  cjg("re^úni",I,M).
vrb(proibir ,I,M) -->  cjg("pro^íbi",I,M).
vrb(minguar ,I,M) -->  cjg("m^íngua",I,M).
vrb(aguar   ,I,M) -->  cjg("^água",I,M).
vrb(desaguar,I,M) --> "des",            vrb(aguar,I,M).
vrb(enxaguar,I,M) --> "enx",            vrb(aguar,I,M).


%________particularidades de acentuação (casos únicos)
vrb(parar,I,M) --> "p", a(a,K), cjg("ra",I,M), { co(K,I,[s+3+pr]) }.
vrb(pelar,I,M) --> "p", e(a,K), cjg("la",I,M), { co(K,I,[s+_+pr]) }.
vrb(coar, I,M) --> "c", o(f,K), reg(1+"a",I,M), { co(K,I,[s+_+pr]) }.



%________verbos com dois particípios (regular e irregular)
vrb(entregar    ,I,M) --> "entre",   cjg("ga"+"gue",I,M).
vrb(aceitar     ,I,M) --> "acei",    cjg("ta"+"to",I,M).
vrb(expressar   ,I,M) --> "expres",  cjg("sa"+"so",I,M).
vrb(expulsar    ,I,M) --> "expul",   cjg("sa"+"so",I,M).
vrb(fritar      ,I,M) --> "fri",     cjg("ta"+"to",I,M).
vrb(ganhar      ,I,M) --> "gan",     cjg("ha"+"ho",I,M).
vrb(gastar      ,I,M) --> "gas",     cjg("ta"+"to",I,M).
vrb(limpar      ,I,M) --> "lim",     cjg("pa"+"po",I,M).
vrb(salvar      ,I,M) --> "sal",     cjg("va"+"vo",I,M).
vrb(soltar      ,I,M) --> "sol",     cjg("ta"+"to",I,M).
vrb(pagar       ,I,M) --> "pa",      cjg("ga"+"go",I,M).
vrb(pegar       ,I,M) --> "p",       cjg("ega"+"ego",I,M).
vrb(matar       ,I,M) --> "m",       cjg("ata"+"orto",I,M).

vrb(morrer      ,I,M) --> "mor",     cjg("re" +"to",I,M).
vrb(acender     ,I,M) --> "ace",     cjg("nde"+"so",I,M).
vrb(benzer      ,I,M) --> "ben",     cjg("ze" +"to",I,M).
vrb(eleger      ,I,M) --> "ele",     cjg("ge" +"ito",I,M).
vrb(prender     ,I,M) --> "pre",     cjg("nde"+"so",I,M).
vrb(suspender   ,I,M) --> "suspen",  cjg("de" +"so",I,M).
vrb(romper      ,I,M) --> "ro",      cjg("mpe"+"to",I,M).
%% derivados de romper nao aceitam o participio irregular
vrb_(romper     ,I,M) -->            cjg("rompe",I,M).
vrb(corromper   ,I,M) --> "cor",        vrb_(romper,I,M).
vrb(irromper    ,I,M) --> "ir",         vrb_(romper,I,M).
vrb(interromper ,I,M) --> "inter",      vrb_(romper,I,M).

vrb(omitir      ,I,M) --> "omi",     cjg("ti" +"sso",I,M).
vrb(tingir      ,I,M) --> "tin",     cjg("gi" +"to",I,M).
vrb(extinguir   ,I,M) --> "extin",   cjg("gui" +"to",I,M).
vrb(distinguir  ,I,M) --> "distin",  cjg("gui" +"to",I,M).
  %% distinguir ou distingüir ??

vrb_(mergir     ,I,M) --> "mer",     cjg("ge" +"so",I,M).
vrb_(primir     ,I,M) --> "pr",      cjg("imi"+"esso",I,M).
vrb(emergir     ,I,M) --> "e",          vrb_(mergir,I,M).
vrb(imergir     ,I,M) --> "i",          vrb_(mergir,I,M).
vrb(submergir   ,I,M) --> "sub",        vrb_(mergir,I,M).
vrb(exprimir    ,I,M) --> "ex",         vrb_(primir,I,M).
vrb(imprimir    ,I,M) --> "im",         vrb_(primir,I,M).
vrb(suprimir    ,I,M) --> "su",         vrb_(primir,I,M).

vrb(inserir     ,I,M) --> "ins",     cjg("^eri"+"erto",I,M).
vrb(frigir      ,I,M) --> "fr",      cjg("^igi"+"ito",I,M).


%________verbos apenas com particípio irregular
vrb(abrir       ,I,M) --> "ab",      cjg("ri"   -"erto",I,M).
vrb(cobrir      ,I,M) --> "c",       cjg("^obri"-"oberto",I,M).
vrb_(screver    ,I,M) --> "scr",     cjg("eve"  -"ito",I,M).
vrb(entreabrir  ,I,M) --> "entre",      vrb(abrir,I,M).
vrb(descobrir   ,I,M) --> "des",        vrb(cobrir,I,M).
vrb(encobrir    ,I,M) --> "en",         vrb(cobrir,I,M).
vrb(recobrir    ,I,M) --> "re",         vrb(cobrir,I,M).
vrb(descrever   ,I,M) --> "de",         vrb_(screver,I,M).
vrb(escrever    ,I,M) --> "e",          vrb_(screver,I,M).
vrb(inscrever   ,I,M) --> "in",         vrb_(screver,I,M).
% (prescrever, sobrescrever, subscrever, transcrever, ...)



%________verbos defectivos (sem formas dos presentes)
vrb(reaver  ,I,M) --> "re", cjg("ave",I,M),
                                { \+ membro(I,[s+_+pr,p+3+pr,_+sp]) }.
vrb(precaver,I,M) --> cjg("precave",I,M),
                                { \+ membro(I,[s+_+pr,p+3+pr,_+sp]) }.

vrb(abolir   ,I,M) --> cjg("aboli",I,M),
                                  { \+ membro(I,[s+1+pr,_+sp]) }.
vrb(competir ,I,M) --> cjg("competi",I,M),
                                  { \+ membro(I,[s+1+pr,_+sp]) }.
vrb(delinquir,I,M) --> cjg("delinqüi",I,M),
                                  { \+ membro(I,[s+1+pr,_+sp]) }.
vrb(demolir  ,I,M) --> cjg("demoli",I,M),
                                  { \+ membro(I,[s+1+pr,_+sp]) }.
vrb(retorquir,I,M) --> cjg("retorqui",I,M),
                                  { \+ membro(I,[s+1+pr,_+sp]) }.
% (outros defectivos semelhantes: aturdir, brandir, carpir, colorir,
%  delir, discernir, exaurir, extorquir, fulgir, premir, e
%  derivados de ..pelir: compelir, expelir, impelir, propelir, repelir)

vrb(falir   ,I,M) --> cjg("fali",I,M),
                                { \+ membro(I,[s+_+pr,p+3+pr,_+sp]) }.
vrb(florir  ,I,M) --> cjg("flori",I,M),
                                { \+ membro(I,[s+_+pr,p+3+pr,_+sp]) }.
% (outros defectivos semelhantes: aguerrir, combalir, comedir, embair,
%  foragir, remir, renhir, sortir)


%_________________________fim de basev.pl___________________________
