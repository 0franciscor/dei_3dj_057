:-consult('GA_distribute.pl').
:-consult('bc_entrega.pl').
:-consult('bc_armazens.pl').
:-consult('bc_factos_camiao.pl').
:-consult('US1.pl').
:-consult('heuristics_geneticAlgorithm.pl').

:-dynamic geracoes/1.
:-dynamic populacao/1.
:-dynamic prob_cruzamento/1.
:-dynamic prob_mutacao/1.
:-dynamic totalMassOfDeliveries/2.
:-dynamic date/1.
:-dynamic idTruck/1.
:-dynamic tarefas/1.
:-dynamic totalMass/1.
:-dynamic list_trucks/1.

date(20230109).
idTruck(eTruck01).
list_trucks([eTruck01,eTruck02]).


inicializa(ListaEntregas):-
    length(ListaEntregas, N),
    (retract(tarefas(_));true),
    asserta(tarefas(N)),
    (retract(geracoes(_));true),
    N1 is N * 15,
	asserta(geracoes(N1)),
	(retract(populacao(_));true),
	N2 is N * 6,
	asserta(populacao(N2)),
	(retract(prob_cruzamento(_));true),
    asserta(prob_cruzamento(1/2)),
	(retract(prob_mutacao(_));true),
	asserta(prob_mutacao(1/4)).


gera(ListaEntregas,Top):-
	inicializa(ListaEntregas),
	gera_populacao(Pop,ListaEntregas),
	write('Pop='),write(Pop),nl,
	avalia_genes(Pop,PopAv),
	write('PopAv='),write(PopAv),nl,
	ordena_populacao(PopAv,PopOrd),
	geracoes(NG),
	head(PopOrd,First),
	gera_geracao(0,NG,PopOrd,First,0,Top).

calculateWeight([],0):-!.
calculateWeight([H|T],Weight1):-entrega(H,_,Mass,_,_,_), calculateWeight(T,Weight), Weight1 is  Weight+Mass.


gera_populacao(Pop, ListaEntregas):-
	populacao(TamPop),
	tarefas(NumT),
	gera_populacao_heuristicas(ListaEntregas,TamPop,Pop1),
	populacao(TamPop1),
	gera_populacao(TamPop1,ListaEntregas,NumT,Pop1,Pop2),
	merge(Pop1,Pop2,Pop).

gera_populacao_heuristicas(ListaEntregas,TamPop,Populacao):-
	date(Date),
    deliveriesInADay(Date,ListaEntregas,ListaEntregas1),
	calculateWeight(ListaEntregas1,MaxWeight),
	(retract(totalMass(_));true),
    asserta(totalMass(MaxWeight)),
	list_trucks(TruckList),
	tarefas(NumT),
	calculateRacio(TruckList, MaxWeight, NumT),
    largestMassFirst1(ListaEntregas1,WarehouseSorted),
	closestWarehouseFirst1(ListaEntregas1, PATH_LIST),
    cheapestWarehouseFirst1(ListaEntregas1,Result),
    ((\+(WarehouseSorted==PATH_LIST),!, Populacao1=[WarehouseSorted,PATH_LIST]; Populacao1=WarehouseSorted)),
    ((\+member(Result,Populacao1),!, merge1(Populacao1, Result, Populacao)); Populacao=Populacao1),
	retract(populacao(_)),
	length(Populacao, TamPop1),
	asserta(populacao((TamPop-TamPop1))).

merge1([],L,[L]):-!.
merge1([L|LL],L1,[L|T]):- merge1(LL,L1,T).


gera_populacao(0,_,_,_,[]):-!.

gera_populacao(TamPop,ListaTarefas,NumT,Populacao,[Ind|Resto]):-
	TamPop1 is TamPop-1,
	gera_populacao(TamPop1,ListaTarefas,NumT,Populacao,Resto),
	loop_gera_individuo(ListaTarefas,NumT,Ind,Populacao,Resto).

gera_populacao(TamPop,ListaTarefas,NumT,Populacao,L):-
	gera_populacao(TamPop,ListaTarefas,NumT,Populacao,L).

loop_gera_individuo(ListaTarefas,NumT,Ind,Populacao,Resto):-
	(gera_individuo(ListaTarefas,NumT,Ind1), !,
	not(member(Ind1,Populacao)), not(member(Ind1,Resto)), Ind = Ind1);(loop_gera_individuo(ListaTarefas,NumT,Ind,Populacao,Resto)).

gera_individuo([G],1,[G]):-!.

gera_individuo(ListaTarefas,NumT,[G|Resto]):-
	NumTemp is NumT + 1, % To use with random
	random(1,NumTemp,N),
	retira(N,ListaTarefas,G,NovaLista),
	NumT1 is NumT-1,
	gera_individuo(NovaLista,NumT1,Resto).

retira(1,[G|Resto],G,Resto).
retira(N,[G1|Resto],G,[G1|Resto1]):-
	N1 is N-1,
	retira(N1,Resto,G,Resto1).


avalia_populacao([],[]).
avalia_populacao([Ind|Resto],[Ind*V|Resto1]):-
	avalia(Ind,V),
	avalia_populacao(Resto,Resto1).

avalia(Seq,V):-
	date(Date),
	findAllDeliveriesInACity(Date,Seq,ListaEntregas1),
	idTruck(Truck),
	carateristicasCam(Truck,_,Capacity,Batery,_,_),
	calculateWeight(ListaEntregas1,Weight),
	((Weight > Capacity, V=100000);avalia(Seq,Truck,ListaEntregas1,Batery,0,V)).
    

avalia(Seq,Truck,ListaEntregas1,Batery,TotalTime,V):-
	analisePath(Seq,Truck,ListaEntregas1,Batery,TotalTime,V).


ordena_populacao(PopAv,PopAvOrd):-
	bsort(PopAv,PopAvOrd).

bsort([X],[X]):-!.
bsort([X|Xs],Ys):-
	bsort(Xs,Zs),
	btroca([X|Zs],Ys).


btroca([X],[X]):-!.

btroca([X*VX,Y*VY|L1],[Y*VY|L2]):-
	VX>VY,!,
	btroca([X*VX|L1],L2).

btroca([X|L1],[X|L2]):-btroca(L1,L2).

media([],0):-!.
media([_*Valor|Resto], Soma):-
    media(Resto, Soma1),
    Soma is Soma1 + Valor.


avalia_genes([],[]):-!.
avalia_genes([H|T],[H*Max|Resto]):-
    split_genes(H,ResultList),
    avalia_populacao(ResultList,PopAv),
    getMaxTempo(PopAv,Max),
    avalia_genes(T,Resto).


split_genes(L,ResultList):-
    distributeDeliveries(L, ResultList).

getMaxTempo(L,Max):-    
    getTempos(L,L1),
    max(L1,Max).


getTempos([],[]):-!.
getTempos([_*Tempo|T], [Tempo|T1]):-
    getTempos(T,T1).


max([X],X).
max([X|Xs],M):-
    max(Xs,M1),
    (X>M1 -> M=X; M=M1).

gera_geracao(G,G,[Top|_],_,_,Top):-!,
	write('Top '), write(G), write(':'), nl, write(Top), nl.
	

gera_geracao(N,G,Pop,Last,N3,Top):-
	write('Geracao '), write(N), write(':'), nl, write(Pop), nl,
	media(Pop,Media),
    write('Media: '), write(Media), nl,
	length(Pop,TamPop),
	NrMove is ceiling(TamPop*3/10),
	remove_top10(Pop,PopMove,PopAux,NrMove,0),
	random_permutation(PopAux,Pop1),
	cruzamento(Pop1,NPop1),
	mutacao(NPop1,NPop),
	merge(NPop, PopMove, NPop2),
	avalia_genes(NPop2,NPopAv),
	ordena_populacao(NPopAv,NPopOrd),
	N1 is N+1,
	((check_geracao1(NPopOrd,Last), Last1 = Last, N2 is N3 + 1, !)
	;
	(N2 is 0, head(NPopOrd,Last1))),
	((check_geracao2(G,N2), head(NPopOrd,Top), !)
	;
	gera_geracao(N1,G,NPopOrd,Last1,N2,Top)).




check_geracao1([Last|_],Last).
check_geracao2(N,N1):-
	N1>=(ceiling(sqrt(N) * 6)).

head([H|_],H).

remove_top10(L,[],L,N,N):-!.
remove_top10([Ind*_|Resto],[Ind|PopAux],PopF,TamPop,N):-
	N1 is N+1,
	remove_top10(Resto,PopAux,PopF,TamPop,N1), !.

gerar_pontos_cruzamento(P1,P2):-
	gerar_pontos_cruzamento1(P1,P2).

gerar_pontos_cruzamento1(P1,P2):-
	tarefas(N),
	NTemp is N+1,
	random(1,NTemp,P11),
	random(1,NTemp,P21),
	P11\==P21,!,
	((P11<P21,!,P1=P11,P2=P21);(P1=P21,P2=P11)).
gerar_pontos_cruzamento1(P1,P2):-
	gerar_pontos_cruzamento1(P1,P2).


cruzamento([],[]).
cruzamento([Ind*_],[Ind]).
cruzamento([Ind1*_,Ind2*_|Resto],[NInd1,NInd2|Resto1]):-
	gerar_pontos_cruzamento(P1,P2),
	prob_cruzamento(Pcruz),random(0.0,1.0,Pc),
	((Pc =< Pcruz,!,
        cruzar(Ind1,Ind2,P1,P2,NInd1),
	  cruzar(Ind2,Ind1,P1,P2,NInd2))
	;
	(NInd1=Ind1,NInd2=Ind2)),
	cruzamento(Resto,Resto1).

preencheh([],[]).

preencheh([_|R1],[h|R2]):-
	preencheh(R1,R2).


sublista(L1,I1,I2,L):-
	I1 < I2,!,
	sublista1(L1,I1,I2,L).

sublista(L1,I1,I2,L):-
	sublista1(L1,I2,I1,L).

sublista1([X|R1],1,1,[X|H]):-!,
	preencheh(R1,H).

sublista1([X|R1],1,N2,[X|R2]):-!,
	N3 is N2 - 1,
	sublista1(R1,1,N3,R2).

sublista1([_|R1],N1,N2,[h|R2]):-
	N3 is N1 - 1,
	N4 is N2 - 1,
	sublista1(R1,N3,N4,R2).

rotate_right(L,K,L1):-
	tarefas(N),
	T is N - K,
	rr(T,L,L1).

rr(0,L,L):-!.

rr(N,[X|R],R2):-
	N1 is N - 1,
	append(R,[X],R1),
	rr(N1,R1,R2).


elimina([],_,[]):-!.

elimina([X|R1],L,[X|R2]):-
	not(member(X,L)),!,
	elimina(R1,L,R2).

elimina([_|R1],L,R2):-
	elimina(R1,L,R2).

insere([],L,_,L):-!.
insere([X|R],L,N,L2):-
	tarefas(T),
	((N>T,!,N1 is N mod T);N1 = N),
	insere1(X,N1,L,L1),
	N2 is N + 1,
	insere(R,L1,N2,L2).


insere1(X,1,L,[X|L]):-!.
insere1(X,N,[Y|L],[Y|L1]):-
	N1 is N-1,
	insere1(X,N1,L,L1).

cruzar(Ind1,Ind2,P1,P2,NInd11):-
	sublista(Ind1,P1,P2,Sub1),
	tarefas(NumT),
	R is NumT-P2,
	rotate_right(Ind2,R,Ind21),
	elimina(Ind21,Sub1,Sub2),
	P3 is P2 + 1,
	insere(Sub2,Sub1,P3,NInd1),
	eliminah(NInd1,NInd11).


eliminah([],[]).

eliminah([h|R1],R2):-!,
	eliminah(R1,R2).

eliminah([X|R1],[X|R2]):-
	eliminah(R1,R2).

mutacao([],[]).
mutacao([Ind|Rest],[NInd|Rest1]):-
	prob_mutacao(Pmut),
	random(0.0,1.0,Pm),
	((Pm < Pmut,!,mutacao1(Ind,NInd));NInd = Ind),
	mutacao(Rest,Rest1).

mutacao1(Ind,NInd):-
	gerar_pontos_cruzamento(P1,P2),
	mutacao22(Ind,P1,P2,NInd).

mutacao22([G1|Ind],1,P2,[G2|NInd]):-
	!, P21 is P2-1,
	mutacao23(G1,P21,Ind,G2,NInd).
mutacao22([G|Ind],P1,P2,[G|NInd]):-
	P11 is P1-1, P21 is P2-1,
	mutacao22(Ind,P11,P21,NInd).

mutacao23(G1,1,[G2|Ind],G2,[G1|Ind]):-!.
mutacao23(G1,P,[G|Ind],G2,[G|NInd]):-
	P1 is P-1,
	mutacao23(G1,P1,Ind,G2,NInd).



