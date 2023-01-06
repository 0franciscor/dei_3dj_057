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
:- dynamic final_list/1.



%% FIND ALL DESTINATIONS OF DELIVERIES %%

findAllDestination(Date, Result):-
    findall(Destination, entrega(_,Date,_,Destination,_,_), Destinations1),
    removeDuplicates(Destinations1, Destinations),
    delete([], Destinations, Result).

removeDuplicates([],[]):-!.
removeDuplicates([H|T],[H1|L]):- removeDuplicates(T,L), ((\+member(H,L), H1 is H); H1=[]).


%% INTERSECTION BETWEEN THE DELIVERIES DESTINATIONS AND THE CITIES %%
citiesToVisite(Date):-
    findAllDestination(Date, Destinations),
    sumMassOfDeliveriesWithSameDestination(Destinations).

%% group deliveries that have the same destination %%

sumMassOfDeliveriesWithSameDestination([]):-!.
sumMassOfDeliveriesWithSameDestination([H|T]):- sumMassOfDeliveriesWithSameDestination(T),sumMassOfDeliveriesWithSameDestination1(H).

sumMassOfDeliveriesWithSameDestination1(Destination):-
    findall(Mass, entrega(_,_,Mass,Destination,_,_), Masses),
    sum_list(Masses, Sum),
    assert(totalMassOfDeliveries(Destination,Sum)).

sum_list([],0).
sum_list([H|T],Sum):- sum_list(T,Sum1), Sum is Sum1+H.

%----------------------------------------------------------------------------------------%

% tarefa(Id,TempoProcessamento,TempConc,PesoPenalizacao).
tarefa(t1,2,5,1).
tarefa(t2,4,7,6).
tarefa(t3,1,11,2).
tarefa(t4,3,9,3).
tarefa(t5,3,8,2).

% tarefas(NTarefas).

date(20221205).
idTruck(eTruck01).


% parameteriza��o
inicializa(ListaEntregas):-
    length(ListaEntregas, N),
    (retract(tarefas(_));true),
    asserta(tarefas(N)),
    (retract(geracoes(_));true),
    N1 is N * 3, asserta(geracoes(N1)),
	(retract(populacao(_));true), asserta(populacao(N)),
	(retract(prob_cruzamento(_));true),
    asserta(prob_cruzamento(1/2)),
	(retract(prob_mutacao(_));true), asserta(prob_mutacao(1/4)).


gera(ListaEntregas):-
	inicializa(ListaEntregas),
	gera_populacao(Pop,ListaEntregas),
	write('Pop='),write(Pop),nl,
	avalia_populacao(Pop,PopAv),
	write('PopAv='),write(PopAv),nl,
	ordena_populacao(PopAv,PopOrd),
	geracoes(NG),
	gera_geracao(0,NG,PopOrd).

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
    largestMassFirst(ListaEntregas1,WarehouseSorted),
	closestWarehouseFirst(ListaEntregas1, PATH_LIST),
    cheapestWarehouseFirst(ListaEntregas1,Result),
    ((\+(WarehouseSorted==PATH_LIST),!, Populacao1=[WarehouseSorted,PATH_LIST]; true)),
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
	loop_gera_individuo(ListaTarefas,NumT,Ind,Populacao).

gera_populacao(TamPop,ListaTarefas,NumT,Populacao,L):-
	gera_populacao(TamPop,ListaTarefas,NumT,Populacao,L).

loop_gera_individuo(ListaTarefas,NumT,Ind,Populacao):-
	(gera_individuo(ListaTarefas,NumT,Ind1), !,
	not(member(Ind1,Populacao)), Ind = Ind1);(loop_gera_individuo(ListaTarefas,NumT,Ind,Populacao)).

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
	carateristicasCam(Truck,_,_,Batery,_,_),
	avalia(Seq,Truck,ListaEntregas1,Batery,0,V).


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


gera_geracao(G,G,Pop):-!,
	write('Gera��o '), write(G), write(':'), nl, write(Pop), nl.

gera_geracao(N,G,Pop):-
	write('Gera��o '), write(N), write(':'), nl, write(Pop), nl,
	random_permutation(Pop,Pop1),
	cruzamento(Pop1,NPop1),
	mutacao(NPop1,NPop),
	avalia_populacao(NPop,NPopAv),
	ordena_populacao(NPopAv,NPopOrd),
	N1 is N+1,
	gera_geracao(N1,G,NPopOrd).

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





% ------------------------------------------------------------------------%


distributeDeliveries(TruckList, DeliveryList,Date,SplitList):-
     calculateMaxDLWeight(DeliveryList,Date,Weight),
    calculateMaxTruckCapacity(TruckList,MaxCapacity),
    (  ( MaxCapacity<Weight,!,extraTruck(TruckList,NewTruckList),RealTruckList = NewTruckList);
    RealTruckList = TruckList),

    truckListLength(RealTruckList,0,TLLength), deliveryListLength(DeliveryList,0,DLLength),
    DLPerTruck is DLLength/TLLength,
    extractDestinations(DeliveryList, Destinations),

    loop_split_list(RealTruckList,Destinations,DLPerTruck,SplitList,Date).


    loop_split_list(TruckList,Destinations,DLPerTruck,SplitList,Date):- split_list(Destinations,DLPerTruck,SplitList),dummy_function(TruckList,Destinations,DLPerTruck,SplitList,Date).


dummy_function(TruckList,_,_,SplitList,Date):- compareTtrucksWithDLWeight(TruckList, SplitList, Date,[]),!.
dummy_function(TruckList,_,DLPerTruck,SplitList,Date):- compareVerified(SplitList,RandomDestinations),
      loop_split_list(TruckList,RandomDestinations,DLPerTruck,_,Date).

%compare weights with truck capacity%


truckListLength([],AUX,TLLength):- TLLength is AUX,!.

truckListLength([_|T],AUX,TLLength):-  AUX1 is AUX + 1, truckListLength(T,AUX1,TLLength).


deliveryListLength([],AUX,DLLength):-DLLength is AUX,!.
deliveryListLength([_|T],AUX,DLLength):-AUX1 is AUX + 1,deliveryListLength(T,AUX1,DLLength).

split_list(List, X, Splits) :-
    length(List, Length),
    X_round is round(X),
    split_list_(List, X_round, Length, Splits).

split_list_([], _, _, []) :- !.
split_list_(List, X, Length, [Split|Splits]) :-
    (   Length >= X
    ->  length(Split, X),
        append(Split, Rest, List),
        NewLength is Length - X,
           split_list_(Rest, X, NewLength, Splits)
    ;   Split = List,
        Splits = []
    ).


calculateWeight([],_,0):-!.
calculateWeight([H|T],Date,Weight1):-entrega(_,Date,Mass,H,_,_), calculateWeight(T,Date,Weight), Weight1 is  Weight+Mass.

calculateMaxTruckCapacity([],0):-!.
calculateMaxTruckCapacity([H|T],Weight1):- carateristicasCam(H,_,Capacity,_,_,_),calculateMaxTruckCapacity(T,Weight),Weight1 is Weight + Capacity.

compareWeight(Truck,Weight):- carateristicasCam(Truck,_,Capacity,_,_,_), Weight < Capacity.


compareTtrucksWithDLWeight([],[],_,_):-!.

compareTtrucksWithDLWeight([TH|TT],[SLH|SLT],Date,VerifiedList):-calculateWeight(SLH,Date,Weight),(compareWeight(TH,Weight),add_to_verified_list(SLH,VerifiedList,_)),
    compareTtrucksWithDLWeight(TT,SLT,Date,VerifiedList).


extraTruck(L, NewList):- append(L,[extraETruck],NewList).


calculateMaxDLWeight([],_,0):-!.
calculateMaxDLWeight([H|T],Date,Weight1):-entrega(H,Date,Mass,_,_,_), calculateMaxDLWeight(T,Date,Weight), Weight1 is  Weight+Mass.



add_to_verified_list(TH, VerifiedList, FinalList) :-
    append(VerifiedList, TH, FinalList),
    retractall(final_list(_)),
    asserta(final_list(FinalList)).

get_final_list(FinalList) :-
    final_list(FinalList).



compareVerified(SplitList,RandomDestinations):- get_final_list(FinalList), get_nonmatching_sublists(SplitList, FinalList,Result),
    flatten(Result, FlattenDestinations),
    random_permutation(FlattenDestinations,PermutatedDestinations),!,
    append(FinalList,PermutatedDestinations,RandomDestinations) .



get_nonmatching_sublists(SplitList, FinalList, Result) :-
    findall(Sublist, (member(Sublist, SplitList), member(X, Sublist), member(X, FinalList)), Matching),
    subtract(SplitList, Matching, Result).
