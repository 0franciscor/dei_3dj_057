:-consult('bc_armazens.pl').
:-consult('bc_factos_camiao.pl').
:-consult('bc_entrega.pl').

allPaths(L, LF) :-((checkIfCityExist(L),!,findMatosinhos(M1),deleteMatosinhos(M1,L,L2),findall(LAux,permutation(L2,LAux),L3),appendMatosinhos([M1],L3,LF)); write('One of the entered coordinates does not correspond to a warehouse that is in the system.')).

deleteMatosinhos(A,L,L1):- ((member(A,L),!,delete(L,A,L1)); L1=L).

checkIfCityExist([]):-!.
checkIfCityExist([H|T]):- ((idArmazem(_,H),!,checkIfCityExist(T));false).

appendMatosinhos(_,[],[]):-!.
appendMatosinhos(A,[L|LL],[L2|T]):- appendMatosinhos(A,LL,T), append(A,L,L1), append(L1,A,L2).

findMatosinhos(M):-idArmazem('Matosinhos',M).



%US2
weightWithDeliveries(IDTRUCK,DL,FW):- carateristicasCam(IDTRUCK,TW,CP,_,_,_),sumDeliveryWeights(DL,DW,CP),FW is TW+DW.


sumDeliveryWeights([],0,_):-!.
sumDeliveryWeights([H|T],DW,CP):- sumDeliveryWeights(T,DW1,CP),entrega(H,_,WEIGHT,_,_,_),AUX is CP-DW1,((WEIGHT=<AUX,!,DW is DW1+WEIGHT); DW is DW1).


fullCapacity(IDTRUCK,FC):- carateristicasCam(IDTRUCK,T,C,_,_,_), FC is T+C.

ratioWeights(FW,FC,FR):- FR is FW/FC.

calculateTime(T,FW,FC,FT):-ratioWeights(FW,FC,FR), FT is T*FR.

calculateEnergy(KW,FW,FC,FE):-ratioWeights(FW,FC,FR), FE is KW*FR.


tempo(IDTRUCK,B,E,DL,TIMETOTRAVEL):- dadosCam_t_e_ta(IDTRUCK,B,E,T,_,_), weightWithDeliveries(IDTRUCK,DL,FW),fullCapacity(IDTRUCK,FC),calculateTime(T,FW,FC,FT), TIMETOTRAVEL is FT.

energy(IDTRUCK,B,E,DL,ENERGY):- dadosCam_t_e_ta(IDTRUCK,B,E,_,KW,_), weightWithDeliveries(IDTRUCK,DL,FW),fullCapacity(IDTRUCK,FC),calculateEnergy(KW,FW,FC,FE),carateristicasCam(IDTRUCK,_,_,CHARGE,_,_), ENERGY is CHARGE-FE.

%!  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extractDestinations([],[]):-!.
extractDestinations([H|T], [X|Y]):-entrega(H,_,_,W,_,_), extractDestinations(T,Y),  X is W.

findAllPaths(DL,AP):- extractDestinations(DL,WL),allPaths(WL,AP).

%%unloadTime(H,T):- entrega(H,_,_,_,_,T).


% %TimeForEachPath(IDTRUCK,DH,B,E,TIME):-tempo(IDTRUCK,B,E,DL,TT),unloadTime(DL,UT),
% updateWeight(DL), TIME is TT+UT.


%get rid of deliveries in warehouse 5.
%needed energy confirmations.(only extratraveltime added)
%update battery state

analisePath([_|[]],_,_,0):-!.
analisePath([H|[H1|T]],IDTRUCK,[X|Y],T):- tempo(IDTRUCK,H,H1,[X|Y],TT),entrega(X,_,_,_,_,UT),energy(IDTRUCK,H,H1,[X|Y],TRUCKE), extraTimeTravel(IDTRUCK,[X|Y],H1,T,TRUCKE,ET),analisePath([H1|T],IDTRUCK,Y,T1), T1 is T+TT+UT+ET.

extraTimeTravel(IDTRUCK,DL,BW,[H|_],TRUCKE,ET):- energy(IDTRUCK,BW,H,DL,TRUCKE),carateristicasCam(IDTRUCK,_,_,BAT,_,_),dadosCam_t_e_ta(IDTRUCK,BW,H,_,_,EXTRA), (TRUCKE<BAT*0.2,ET is EXTRA,TRUCKE is BAT*0.8  ; ET is 0).


%compares best time of all paths

comparePaths([],_,_,1000000):-!.
comparePaths([H|T],IDTRUCK,DL,BP):- analisePath(H,IDTRUCK,DL,TIME),comparePaths(T,IDTRUCK,DL,BP1),(BP > TIME, BP1 is TIME ; BP1 is BP).


appendDelivery(L,L1):- append([1], L, L2), append(L2,[1],L1).

%append to list
quickestPath(IDTRUCK,DELL,QL):-appendDelivery(DELL,DL),findAllPaths(DL,AP), comparePaths(AP,IDTRUCK,DL,BP), QL is BP.
