:-consult('bc_armazens.pl').
:-consult('bc_factos_camiao.pl')
:-consult('bc_entrega.pl')

allPaths(L, LF) :-(checkIfCityExist(L),!,findMatosinhos(M1),deleteMatosinhos(M1,L,L2),findall(LAux,permutation(L2,LAux),L3),appendMatosinhos([M1],L3,LF)); write('One of the entered coordinates does not correspond to a warehouse that is in the system.').

deleteMatosinhos(A,L,L1):- ((member(A,L),!,delete(L,A,L1)); true).

checkIfCityExist([]):-!.
checkIfCityExist([H|T]):- ((idArmazem(_,H),!,checkIfCityExist(T));false).

appendMatosinhos(_,[],[]):-!.
appendMatosinhos(A,[L|LL],[L2|T]):- appendMatosinhos(A,LL,T), append(A,L,L1), append(L1,A,L2).

findMatosinhos(M):-idArmazem('Matosinhos',M).



weightWithDeliveries(IDTRUCK,DL,FW):- carateristicasCam(IDTRUCK,TW,_,_,_,_),sumDeliveryWeights(DL,TW,DW),FW is TW+DW.


sumDeliveryWeights([H|T],TW,DW):-entrega(H,_,DM,_,_,_),TW1 is TW+DM, sumDeliveryWeights(T,TW1,DW), DW is TW.
sumDeliveryWeights([H|[]],_,DW):-entrega(H,_,DM,_,_,_),DW is DM,!.

fullCapacity(IDTRUCK,FC):- carateristicasCam(IDTRUCK,T,C,_,_,_), FC is T*C.


ratioWeights(FW,FC,FR):- FR is FW/FC.

calculateTime(T,FW,FC,FT):-ratioWeights(FW,FC,FR), FT is T*FR.

calculateEnergy(KW,FW,FC,FE):-ratioWeights(FW,FC,FR), FE is KW*FR.




tempo(IDTRUCK,B,E,DL,TIMETOTRAVEL):- dadosCam_t_e_ta(IDTRUCK,B,E,T,_,_), weightWithDeliveries(IDTRUCK,DL,FW),fullCapacity(IDTRUCK,FC),calculateTime(T,FW,FC,FT), TIMETOTRAVEL is FT.

energy(IDTRUCK,B,E,DL,ENERGY):- dadosCam_t_e_ta(IDTRUCK,B,E,_,KW,_), weightWithDeliveries(IDTRUCK,DL,FW),fullCapacity(IDTRUCK,FC),calculateEnergy(KW,FW,FC,FE), ENERGY is FE.

