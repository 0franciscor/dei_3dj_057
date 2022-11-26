:-consult('bc_armazens.pl').
:-consult('bc_factos_camiao.pl').
:-consult('bc_entrega.pl').

:-dynamic infoTime/2.

allPaths(L, LF) :-((checkIfCityExist(L),!,findMatosinhos(M1),deleteMatosinhos(M1,L,L2),findall(LAux,permutation(L2,LAux),L3),appendMatosinhos([M1],L3,LF)); write('One of the entered coordinates does not correspond to a warehouse that is in the system.')).

deleteMatosinhos(A,L,L1):- ((member(A,L),!,delete(L,A,L1)); L1=L).

checkIfCityExist([]):-!.
checkIfCityExist([H|T]):- ((idArmazem(_,H),!,checkIfCityExist(T));false).

appendMatosinhos(_,[],[]):-!.
appendMatosinhos(A,[L|LL],[L2|T]):- appendMatosinhos(A,LL,T), append(A,L,L1), append(L1,A,L2).


findMatosinhos(M):-idArmazem('Matosinhos',M).

weightWithDeliveries(IDTRUCK,DL,FW):- carateristicasCam(IDTRUCK,TW,CP,_,_,_),sumDeliveryWeights(DL,DW,CP),FW is TW+DW.


sumDeliveryWeights([],0,_):-!.
sumDeliveryWeights([H|T],DW,CP):- sumDeliveryWeights(T,DW1,CP),entrega(H,_,WEIGHT,_,_,_),AUX is CP-DW1,((WEIGHT=<AUX,!,DW is DW1+WEIGHT); DW is DW1).


fullCapacity(IDTRUCK,FC):- carateristicasCam(IDTRUCK,T,C,_,_,_), FC is T+C.

ratioWeights(FW,FC,FR):- FR is FW/FC.

calculateTime(T,FW,FC,FT):-ratioWeights(FW,FC,FR), FT is T*FR.

calculateEnergy(KW,FW,FC,FE):-ratioWeights(FW,FC,FR), FE is KW*FR.


tempo(IDTRUCK,B,E,DL,TIMETOTRAVEL):- dadosCam_t_e_ta(IDTRUCK,B,E,T,_,_), weightWithDeliveries(IDTRUCK,DL,FW),fullCapacity(IDTRUCK,FC),calculateTime(T,FW,FC,FT), TIMETOTRAVEL is FT.

energy(IDTRUCK,B,E,DL,ENERGY):- dadosCam_t_e_ta(IDTRUCK,B,E,_,KW,_), weightWithDeliveries(IDTRUCK,DL,FW),fullCapacity(IDTRUCK,FC),calculateEnergy(KW,FW,FC,FE), ENERGY is FE.





%!  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extractDestinations([],[]):-!.
extractDestinations([H|T], [X|Y]):-entrega(H,_,_,W,_,_), extractDestinations(T,Y),  X is W.

findAllPaths(DL,AP):- extractDestinations(DL,WL),allPaths(WL,AP).

unloadTime([_|[YH|_]],UT):- entrega(YH,_,_,_,_,T), UT is T.

dechargeTruck(BAT,ECOST,TRUCKE):- TRUCKE is BAT-ECOST.


removeFromDel(X,[X|Y],Y).
removeFromDel(X,[H|T],[H|T1]):- removeFromDel(X,T,T1).


deleteDelivery(H1,[X|Y],FDL):- entrega(DEL,_,_,H1,_,_),removeFromDel(DEL,[X|Y],FDL).


analisePath([_|[]],_,_,_,TOTALTIME,TIME):-TIME is TOTALTIME,!.
analisePath([H|[H1|T]],IDTRUCK,[X|Y],BAT,TOTALTIME,TIME):- tempo(IDTRUCK,H,H1,[X|Y],TT),unloadTime([X|Y],UT),
    energy(IDTRUCK,H,H1,[X|Y],ECOST),dechargeTruck(BAT,ECOST,TRUCKE),
    deleteDelivery(H1,[X|Y],FDL),
    checkIfCharges(IDTRUCK,FDL,H1,T,TRUCKE,FENERGY,CT),
    extraTravelTime(IDTRUCK,FDL,H1,T,ET),
    ((CT>=UT, UT1 is CT);(UT1 is UT)),
    totalTimeCounter(TOTALT,TT,UT1,ET),TOTALTIME1 is TOTALTIME+TOTALT,
    analisePath([H1|T],IDTRUCK,FDL,FENERGY,TOTALTIME1,TIME).


totalTimeCounter(TOTALT,TT,UT,ET):- TOTALT is TT+UT+ET.


extraTravelTime(_,_,_,[],ET):-ET is 0.
extraTravelTime(IDTRUCK,DL,BW,[H|_],ET):-  energy(IDTRUCK,BW,H,DL,E),carateristicasCam(IDTRUCK,_,_,BAT,_,_),dadosCam_t_e_ta(IDTRUCK,BW,H,_,_,EXTRA),
    (BAT*0.8-E<BAT*0.2, ET is EXTRA; ET is 0).

chargeTruck(BAT,TRUCKE,CT):- CT is (BAT*0.8-TRUCKE)*60/48.

lastCharge(KW,CT):- CT is KW*60/48.


checkIfCharges(IDTRUCK,DL,BW,[5|_],TRUCKE,FENERGY,CHARGETIME):- energy(IDTRUCK,BW,5,DL,E),carateristicasCam(IDTRUCK,_,_,BAT,_,_),
(   (BAT*0.2-E>0,lastCharge(BAT*0.2-E,CT),FENERGY is BAT*0.2,CHARGETIME is CT); chargeTruck(BAT,TRUCKE,CT),FENERGY is BAT*0.8,CHARGETIME is CT ).


checkIfCharges(_,_,_,[],_,_,CHARGETIME):- CHARGETIME is 0.
checkIfCharges(IDTRUCK,DL,BW,[H|_],TRUCKE,FENERGY,CHARGETIME):- energy(IDTRUCK,BW,H,DL,E),carateristicasCam(IDTRUCK,_,_,BAT,_,_),
 (
     (TRUCKE<BAT*0.2, chargeTruck(BAT,BAT*0.2,CT),FENERGY is BAT*0.8,CHARGETIME is CT);
     ((TRUCKE-E<BAT*0.2, chargeTruck(BAT,TRUCKE,CT),FENERGY is BAT*0.8,CHARGETIME is CT );FENERGY is TRUCKE,CHARGETIME is 0)
   ).

%compares best time of all paths

comparePaths([],_,_):-!.
comparePaths([H|T],IDTRUCK,DL):-comparePaths(T,IDTRUCK,DL),carateristicasCam(IDTRUCK,_,_,BAT,_,_), TOTALTIME is 0,analisePath(H,IDTRUCK,DL,BAT,TOTALTIME,TIME),
    createPathsWithEnergy(H,TIME).

createPathsWithEnergy(H,T):-assert(infoTime(H,T)).

getAllInfoTimes(infoTime(L,T)):- setof(T,infoTime(L,T),[T|_]).

appendDelivery(L,L1):- append([1], L, L2), append(L2,[1],L1).

%append to list
quickestPath(IDTRUCK,DELL,T):-appendDelivery(DELL,DL),findAllPaths(DL,AP), comparePaths(AP,IDTRUCK,DL),getAllInfoTimes(infoTime(T,_)).
