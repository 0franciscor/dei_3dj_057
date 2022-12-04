:-consult('US1.pl').

% Largest Mass first Heuristic %
extractMass([X],[M]):- entrega(X,_,M,_,_,_).
extractMass([H|T],[H1|T1]):- extractMass(T,T1), entrega(H,_,H1,_,_,_).

extractWarehouse([],[Matosinhos]):- findMatosinhos(Matosinhos).
extractWarehouse([HD|TD],[H1|T1]):- extractWarehouse(TD,T1), entrega(HD,_,_,H1,_,_).

largestMassFirst(Day,WarehouseSorted):- 
    get_time(Ti),
    getAllDeliveriesInADay(Day,Delivery), 
    extractMass(Delivery,Mass), sortTwoList(Mass,Delivery,_,DeliverySorted),
    reverse(DeliverySorted,DeliverySortedReversed), extractWarehouse(DeliverySortedReversed,WarehouseSorted),
    get_time(Tf),
    TSol is Tf - Ti,
    write(TSol),!.



%% Closest Warehouse First %%

closestWarehouseFirst([],[]).
closestWarehouseFirst(Day, PATH_LIST):- get_time(Ti),getAllDeliveriesInADay(Day,FINAL_LIST),
                                        extractDestinations(FINAL_LIST, WAREHOUSE_LIST_MATOSINHOS),
                                        delete(WAREHOUSE_LIST_MATOSINHOS, 5, WAREHOUSE_LIST),
                                        searchClosestWarehouse(5,WAREHOUSE_LIST,PATH_LIST),
                                        get_time(Tf),
                                        TSol is Tf - Ti,
                                        write(TSol).

searchClosestWarehouse(_,[],[]):-!.
searchClosestWarehouse(BEGIN,[H|T],[MENOR|PATH_LIST]):- compareClosest(BEGIN,[H|T],_,MENOR), delete([H|T], MENOR, NOVA_LISTA), searchClosestWarehouse(MENOR, NOVA_LISTA, PATH_LIST).

compareClosest(_,[],1000,_):-!.
compareClosest(ARMAZEM, [H|T], TEMPO, FH):- compareClosest(ARMAZEM, T, TEMPO1, FH1), dadosCam_t_e_ta(_, ARMAZEM, H, TEMPO2,_,_), ((TEMPO2 < TEMPO1, !, (FH is H, TEMPO is TEMPO2));(TEMPO is TEMPO1,FH is FH1)).



% Cheapest Warehouse first Mass/Km Heuristic %
extractCities([],[]).
extractCities(Delivery_List, Warehouse_List):- appendDelivery(Delivery_List, Final_List), extractDestinations(Final_List, WarehouseListWithMatosinhos),
                                               findMatosinhos(Matosinhos), delete(WarehouseListWithMatosinhos, Matosinhos, Warehouse_List).

extractMassFromWarehouse([DH],[MH]):- entrega(_,_,MH,DH,_,_).
extractMassFromWarehouse([DH|DT],[MH|MT]):-extractMassFromWarehouse(DT,MT), entrega(_,_,MH,DH,_,_).

extractDistances(_,[],[]).
extractDistances(Origin, [DH|DT],[TH|TT]):- extractDistances(Origin,DT,TT), dadosCam_t_e_ta(_, Origin, DH, TH,_,_).

toMassOverDistance([],[],[]).
toMassOverDistance([MH|MT],[DH|DT],[OH|OT]):- toMassOverDistance(MT,DT,OT), OH is MH/DH.


                    %% Bubble Sort Two lists %%
sortTwoList(List1,List2,Sorted1,Sorted2):-sort2(List1,List2,[],Sorted1,[],Sorted2).
sort2([],[],Acc1,Acc1,Acc2,Acc2).
sort2([H1|T1],[H2|T2],Acc1,Sorted1,Acc2,Sorted2):-bubble(H1,H2,T1,T2,NT1,NT2,Max1,Max2),sort2(NT1,NT2,[Max1|Acc1],Sorted1,[Max2|Acc2],Sorted2).

bubble(X1,X2,[],[],[],[],X1,X2).
bubble(X1,X2,[Y1|T1],[Y2|T2],[Y1|NT1],[Y2|NT2],Max1,Max2):-X1>Y1,bubble(X1,X2,T1,T2,NT1,NT2,Max1,Max2).
bubble(X1,X2,[Y1|T1],[Y2|T2],[X1|NT1],[X2|NT2],Max1,Max2):-X1=<Y1,bubble(Y1,Y2,T1,T2,NT1,NT2,Max1,Max2).



extractBoth(_,[],Visited,Visited).
extractBoth(Origin, Destinations, Visited,Result):- extractMassFromWarehouse(Destinations, Mass), extractDistances(Origin, Destinations, Distances), toMassOverDistance(Mass,Distances,MassOverDistances),
                                                        sortTwoList(MassOverDistances, Destinations, _, [H|T]), append([H],Visited, Visited2),extractBoth(H, T, Visited2,Result).


cheapestWarehouseFirst(Day,Result):- get_time(Ti),
    getAllDeliveriesInADay(Day,Entregas),extractCities(Entregas, Warehouse_List), findMatosinhos(Matosinhos),append([Matosinhos],Warehouse_List,[H|T]), EmptyList = [H], extractBoth(H,T,EmptyList,ResultNoMatosinhos),
    findMatosinhos(Matosinhos),append([Matosinhos],ResultNoMatosinhos,ResultReverse), reverse(ResultReverse,Result),
    get_time(Tf),
    TSol is Tf - Ti,
    write(TSol),!.
