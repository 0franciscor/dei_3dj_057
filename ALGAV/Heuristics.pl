:-consult('US1.pl').



% Largest Mass first Heuristic %
extractMass([X],[M]):- entrega(X,_,M,_,_,_).
extractMass([H|T],[H1|T1]):- extractMass(T,T1), entrega(H,_,H1,_,_,_).

extractWarehouse([],[Matosinhos]):- findMatosinhos(Matosinhos).
extractWarehouse([H|T],[H1|T1]):- extractWarehouse(T,T1), entrega(_,_,H,H1,_,_).

largestMassFirst(E,L):- extractMass(E,M), sort(M, M1), reverse(M1, M2), extractWarehouse(M2,L1), findMatosinhos(Matosinhos),append([Matosinhos],L1,L).


%% Closest Warehouse First %%


closestWarehouseFirst([],[]).
closestWarehouseFirst(DELIVERY_LIST, PATH_LIST):- appendDelivery(DELIVERY_LIST, FINAL_LIST), extractDestinations(FINAL_LIST, WAREHOUSE_LIST), searchClosestWarehouse(WAREHOUSE_LIST,PATH_LIST).

searchClosestWarehouse([],[]):-!.
searchClosestWarehouse([5|[H|T]], PATH_LIST):- dadosCam_t_e_ta(_, H, HT, TEMPO,_,_), compareClosest(H,[HT|TT],TEMPO,PATH_LIST).

compareClosest(_,[|5],_,1000000,[]):-!.
compareClosest(ARMAZEM, [H|Y], TEMPO, [FH|FT]):- compareClosest(ARMAZEM, Y, TEMPO2, FT), dadosCam_t_e_ta(TRUCKID, ARMAZEM, H, TEMPO,_,_), ((TEMPO < TEMPO2, !, FH is ARMAZEM); FH is H).






% Cheapest Warehouse first Mass/Km Heuristic %
extractCities([],[]).
extractCities(Delivery_List, Warehouse_List):- appendDelivery(Delivery_List, Final_List), extractDestinations(Final_List, WarehouseListWithCringe), delete(WarehouseListWithCringe, 5, Warehouse_List).

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


cheapestWarehouseFirst(Entregas,Result):- extractCities(Entregas, Warehouse_List), findMatosinhos(Matosinhos),append([Matosinhos],Warehouse_List,[H|T]), EmptyList = [H], extractBoth(H,T,EmptyList,ResultNoMatosinhos),
                                          findMatosinhos(Matosinhos),append([Matosinhos],ResultNoMatosinhos,ResultReverse), reverse(ResultReverse,Result),!. 