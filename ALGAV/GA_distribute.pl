:-consult('bc_entrega.pl').
:-consult('bc_armazens.pl').
:-consult('bc_factos_camiao.pl').
:-consult('US1.pl').
:-consult('heuristics_geneticAlgorithm.pl').

% ------------------------------------------------------------------------%
:-dynamic final_list/1.
:-dynamic racio/1.


distributeDeliveries(Path, SplitList):-
    racio(DLPerTruck),
    split_list(Path,DLPerTruck,SplitList).

calculateRacio(TruckList, Mass, DeliverySize):-
    calculateMaxTruckCapacity(TruckList,MaxCapacity),
    (( MaxCapacity<Mass,!,
    extraTruck(TruckList,NewTruckList),
    RealTruckList = NewTruckList,
    ((retract(list_trucks(_));true),
    asserta(list_trucks(RealTruckList))))
    ;RealTruckList = TruckList),
    truckListLength(RealTruckList,0,TLLength),
    DLPerTruck is DeliverySize/TLLength,
    (retract(racio(_));true),
    asserta(racio(DLPerTruck)).
    




loop_split_list(TruckList,Destinations,DLPerTruck,SplitList,Date,ResultList):-
	split_list(Destinations,DLPerTruck,SplitList),
	dummy_function(TruckList,Destinations,DLPerTruck,SplitList,Date,ResultList).


dummy_function(TruckList,_,_,SplitList,Date,ResultList):-
	compareTtrucksWithDLWeight(TruckList, SplitList, Date,[]),!, ResultList = SplitList.
dummy_function(TruckList,_,DLPerTruck,SplitList,Date,ResultList):-
	compareVerified(SplitList,RandomDestinations),
    loop_split_list(TruckList,RandomDestinations,DLPerTruck,_,Date,ResultList).

%compare weights with truck capacity%


truckListLength([],AUX,TLLength):- TLLength is AUX,!.

truckListLength([_|T],AUX,TLLength):-  AUX1 is AUX + 1, truckListLength(T,AUX1,TLLength).


deliveryListLength([],AUX,DLLength):-DLLength is AUX,!.
deliveryListLength([_|T],AUX,DLLength):-AUX1 is AUX + 1,deliveryListLength(T,AUX1,DLLength).

split_list(List, X, Splits) :-
    length(List, Length),
    X_round is ceiling(X),
    split_list_(List, X_round, Length, Splits).

split_list_([], _, _, []) :- !.
split_list_(List, X, Length, [Split|Splits]) :-
    (   Length >= X
    ->  length(Split, X),
        append(Split, Rest, List),
        NewLength is Length - X,
        (split_list_(Rest, X, NewLength, Splits),!);
		Split = List,
        Splits = []
    ).


calculateWeight([],_,0):-!.
calculateWeight([H|T],Date,Weight1):-entrega(_,Date,Mass,H,_,_), calculateWeight(T,Date,Weight), Weight1 is  Weight+Mass.

calculateMaxTruckCapacity([],0):-!.
calculateMaxTruckCapacity([H|T],Weight1):- carateristicasCam(H,_,Capacity,_,_,_),calculateMaxTruckCapacity(T,Weight),Weight1 is Weight + Capacity.

compareWeight(Truck,Weight):- carateristicasCam(Truck,_,Capacity,_,_,_), Weight < Capacity.


compareTtrucksWithDLWeight([],[],_,_):-!.

compareTtrucksWithDLWeight([TH|TT],[SLH|SLT],Date,VerifiedList):-
    calculateWeight(SLH,Date,Weight),
    (compareWeight(TH,Weight),!,add_to_verified_list(SLH,VerifiedList,_)),
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



compareVerified(SplitList,RandomDestinations):-
    get_final_list(FinalList),
    get_nonmatching_sublists(SplitList, FinalList,Result),
    flatten(Result, FlattenDestinations),
    random_permutation(FlattenDestinations,PermutatedDestinations),!,
    append(FinalList,PermutatedDestinations,RandomDestinations) .




filter(_, [], []).
filter(Pred, [Head|Tail], Filtered) :-
    ( call(Pred, Head) ->
        Filtered = [Head|FilteredTail]
    ;
        Filtered = FilteredTail
    ),
    filter(Pred, Tail, FilteredTail).


get_nonmatching_sublists(SplitList, FinalList, Result) :-
    filter(contains_element_in_final_list(FinalList), SplitList, Matching),
    subtract(SplitList, Matching, Result).

contains_element_in_final_list(FinalList, Sublist) :-
    member(X, Sublist),
    member(X, FinalList).
