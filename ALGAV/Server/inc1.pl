:-module(inc1, [add_city/3, update_city/3, remove_city/1,
create_truck/7, update_truck/7, remove_truck/1,
create_delivery/7, update_delivery/7, remove_delivery/1,
create_path/7, update_path/7, remove_path/2,
create_warehouse/8, update_warehouse/8, remove_warehouse/1,
bestPath_findAll/3, create_base_de_conhecimento/0, delete_base_de_conhecimento/0,
getById_city/2, getById_delivery/2, getById_truck/2,
getById_path/3, getById_warehouse/2, heuristic_mass/2,
heuristic_closestWarehouse/2,heuristic_massAndDistance/2,
count/10, resetar/0]).

:- use_module(library(http/json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json_convert)).
:- use_module(library(apply)).

:- dynamic idArmazem/2.
:- dynamic caracteristicasCam/6.
:- dynamic entrega/6.
:- dynamic dadosCam_t_e_ta/6.
:- dynamic infoTime/2.
:- dynamic armazem/7.
:- dynamic power/1.
:- dynamic infoTime/2.

:- json_object warehouse_json(id:string, address:string, altitude:integer, latitude:string, longitude:string,	designation:string, city:integer).
:- json_object city_json(id:integer, name:string).
:- json_object truck_json(truckID:string, tare:integer, capacity:integer, maxBatteryCapacity:integer, autonomy:integer, fastChargeTime:integer).
:- json_object paths_json(startWHId:string, destinationWHId:string, pathTravelTime:string, wastedEnergy:string, extraTravelTime:string).
:- json_object deliveries_json(deliveryID:string, deliveryMass:integer, destination:string, loadTime:integer, unloadTime:integer, deliveryDateProlog:string).
:- json_object bestPath_json(date:string, truck:string, bestPath:list).

% ---------------------------------------------------------------------------
city_URL("http://5.249.66.111:3001/api/warehouse/allCities").
warehouse_URL("http://5.249.66.111:3001/api/warehouse/all").
truck_URL("http://5.249.66.111:3001/api/truck/all").
path_URL("http://5.249.66.111:3001/api/path/all/undefined/undefined").
delivery_URL("http://5.249.66.111:3001/api/delivery/allProlog").

power(0).

check_sv():-
		((power(0),
        delete_base_de_conhecimento(),
        create_base_de_conhecimento(),
        retract(power(0)),
        asserta(power(1))); true).


% ---------------------------------------------------------------------------

delete_base_de_conhecimento():-
		retractall(idArmazem(_, _)),
		retractall(infoTime(_, _)),
		retractall(caracteristicasCam(_, _, _, _, _, _)),
		retractall(dadosCam_t_e_ta(_, _, _, _, _, _)),
		retractall(entrega(_, _, _, _, _, _)),
		retractall(armazem(_, _, _, _, _, _, _)),
		retractall(power(_)),
		asserta(power(0)).

% ---------------------------------------------------------------------------

create_base_de_conhecimento():-
		set_warehouse(),
		set_cities(),
		set_trucks(),
		set_deliveries(),
		set_paths(),
		assert(infoTime(100000000000000000,_)),
		assert(entrega("1","2022125",0,"5",0,0)),!.

% ---------------------------------------------------------------------------
set_warehouse():-
		get_warehouse(Warehouse),
		parse_warehouse(Warehouse).

get_warehouse(Warehouse) :-
	    warehouse_URL(URL),
	    setup_call_cleanup(
        http_open(URL, In, [request_header('Accept'='application/json'), cert_verify_hook(cert_accept_any)]),
        json_read_dict(In, Warehouse), close(In)).

parse_warehouse([]).
parse_warehouse([HWarehouse|TWarehouse]):-
		(create_warehouse(HWarehouse.get(id), HWarehouse.get(address), HWarehouse.get(altitude), HWarehouse.get(latitude), HWarehouse.get(longitude), HWarehouse.get(designation), HWarehouse.get(city), _),
		parse_warehouse(TWarehouse));
        parse_warehouse(TWarehouse).

% ---------------------------------------------------------------------------
set_cities():-
		get_cities(Cities),
		parse_cities(Cities).

get_cities(Cities) :-
	    city_URL(URL),
	    setup_call_cleanup(
        http_open(URL, In, [request_header('Accept'='application/json'), cert_verify_hook(cert_accept_any)]),
        json_read_dict(In, Cities), close(In)).

parse_cities([]).
parse_cities([HCity|TCity]):-
		(add_city(HCity.get(id), HCity.get(name), _),
		parse_cities(TCity));
        parse_cities(TCity).

% ---------------------------------------------------------------------------

set_trucks():-
		get_trucks(Trucks),
		parse_trucks(Trucks).

get_trucks(Trucks) :-
	    truck_URL(URL),
	    setup_call_cleanup(
        http_open(URL, In, [request_header('Accept'='application/json'), cert_verify_hook(cert_accept_any)]),
        json_read_dict(In, Trucks), close(In)).

parse_trucks([]).
parse_trucks([HTruck|TTruck]):-
		(create_truck(HTruck.get(truckID), HTruck.get(tare), HTruck.get(capacity), HTruck.get(maxBatteryCapacity), HTruck.get(autonomy), HTruck.get(fastChargeTime), _),
		parse_trucks(TTruck));
        parse_trucks(TTruck).

% ---------------------------------------------------------------------------

set_paths():-
		get_paths(Paths),
		parse_paths(Paths).

get_paths(Paths) :-
	    path_URL(URL),
	    setup_call_cleanup(
        http_open(URL, In, [request_header('Accept'='application/json'), cert_verify_hook(cert_accept_any)]),
        json_read_dict(In, Paths), close(In)).

parse_paths([]).
parse_paths([HPath|TPath]):-
		(create_path("eTruck01",HPath.get(destinationWHId), HPath.get(extraTravelTime), HPath.get(pathTravelTime), HPath.get(startWHId), HPath.get(wastedEnergy),_),
		parse_paths(TPath));
        parse_paths(TPath).

% ---------------------------------------------------------------------------

set_deliveries():-
		get_deliveries(Delivery),
		parse_deliveries(Delivery).

get_deliveries(Delivery) :-
	    delivery_URL(URL),
	    setup_call_cleanup(
        http_open(URL, In, [request_header('Accept'='application/json'), cert_verify_hook(cert_accept_any)]),
        json_read_dict(In, Delivery), close(In)).

parse_deliveries([]).
parse_deliveries([HDelivery|TDelivery]):-
		(create_delivery(HDelivery.get(deliveryID),HDelivery.get(deliveryDateProlog),HDelivery.get(deliveryMass), HDelivery.get(destination), HDelivery.get(loadTime), HDelivery.get(unloadTime),_),
		parse_deliveries(TDelivery));
        parse_deliveries(TDelivery).

% ---------------------------------------------------------------------------
add_city(Id, Name, CityJson):- 
		\+idArmazem(_,Id), 
		assertz(idArmazem(Name, Id)), 
		cityprolog_tojson(Id, Name, CityJson).

update_city(Id, Name, CityJson):- check_sv(),idArmazem(_, Id), retract(idArmazem(_, Id)), assertz(idArmazem(Name, Id)), cityprolog_tojson(Id, Name, CityJson).

remove_city(Id):- check_sv(),idArmazem(_, Id), retract(idArmazem(_, Id)).

getById_city(Id, CityJson):- check_sv(),idArmazem(Name, Id), cityprolog_tojson(Id,Name,CityJson).

% ---------------------------------------------------------------------------
cityprolog_tojson(Id, Name, NoJson):-
		atom_string(Id, IdJs), atom_string(Name, NameJs),
		NoJson=json([id = IdJs, name = NameJs]).

% ---------------------------------------------------------------------------
create_truck(Id, Tare, Capacity, BateryCapacity, Autonomy,TimeToCharge, TruckJson):-
			\+caracteristicasCam(Id, _, _, _, _, _), 
			assertz(caracteristicasCam(Id, Tare, Capacity, BateryCapacity, Autonomy, TimeToCharge)), 
			truckprolog_tojson(Id, Tare, Capacity, BateryCapacity, Autonomy, TimeToCharge, TruckJson).

update_truck(Id, Tare, Capacity, BateryCapacity, Autonomy,TimeToCharge, TruckJson):-
	check_sv(), caracteristicasCam(Id, _, _, _, _, _), retract(caracteristicasCam(Id, _, _, _, _, _)), assertz(caracteristicasCam(Id, Tare, Capacity, BateryCapacity, Autonomy, TimeToCharge)), truckprolog_tojson(Id, Tare, Capacity, BateryCapacity, Autonomy, TimeToCharge, TruckJson).

remove_truck(Id):-
	check_sv(),caracteristicasCam(Id, _, _, _, _, _), retract(caracteristicasCam(Id, _, _, _, _, _)).

getById_truck(Id, TruckJson):- check_sv(),caracteristicasCam(Id, Tare, Capacity, BateryCapacity, Autonomy, TimeToCharge), truckprolog_tojson(Id,Tare,Capacity,BateryCapacity,Autonomy,TimeToCharge, TruckJson).

% ---------------------------------------------------------------------------
truckprolog_tojson(Id, Tare, Capacity, BateryCapacity, Autonomy,TimeToCharge, NoJson):-
		atom_string(Id, IdJs), atom_string(Tare, TareJs), atom_string(Capacity, CapacityJs), atom_string(BateryCapacity, BateryCapacityJs), atom_string(Autonomy, AutonomyJs), atom_string(TimeToCharge, TimeToChargeJs),
		NoJson=json([id = IdJs, tare = TareJs, capacity = CapacityJs, bateryCapacity = BateryCapacityJs, autonomy = AutonomyJs, timeToCharge = TimeToChargeJs]).

% ---------------------------------------------------------------------------
create_delivery(Id, Date, Mass, Destination, LoadTime, UnloadTime, DeliveryJson):-
				\+entrega(Id, _, _, _, _, _), 
				assertz(entrega(Id, Date, Mass, Destination, LoadTime, UnloadTime)), 
				deliveryprolog_tojson(Id, Date, Mass, Destination, LoadTime, UnloadTime, DeliveryJson).

update_delivery(Id, Date, Mass, Destination, LoadTime, UnloadTime, DeliveryJson):-
	check_sv(),entrega(Id, _, _, _, _, _), retract(entrega(Id, _, _, _, _, _)), assertz(entrega(Id, Date, Mass, Destination, LoadTime, UnloadTime)), deliveryprolog_tojson(Id, Date, Mass, Destination, LoadTime, UnloadTime, DeliveryJson).

remove_delivery(Id):-
	check_sv(),entrega(Id, _, _, _, _, _), retract(entrega(Id, _, _, _, _, _)).

getById_delivery(Id, DeliveryJson):-
	check_sv(),entrega(Id, Date, Mass, Destination, LoadTime, UnloadTime),
	deliveryprolog_tojson(Id, Date, Mass, Destination, LoadTime, UnloadTime, DeliveryJson).


% ---------------------------------------------------------------------------
deliveryprolog_tojson(Id, Date, Mass, Destination, LoadTime, UnloadTime, NoJson):-
		atom_string(Id, IdJs), atom_string(Date, DateJs), atom_string(Mass, MassJs), atom_string(Destination, DestinationJs), atom_string(LoadTime, LoadTimeJs), atom_string(UnloadTime, UnloadTimeJs),
		NoJson=json([id = IdJs, date = DateJs, mass = MassJs, destination = DestinationJs, loadTime = LoadTimeJs, unloadTime = UnloadTimeJs]).

% ---------------------------------------------------------------------------
create_path(Truck, CityDest, AditionalTime, PathTime, CityOrig, Energy, PathJson):-
			\+dadosCam_t_e_ta(_, City, City1, _, _, _),
			armazem(CityOrig,_,_,_,_,_,City),
			armazem(CityDest,_,_,_,_,_,City1),
			assertz(dadosCam_t_e_ta(Truck, City, City1, PathTime, Energy, AditionalTime)), 			
			pathprolog_tojson(Truck, City, City1, PathTime, Energy, AditionalTime, PathJson).

update_path(Truck, CityOrig, CityDest, PathTime, Energy, AditionalTime, PathJson):-
	check_sv(),dadosCam_t_e_ta(_, CityOrig, CityDest, _, _, _), retract(dadosCam_t_e_ta(_, CityOrig, CityDest, _, _, _)), assertz(dadosCam_t_e_ta(Truck, CityOrig, CityDest, PathTime, Energy, AditionalTime)), pathprolog_tojson(Truck, CityOrig, CityDest, PathTime, Energy, AditionalTime, PathJson).

remove_path(CityOrig, CityDest):-
	check_sv(),dadosCam_t_e_ta(_, CityOrig, CityDest, _, _, _), retract(dadosCam_t_e_ta(_, CityOrig, CityDest, _, _, _)).

getById_path(Origin, Destination, PathJson):-
	check_sv(),
	dadosCam_t_e_ta(Truck, Origin, Destination, PathTime, Energy, AditionalTime),
	pathprolog_tojson(Truck, Origin, Destination, PathTime, Energy, AditionalTime, PathJson).

% ---------------------------------------------------------------------------
pathprolog_tojson(Truck, CityOrig, CityDest, PathTime, Energy, AditionalTime, NoJson):-
		atom_string(Truck, TruckJs),atom_string(CityOrig, CityOrigJs),atom_string(CityDest, CityDestJs),atom_string(PathTime, PathTimeJs),atom_string(Energy, EnergyJs),atom_string(AditionalTime, AditionalTimeJs),
		NoJson=json([truck = TruckJs, origin = CityOrigJs, destination = CityDestJs, totalTime = PathTimeJs, energy = EnergyJs, aditionalTime = AditionalTimeJs]).

% ---------------------------------------------------------------------------
create_warehouse(Id, Address, Altitude, Latitude, Longitude, Designation, City, WarehouseJson):-
				\+armazem(Id,_,_,_,_,_,_), 
				assertz(armazem(Id, Address, Altitude, Latitude, Longitude, Designation, City)), 
				warehouseprolog_tojson(Id, Address, Altitude, Latitude, Longitude, Designation, City, WarehouseJson).

update_warehouse(Id, Address, Altitude, Latitude, Longitude, Designation, City, WarehouseJson):-
	check_sv(),armazem(Id,_,_,_,_,_,_), retract(armazem(Id,_,_,_,_,_,_)), assertz(armazem(Id, Address, Altitude, Latitude, Longitude, Designation, City)), warehouseprolog_tojson(Id, Address, Altitude, Latitude, Longitude, Designation, City, WarehouseJson).

remove_warehouse(Id):-
	check_sv(),armazem(Id,_,_,_,_,_,_), retract(armazem(Id,_,_,_,_,_,_)).

getById_warehouse(Id, WarehouseJson):-
	check_sv(),armazem(Id, Address, Altitude, Latitude, Longitude, Designation, City),
	warehouseprolog_tojson(Id, Address, Altitude, Latitude, Longitude, Designation, City, WarehouseJson).

% ---------------------------------------------------------------------------
warehouseprolog_tojson(Id, Address, Altitude, Latitude, Longitude, Designation, City, NoJson):-
		atom_string(Id, IdJs), atom_string(Address, AddressJs), atom_string(Altitude, AltitudeJs), atom_string(Latitude, LatitudeJs), atom_string(Longitude, LongitudeJs), atom_string(Designation, DesignationJs),
		atom_string(City, CityJs),
		NoJson=json([id = IdJs, address = AddressJs, altitude = AltitudeJs, latitude = LatitudeJs, longitude = LongitudeJs, designation = DesignationJs, city = CityJs]).

% ---------------------------------------------------------------------------
count(LC, LT, LD, LP, LW, TC, TT, TD, TP, TW):-
		findall((_,_), idArmazem(_,_), LC),
        findall((_,_,_,_,_,_), caracteristicasCam(_,_,_,_,_,_), LT),
        findall((_,_,_,_,_,_), entrega(_,_,_,_,_,_), LD),
        findall((_,_,_,_,_,_), dadosCam_t_e_ta(_,_,_,_,_,_), LP),
        findall((_,_,_,_,_,_,_), armazem(_,_,_,_,_,_,_), LW),
        length(LC, TC),
        length(LT, TT),
        length(LD, TD),
        length(LP, TP),
        length(LW, TW).

% ---------------------------------------------------------------------------
resetar():-
	retractall(idArmazem(_,_)).

% ----------------------------------US1---------------------------------------

allPaths(L, LF) :-((checkIfCityExist(L),!,findMatosinhos(M1),deleteMatosinhos(M1,L,L2),findall(LAux,permutation(L2,LAux),L3),appendMatosinhos([M1],L3,LF)); write('One of the entered coordinates does not correspond to a warehouse that is in the system.')).

deleteMatosinhos(A,L,L1):- ((member(A,L),!,delete(L,A,L1)); L1=L).

checkIfCityExist([]):-!.
checkIfCityExist([H|T]):- ((idArmazem(_,H),!,checkIfCityExist(T));false).

appendMatosinhos(_,[],[]):-!.
appendMatosinhos(A,[L|LL],[L2|T]):- appendMatosinhos(A,LL,T), append(A,L,L1), append(L1,A,L2).

findMatosinhos(M):-idArmazem("Matosinhos",M).


% ----------------------------------US2---------------------------------------

bestPath_findAll(Date, Truck, Json):-
	check_sv(),
	quickestPath(Truck, Date, Result),
	bestPathProlog_ToJson(Date,Truck,Result,Json).

bestPathProlog_ToJson(Date,Truck,Result,NoJson):-
	atom_string(Date, DateJs), atom_string(Truck, TruckJs), lista_para_string(Result, ResultJs),
	NoJson=json([date = DateJs, truck = TruckJs, bestPath = ResultJs]).

lista_para_string([], []).
lista_para_string([H|T], [HS|TS]):-
	    atom_string(H, HS),
	    lista_para_string(T, TS).

weightWithDeliveries(IDTRUCK,DL,FW):- caracteristicasCam(IDTRUCK,TW,CP,_,_,_),sumDeliveryWeights(DL,DW,CP),FW is TW+DW.

sumDeliveryWeights([],0,_):-!.
sumDeliveryWeights([H|T],DW,CP):- sumDeliveryWeights(T,DW1,CP),entrega(H,_,WEIGHT,_,_,_),AUX is CP-DW1,((WEIGHT=<AUX,!,DW is DW1+WEIGHT); DW is DW1).


fullCapacity(IDTRUCK,FC):- caracteristicasCam(IDTRUCK,T,C,_,_,_), FC is T+C.

ratioWeights(FW,FC,FR):- FR is FW/FC.

calculateTime(T,FW,FC,FT):-ratioWeights(FW,FC,FR), FT is T*FR.

calculateEnergy(KW,FW,FC,FE):-ratioWeights(FW,FC,FR), FE is KW*FR.


tempo(IDTRUCK,B,E,DL,TIMETOTRAVEL):- dadosCam_t_e_ta(IDTRUCK,B,E,T,_,_), weightWithDeliveries(IDTRUCK,DL,FW),fullCapacity(IDTRUCK,FC),calculateTime(T,FW,FC,FT), TIMETOTRAVEL is FT.

energy(IDTRUCK,B,E,DL,ENERGY):- dadosCam_t_e_ta(IDTRUCK,B,E,_,KW,_), weightWithDeliveries(IDTRUCK,DL,FW),fullCapacity(IDTRUCK,FC),calculateEnergy(KW,FW,FC,FE), ENERGY is FE.

extractDestinations([],[]):-!.
extractDestinations([H|T], [X|Y]):-entrega(H,_,_,W,_,_), extractDestinations(T,Y),  X = W.

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
extraTravelTime(IDTRUCK,DL,BW,[H|_],ET):-  energy(IDTRUCK,BW,H,DL,E),caracteristicasCam(IDTRUCK,_,_,BAT,_,_),dadosCam_t_e_ta(IDTRUCK,BW,H,_,_,EXTRA),
    (BAT*0.8-E<BAT*0.2, ET is EXTRA; ET is 0).

chargeTruck(BAT,TRUCKE,CT):- CT is (BAT*0.8-TRUCKE)*60/48.

lastCharge(KW,CT):- CT is KW*60/48.


checkIfCharges(IDTRUCK,DL,BW,[5|_],TRUCKE,FENERGY,CHARGETIME):- energy(IDTRUCK,BW,5,DL,E),caracteristicasCam(IDTRUCK,_,_,BAT,_,_),
(   (BAT*0.2-E>0,lastCharge(BAT*0.2-E,CT),FENERGY is BAT*0.2,CHARGETIME is CT); chargeTruck(BAT,TRUCKE,CT),FENERGY is BAT*0.8,CHARGETIME is CT ).


checkIfCharges(_,_,_,[],_,_,CHARGETIME):- CHARGETIME is 0.
checkIfCharges(IDTRUCK,DL,BW,[H|_],TRUCKE,FENERGY,CHARGETIME):- energy(IDTRUCK,BW,H,DL,E),caracteristicasCam(IDTRUCK,_,_,BAT,_,_),
 (
     (TRUCKE<BAT*0.2, chargeTruck(BAT,BAT*0.2,CT),FENERGY is BAT*0.8,CHARGETIME is CT);
     ((TRUCKE-E<BAT*0.2, chargeTruck(BAT,TRUCKE,CT),FENERGY is BAT*0.8,CHARGETIME is CT );FENERGY is TRUCKE,CHARGETIME is 0)
   ).


comparePaths([],_,_):-!.
comparePaths([H|T],IDTRUCK,DL):-comparePaths(T,IDTRUCK,DL),caracteristicasCam(IDTRUCK,_,_,BAT,_,_), TOTALTIME is 0,analisePath(H,IDTRUCK,DL,BAT,TOTALTIME,TIME), infoTime(TAux, _), ((TAux > TIME,!, retract(infoTime(TAux,_)),assert(infoTime(TIME,H))); true).

appendDelivery(L,L1):- append(["1"], L, L2), append(L2,["1"],L1).

getAllDeliveriesInADay(DATE, LDFinal):- findall(X, entrega(X,DATE,_,_,_,_), LD), delete(LD,"1",LDFinal).

quickestPath(IDTRUCK,DATE,L):-!,getAllDeliveriesInADay(DATE,DELL),retract(infoTime(_,_)),assert(infoTime(100000,_)),appendDelivery(DELL,DL),findAllPaths(DL,AP),comparePaths(AP,IDTRUCK,DL),!,infoTime(_,L).



% ----------------------------------Heuristics---------------------------------------

% Largest Mass first Heuristic %

heuristic_mass(Date, ResultJson):-
	check_sv(),
	largestMassFirst(Date,Result),
	resultHeuristics_Prolog_ToJson(Date,Result,ResultJson).

resultHeuristics_Prolog_ToJson(Date,Result,NoJson):-
	atom_string(Date, DateJs), lista_para_string(Result, ResultJs),
	NoJson=json([date = DateJs, bestRoute = ResultJs]).

extractMass([X],[M]):- entrega(X,_,M,_,_,_).
extractMass([H|T],[H1|T1]):- extractMass(T,T1), entrega(H,_,H1,_,_,_).

extractWarehouse([],[Matosinhos]):- findMatosinhos(Matosinhos).
extractWarehouse([HD|TD],[H1|T1]):- extractWarehouse(TD,T1), entrega(HD,_,_,H1,_,_).

largestMassFirst(Day,WarehouseSorted):- 
    getAllDeliveriesInADay(Day,Delivery), 
    extractMass(Delivery,Mass), sortTwoList(Mass,Delivery,_,DeliverySorted),
    reverse(DeliverySorted,DeliverySortedReversed), extractWarehouse(DeliverySortedReversed,WarehouseSorted),!.

%% Closest Warehouse First %%

heuristic_closestWarehouse(Date, ResultJson):-
	check_sv(),
	closestWarehouseFirst(Date,Result),
	resultHeuristics_Prolog_ToJson(Date,Result,ResultJson).

closestWarehouseFirst([],[]).
closestWarehouseFirst(Day, PATH_LIST):- getAllDeliveriesInADay(Day,FINAL_LIST),
                                        extractDestinations(FINAL_LIST, WAREHOUSE_LIST_MATOSINHOS),
                                        delete(WAREHOUSE_LIST_MATOSINHOS, 5, WAREHOUSE_LIST),
                                        searchClosestWarehouse("5",WAREHOUSE_LIST,PATH_LIST).
searchClosestWarehouse(_,[],[]):-!.
searchClosestWarehouse(BEGIN,[H|T],[MENOR|PATH_LIST]):- compareClosest(BEGIN,[H|T],_,MENOR), delete([H|T], MENOR, NOVA_LISTA), searchClosestWarehouse(MENOR, NOVA_LISTA, PATH_LIST).

compareClosest(_,[],1000,_):-!.
compareClosest(ARMAZEM, [H|T], TEMPO, FH):- compareClosest(ARMAZEM, T, TEMPO1, FH1), dadosCam_t_e_ta(_, ARMAZEM, H, TEMPO2,_,_), ((TEMPO2 < TEMPO1, !, (FH = H, TEMPO is TEMPO2));(TEMPO is TEMPO1,FH = FH1)).


% Cheapest Warehouse first Mass/Km Heuristic %

heuristic_massAndDistance(Date, ResultJson):-
	check_sv(),
	cheapestWarehouseFirst(Date,Result),
	resultHeuristics_Prolog_ToJson(Date,Result,ResultJson).


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


cheapestWarehouseFirst(Day,Result):- 
    getAllDeliveriesInADay(Day,Entregas),extractCities(Entregas, Warehouse_List), findMatosinhos(Matosinhos),append([Matosinhos],Warehouse_List,[H|T]), EmptyList = [H], extractBoth(H,T,EmptyList,ResultNoMatosinhos),
    findMatosinhos(Matosinhos),append([Matosinhos],ResultNoMatosinhos,ResultReverse), reverse(ResultReverse,Result),!.