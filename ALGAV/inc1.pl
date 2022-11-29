:- module(inc1, [add_city/3, update_city/3, remove_city/1,
create_truck/7, update_truck/7, remove_truck/1,
create_delivery/7, update_delivery/7, remove_delivery/1,
create_path/7, update_path/7, remove_path/2,
create_warehouse/8, update_warehouse/8, remove_warehouse/1,
count/8, resetar/0]).
                
:- use_module(library(http/json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json_convert)).
:- use_module(library(apply)).

:- dynamic idArmazem/2.
:- dynamic caracteristicasCam/6.
:- dynamic entrega/6.
:- dynamic dadosCam_t_e_ta/6.
:-dynamic infoTime/2.
:-dynamic armazem/7.

:- json_object warehouse_json(id:string, address:string, altitude:integer, latitude:string, longitude:string, 	designation:string, city:integer).
:- json_object city_json(id:integer, name:string).
:- json_object truck_json(truckID:string, tare:integer, capacity:integer, maxBatteryCapacity:integer, autonomy:integer, fastChargeTime:integer).
:- json_object paths_json(pathTravelTime:integer, wastedEnergy:double,extraTravelTime:integer).
:- json_object deliveries_json(deliveryID:string, deliveryDate:string, deliveryMass:integer, destination:string, loadTime:integer, unloadTime:integer).

% ---------------------------------------------------------------------------
city_URL("http://5.249.66.111:3001/api/warehouse/allCities").
warehouse_URL("http://5.249.66.111:3001/api/warehouse/all").
truck_URL("http://5.249.66.111:3001/api/truck/all").
path_URL("http://5.249.66.111:3001/api/path/all/undefined/").
delivery_URL("http://5.249.66.111:3001/api/delivery/all").

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
		set_paths(),!.

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
		(create_warehouse(HWarehouse.get(id), HWarehouse.get(address), HWarehouse.get(altitude), HWarehouse.get(altitude), HWarehouse.get(latitude), HWarehouse.get(longitude), HWarehouse.get(designation), HWarehouse.get(city), _),
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
parse_paths([HPath|TPath]):- armazem(HPath.get(startWHId),_,_,_,_,_,InitialCity),
		armazem(HPath.get(destinationWHId),_,_,_,_,_,FinalCity),
		(create_path('eTruck01', InitialCity, FinalCity, HPath.get(pathTravelTime), HPath.get(wastedEnergy),HPath.get(extraTravelTime),_),
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
		(create_delivery(HDelivery.get(deliveryID), HDelivery.get(deliveryDate), HDelivery.get(deliveryMass), HDelivery.get(destination), HDelivery.get(loadTime), HDelivery.get(unloadTime),_),
		parse_deliveries(TDelivery));
                parse_deliveries(TDelivery).

% ---------------------------------------------------------------------------
add_city(Id, Name, CityJson):- \+idArmazem(_,Id), assertz(idArmazem(Name, Id)), cityprolog_tojson(Id, Name, CityJson).
    
update_city(Id, Name, CityJson):- idArmazem(_, Id), retract(idArmazem(_, Id)), assertz(idArmazem(Name, Id)), cityprolog_tojson(Id, Name, CityJson).
    
remove_city(Id):- idArmazem(_, Id), retract(idArmazem(_, Id)).

% ---------------------------------------------------------------------------
cityprolog_tojson(Id, Name, NoJson):-
		atom_string(Id, IdJs), atom_string(Name, NameJs),
		NoJson=json([id = IdJs, name = NameJs]).

% ---------------------------------------------------------------------------
create_truck(Id, Tare, Capacity, BateryCapacity, Autonomy,TimeToCharge, TruckJson):- 
	\+caracteristicasCam(Id, _, _, _, _, _), assertz(caracteristicasCam(Id, Tare, Capacity, BateryCapacity, Autonomy, TimeToCharge)), truckprolog_tojson(Id, Tare, Capacity, BateryCapacity, Autonomy, TimeToCharge, TruckJson).
    
update_truck(Id, Tare, Capacity, BateryCapacity, Autonomy,TimeToCharge, TruckJson):- 
	caracteristicasCam(Id, _, _, _, _, _), retract(caracteristicasCam(Id, _, _, _, _, _)), assertz(caracteristicasCam(Id, Tare, Capacity, BateryCapacity, Autonomy, TimeToCharge)), truckprolog_tojson(Id, Tare, Capacity, BateryCapacity, Autonomy, TimeToCharge, TruckJson).
    
remove_truck(Id):- 
	caracteristicasCam(Id, _, _, _, _, _), retract(caracteristicasCam(Id, _, _, _, _, _)).

% ---------------------------------------------------------------------------
truckprolog_tojson(Id, Tare, Capacity, BateryCapacity, Autonomy,TimeToCharge, NoJson):-
		atom_string(Id, IdJs), atom_string(Tare, TareJs), atom_string(Capacity, CapacityJs), atom_string(BateryCapacity, BateryCapacityJs), atom_string(Autonomy, AutonomyJs), atom_string(TimeToCharge, TimeToChargeJs), 
		NoJson=json([id = IdJs, tare = TareJs, capacity = CapacityJs, bateryCapacity = BateryCapacityJs, autonomy = AutonomyJs, timeToCharge = TimeToChargeJs]).
        
% ---------------------------------------------------------------------------
create_delivery(Id, Date, Mass, Destination, LoadTime, UnloadTime, DeliveryJson):-
	\+entrega(Id, _, _, _, _, _), assertz(entrega(Id, Date, Mass, Destination, LoadTime, UnloadTime)), deliveryprolog_tojson(Id, Date, Mass, Destination, LoadTime, UnloadTime, DeliveryJson).
    
update_delivery(Id, Date, Mass, Destination, LoadTime, UnloadTime, DeliveryJson):-
	entrega(Id, _, _, _, _, _), retract(entrega(Id, _, _, _, _, _)), assertz(entrega(Id, Date, Mass, Destination, LoadTime, UnloadTime)), deliveryprolog_tojson(Id, Date, Mass, Destination, LoadTime, UnloadTime, DeliveryJson).
    
remove_delivery(Id):-
	entrega(Id, _, _, _, _, _), retract(entrega(Id, _, _, _, _, _)).

% ---------------------------------------------------------------------------
deliveryprolog_tojson(Id, Date, Mass, Destination, LoadTime, UnloadTime, NoJson):-
		atom_string(Id, IdJs), atom_string(Date, DateJs), atom_string(Mass, MassJs), atom_string(Destination, DestinationJs), atom_string(LoadTime, LoadTimeJs), atom_string(UnloadTime, UnloadTimeJs), 
		NoJson=json([id = IdJs, date = DateJs, mass = MassJs, destination = DestinationJs, loadTime = LoadTimeJs, unloadTime = UnloadTimeJs]).

% ---------------------------------------------------------------------------
create_path(Truck, CityOrig, CityDest, PathTime, Energy, AditionalTime, PathJson):-
	\+dadosCam_t_e_ta(_, CityOrig, CityDest, _, _, _), assertz(dadosCam_t_e_ta(Truck, CityOrig, CityDest, PathTime, Energy, AditionalTime)), pathprolog_tojson(Truck, CityOrig, CityDest, PathTime, Energy, AditionalTime, PathJson).
    
update_path(Truck, CityOrig, CityDest, PathTime, Energy, AditionalTime, PathJson):-
	dadosCam_t_e_ta(_, CityOrig, CityDest, _, _, _), retract(dadosCam_t_e_ta(_, CityOrig, CityDest, _, _, _)), assertz(dadosCam_t_e_ta(Truck, CityOrig, CityDest, PathTime, Energy, AditionalTime)), pathprolog_tojson(Truck, CityOrig, CityDest, PathTime, Energy, AditionalTime, PathJson).
    
remove_path(CityOrig, CityDest):-
	dadosCam_t_e_ta(_, CityOrig, CityDest, _, _, _), retract(dadosCam_t_e_ta(_, CityOrig, CityDest, _, _, _)).

% ---------------------------------------------------------------------------
pathprolog_tojson(Truck, CityOrig, CityDest, PathTime, Energy, AditionalTime, NoJson):-
		atom_string(Truck, TruckJs), atom_string(CityOrig, CityOrigJs), atom_string(CityDest, CityDestJs), atom_string(PathTime, PathTimeJs), atom_string(Energy, EnergyJs), atom_string(AditionalTime, AditionalTimeJs), 
		NoJson=json([truck = TruckJs, origin = CityOrigJs, destination = CityDestJs, totalTime = PathTimeJs, energy = EnergyJs, aditionalTime = AditionalTimeJs]).

% ---------------------------------------------------------------------------
create_warehouse(Id, Address, Altitude, Latitude, Longitude, Designation, City, CityJson):-
	\+armazem(Id,_,_,_,_,_,_), assertz(armazem(Id, Address, Altitude, Latitude, Longitude, Designation, City)), warehouseprolog_tojson(Id, Address, Altitude, Latitude, Longitude, Designation, City, CityJson).
    
update_warehouse(Id, Address, Altitude, Latitude, Longitude, Designation, City, WarehouseJson):-
	armazem(Id,_,_,_,_,_,_), retract(armazem(Id,_,_,_,_,_,_)), assertz(armazem(Id, Address, Altitude, Latitude, Longitude, Designation, City)), warehouseprolog_tojson(Id, Address, Altitude, Latitude, Longitude, Designation, City, WarehouseJson).
    
remove_warehouse(Id):-
	armazem(Id,_,_,_,_,_,_), retract(armazem(Id,_,_,_,_,_,_)).
	
% ---------------------------------------------------------------------------
warehouseprolog_tojson(Id, Address, Altitude, Latitude, Longitude, Designation, City, NoJson):-
		atom_string(Id, IdJs), atom_string(Address, AddressJs), atom_string(Altitude, AltitudeJs), atom_string(Latitude, LatitudeJs), atom_string(Longitude, LongitudeJs), atom_string(Designation, DesignationJs),
		atom_string(City, CityJs), 
		NoJson=json([id = IdJs, address = AddressJs, altitude = AltitudeJs, latitude = LatitudeJs, longitude = LongitudeJs, designation = DesignationJs, city = CityJs]).

% ---------------------------------------------------------------------------
count(LC, LT, LD, LP, TC, TT, TD, TP):-
		findall((_,_), idArmazem(_,_), LC),
        findall((_,_,_,_,_,_), caracteristicasCam(_,_,_,_,_,_), LT),
        findall((_,_,_,_,_,_), entrega(_,_,_,_,_,_), LD),
        findall((_,_,_,_,_,_), dadosCam_t_e_ta(_,_,_,_,_,_), LP),
        length(LC, TC),
        length(LT, TT),
        length(LD, TD),
        length(LP, TP).

% ---------------------------------------------------------------------------
resetar():-
	retractall(idArmazem(_,_)).


