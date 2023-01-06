:- use_module(inc1).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_cors)).
:- use_module(library(option)).

:- http_handler(/, reply_root, []).
:- http_handler('/create_city', create_city,  []).
:- http_handler('/create_truck', create_truck, []).
:- http_handler('/create_delivery', create_delivery, []).
:- http_handler('/create_path', create_path, []).
:- http_handler('/create_warehouse', create_warehouse, []).
:- http_handler('/update_city', update_city,  []).
:- http_handler('/update_truck', update_truck, []).
:- http_handler('/update_delivery', update_delivery, []).
:- http_handler('/update_path', update_path, []).
:- http_handler('/update_warehouse', update_warehouse, []).
:- http_handler('/delete_city', delete_city,  []).
:- http_handler('/delete_truck', delete_truck, []).
:- http_handler('/delete_delivery', delete_delivery, []).
:- http_handler('/delete_path', delete_path, []).
:- http_handler('/delete_warehouse', delete_warehouse, []).
:- http_handler('/getById', getById_city, []).
:- http_handler('/getByIdDelivery', getById_delivery, []).
:- http_handler('/getByIdTruck', getById_truck, []).
:- http_handler('/getByIdPath', getById_path, []).
:- http_handler('/getByIdWarehouse', getById_warehouse, []).
:- http_handler('/bestPath_findAll', bestPath_findAll, []).
:- http_handler('/heuristic_mass', heuristic_mass, []).
:- http_handler('/heuristic_closestWarehouse', heuristic_closestWarehouse, []).
:- http_handler('/heuristic_massAndDistance', heuristic_massAndDistance, []).
:- http_handler('/count', count, []).
:- http_handler('/reset', resetar, []).

reply_root(_Request) :-
        format('Content-type: text/plain~n~n'),
        format('HTTP Server on SWI-Prolog is ready~n').

count(_Request):-
        cors_enable,
        count(LC, LT, LD, LP, LW, TC, TT, TD, TP, TW),
        format('Content-type: text/plain~n~n'),
        format('~w~n~w~n~w~n~w~n~w~n', [TC, TT, TD, TP, TW]),
        format('~w~n~w~n~w~n~w~n~w~n', [LC, LT, LD, LP, LW]).


resetar(_Request):-
		cors_enable, (resetar(), reply_json(_, [status(200)])); reply_json(_, [status(500)]).
% ---------------------------------------------------------------------------

create_city(Request):-
	cors_enable,
        http_read_json_dict(Request, DictIn),
        Id = DictIn.get(id), Name = DictIn.get(name),
        (add_city(Id, Name, CityJson), reply_json(CityJson, [status(200)]), !);
        reply_json(_, [status(500)]).

update_city(Request):-
	cors_enable,
        http_read_json_dict(Request, DictIn),
        Id = DictIn.get(id), Name = DictIn.get(name),
        (update_city(Id, Name, CityJson), reply_json(CityJson,  [status(200)]), !);
        reply_json(_, [status(500)]).

delete_city(Request):-
	cors_enable,
        http_read_json_dict(Request, DictIn),
        Id = DictIn.get(id),
        (remove_city(Id), reply_json(Id,  [status(200)]), !);
        reply_json(_, [status(500)]).

getById_city(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        Id = DictIn.get(id),
        (getById_city(Id, CityJson), reply_json(CityJson,  [status(200)]), !);
        reply_json(_, [status(500)]).

% ---------------------------------------------------------------------------

create_truck(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        Id = DictIn.get(truckId), Tare = DictIn.get(tare), Capacity = DictIn.get(capacity), BateryCapacity = DictIn.get(maxBatteryCapacity), Autonomy = DictIn.get(autonomy), TimeToCharge = DictIn.get(fastChargeTime),
        (create_truck(Id, Tare, Capacity, BateryCapacity, Autonomy,TimeToCharge, TruckJson), reply_json(TruckJson,  [status(200)]), !);
        reply_json(_, [status(500)]).

update_truck(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        Id = DictIn.get(truckId), Tare = DictIn.get(tare), Capacity = DictIn.get(capacity), BateryCapacity = DictIn.get(maxBatteryCapacity), Autonomy = DictIn.get(autonomy), TimeToCharge = DictIn.get(fastChargeTime),
        (update_truck(Id, Tare, Capacity, BateryCapacity, Autonomy,TimeToCharge, TruckJson), reply_json(TruckJson,  [status(200)]), !);
        reply_json(_, [status(500)]).

delete_truck(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        Id = DictIn.get(id),
        (remove_truck(Id), reply_json(Id,  [status(200)]), !);
        reply_json(_, [status(500)]).

getById_truck(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        Id = DictIn.get(id),
        (getById_truck(Id, TruckJson), reply_json(TruckJson,  [status(200)]), !);
        reply_json(_, [status(500)]).

% ---------------------------------------------------------------------------

create_delivery(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        Id = DictIn.get(deliveryID), LoadTime = DictIn.get(loadTime), UnloadTime = DictIn.get(unloadTime), Destination = DictIn.get(destination), Mass = DictIn.get(deliveryMass), Date = DictIn.get(deliveryDateProlog),
        (create_delivery(Id, Date, Mass, Destination, LoadTime, UnloadTime, DeliveryJson), reply_json(DeliveryJson,  [status(200)]), !);
        reply_json(_, [status(500)]).

update_delivery(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        Id = DictIn.get(deliveryID), Date = DictIn.get(deliveryDate), LoadTime = DictIn.get(loadTime), UnloadTime = DictIn.get(unloadTime), Destination = DictIn.get(destination), Mass = DictIn.get(deliveryMass),
        (update_delivery(Id, Date, Mass, Destination, LoadTime, UnloadTime, DeliveryJson), reply_json(DeliveryJson,  [status(200)]), !);
        reply_json(_, [status(500)]).

delete_delivery(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        Id = DictIn.get(id),
        (remove_delivery(Id), reply_json(Id,  [status(200)]), !);
        reply_json(_, [status(500)]).

getById_delivery(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        Id = DictIn.get(id),
        (getById_delivery(Id, DeliveryJson), reply_json(DeliveryJson,  [status(200)]), !);
        reply_json(_, [status(500)]).

% ---------------------------------------------------------------------------

create_path(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        Truck = DictIn.get(truck), CityOrig = DictIn.get(startWHId), CityDest = DictIn.get(destinationWHId), PathTime = DictIn.get(pathTravelTime), Energy = DictIn.get(wastedEnergy), AditionalTime = DictIn.get(extraTravelTime),
        (create_path(Truck, CityOrig, CityDest, PathTime, Energy, AditionalTime, PathJson), reply_json(PathJson,  [status(200)]), !);
        reply_json(_, [status(500)]).

update_path(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        Truck = DictIn.get(truck), OldCityOrig = DictIn.get(startWHId), OldCityDest = DictIn.get(destinationWHId), PathTime = DictIn.get(pathTravelTime), Energy = DictIn.get(wastedEnergy), AditionalTime = DictIn.get(extraTravelTime),
        (update_path(OldCityOrig, OldCityDest, Truck, PathTime, Energy, AditionalTime, PathJson), reply_json(PathJson,  [status(200)]), !);
        reply_json(_, [status(500)]).

delete_path(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        CityOrig = DictIn.get(cityOrig), CityDest = DictIn.get(cityDest),
        (remove_path(CityOrig, CityDest), reply_json(Id,  [status(200)]), !);
        reply_json(_, [status(500)]).

getById_path(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        Origin = DictIn.get(origin), Destination = DictIn.get(destination),
        (getById_path(Origin,Destination,PathJson), reply_json(PathJson,  [status(200)]), !);
        reply_json(_, [status(500)]).

% ---------------------------------------------------------------------------
create_warehouse(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        Id = DictIn.get(id), Address = DictIn.get(address), Altitude = DictIn.get(altitude), Latitude = DictIn.get(latitude),
        Longitude = DictIn.get(longitude), Designation = DictIn.get(designation), City = DictIn.get(city),
        (create_warehouse(Id, Address, Altitude, Latitude, Longitude, Designation, City, WarehouseJson), reply_json(WarehouseJson,  [status(200)]), !);
        reply_json(_, [status(500)]).

update_warehouse(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
		Id = DictIn.get(id), Address = DictIn.get(address), Altitude = DictIn.get(altitude), Latitude = DictIn.get(latitude),
        Longitude = DictIn.get(longitude), Designation = DictIn.get(designation), City = DictIn.get(city),
        (update_warehouse(Id, Address, Altitude, Latitude, Longitude, Designation, City, WarehouseJson), reply_json(WarehouseJson,  [status(200)]), !);
        reply_json(_, [status(500)]).


delete_warehouse(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        Id = DictIn.get(id),
        (remove_warehouse(Id), reply_json(Id,  [status(200)]), !);
        reply_json(_, [status(500)]).

getById_warehouse(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        Id = DictIn.get(id),
        (getById_warehouse(Id,WarehouseJson), reply_json(WarehouseJson,  [status(200)]), !);
        reply_json(_, [status(500)]).

% ---------------------------------------------------------------------------

bestPath_findAll(Request):-
        cors_enable,
        http_read_json_dict(Request, DictIn),
        Date = DictIn.get(date), Truck = DictIn.get(truck),
        (bestPath_findAll(Date, Truck, BestPathJson), reply_json(BestPathJson,  [status(200)]), !);
        reply_json(_, [status(500)]).

% ---------------------------------------------------------------------------

heuristic_mass(Request):-
        cors_enable,
        http_read_json_dict(Request, DictIn),
        Date = DictIn.get(date),
        (heuristic_mass(Date, PathResultJson), reply_json(PathResultJson,  [status(200)]), !);
        reply_json(_, [status(500)]).

% ---------------------------------------------------------------------------

heuristic_closestWarehouse(Request):-
        cors_enable,
        http_read_json_dict(Request, DictIn),
        Date = DictIn.get(date),
        (heuristic_closestWarehouse(Date, PathResultJson), reply_json(PathResultJson,  [status(200)]), !);
        reply_json(_, [status(500)]).

% ---------------------------------------------------------------------------

heuristic_massAndDistance(Request):-
        cors_enable,
        http_read_json_dict(Request, DictIn),
        Date = DictIn.get(date),
        (heuristic_massAndDistance(Date, PathResultJson), reply_json(PathResultJson,  [status(200)]), !);
        reply_json(_, [status(500)]).

% ---------------------------------------------------------------------------
server():-
        delete_base_de_conhecimento(),
        create_base_de_conhecimento(),
        http_server(http_dispatch, [port(3000)]),
        !.

stop():-
        http_stop_server(3000, []).