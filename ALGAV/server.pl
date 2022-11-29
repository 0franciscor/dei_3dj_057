:- use_module(inc1).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(socket)).

:- http_handler(/, reply_root, []).
:- http_handler('/create_city', create_city,  []).
:- http_handler('/create_truck', create_truck, []).
:- http_handler('/create_delivery', create_delivery, []).
:- http_handler('/create_path', create_path, []).
:- http_handler('/update_city', update_city,  []).
:- http_handler('/update_truck', update_truck, []).
:- http_handler('/update_delivery', update_delivery, []).
:- http_handler('/update_path', update_path, []).
:- http_handler('/delete_city', delete_city,  []).
:- http_handler('/delete_truck', delete_truck, []).
:- http_handler('/delete_delivery', delete_delivery, []).
:- http_handler('/delete_path', delete_path, []).
:- http_handler('/count', count, []).
:- http_handler('/reset', resetar, []).

reply_root(_Request) :-
        format('Content-type: text/plain~n~n'),
        format('HTTP Server on SWI-Prolog is ready~n').
        
count(_Request):-
        cors_enable,
        count(LC, LT, LD, LP, TC, TT, TD, TP),
        format('Content-type: text/plain~n~n'),
        format('~w~n~w~n~w~n~w~n~w~n~w~n~w~n~w~n', [LC, LT, LD, LP, TC, TT, TD, TP]).
        
resetar(_Request):-
		cors_enable, (resetar(), reply_json(_, [status(200)])); reply_json(_, [status(500)]).
        
       
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
        (remove_city(Id), reply_json(_,  [status(200)]), !);
        reply_json(_, [status(500)]).
        
create_truck(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        Id = DictIn.get(id), Tare = DictIn.get(tare), Capacity = DictIn.get(capacity), BateryCapacity = DictIn.get(baterycapacity), Autonomy = DictIn.get(autonomy), TimeToCharge = DictIn.get(timeToCharge),
        (create_truck(Id, Tare, Capacity, BateryCapacity, Autonomy,TimeToCharge, TruckJson), reply_json(TruckJson,  [status(200)]), !);
        reply_json(_, [status(500)]).
        
update_truck(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        Id = DictIn.get(id), Tare = DictIn.get(tare), Capacity = DictIn.get(capacity), BateryCapacity = DictIn.get(baterycapacity), Autonomy = DictIn.get(autonomy), TimeToCharge = DictIn.get(timeToCharge),
        (update_truck(Id, Tare, Capacity, BateryCapacity, Autonomy,TimeToCharge, TruckJson), reply_json(TruckJson,  [status(200)]), !);
        reply_json(_, [status(500)]).
        
delete_truck(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        Id = DictIn.get(id),
        (remove_truck(Id), reply_json(_,  [status(200)]), !);
        reply_json(_, [status(500)]).
        
create_delivery(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        Id = DictIn.get(id), Date = DictIn.get(date), Mass = DictIn.get(deliveryMass), Destination = DictIn.get(destination), LoadTime = DictIn.get(loadTime), UnloadTime = DictIn.get(unloadTime),
        (create_delivery(Id, Date, Mass, Destination, LoadTime, UnloadTime, DeliveryJson), reply_json(DeliveryJson,  [status(200)]), !);
        reply_json(_, [status(500)]).

update_delivery(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        Id = DictIn.get(id), Date = DictIn.get(date), Mass = DictIn.get(deliveryMass), Destination = DictIn.get(destination), LoadTime = DictIn.get(loadTime), UnloadTime = DictIn.get(unloadTime),
        (update_delivery(Id, Date, Mass, Destination, LoadTime, UnloadTime, DeliveryJson), reply_json(DeliveryJson,  [status(200)]), !);
        reply_json(_, [status(500)]).
        
delete_delivery(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        Id = DictIn.get(id),
        (remove_delivery(Id), reply_json(_,  [status(200)]), !);
        reply_json(_, [status(500)]).
        
create_path(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        Truck = DictIn.get(truck), CityOrig = DictIn.get(cityOrig), CityDest = DictIn.get(cityDest), PathTime = DictIn.get(pathTime), Energy = DictIn.get(energy), AditionalTime = DictIn.get(aditionalTime),
        (create_path(Truck, CityOrig, CityDest, PathTime, Energy, AditionalTime, PathJson), reply_json(PathJson,  [status(200)]), !);
        reply_json(_, [status(500)]).
        
update_path(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        OldCityOrig = DictIn.get(OldCityOrig), OldCityDest = DictIn.get(OldCityDest), Truck = DictIn.get(truck), PathTime = DictIn.get(pathTime), Energy = DictIn.get(energy), AditionalTime = DictIn.get(aditionalTime),
        (update_path(OldCityOrig, OldCityDest, Truck, PathTime, Energy, AditionalTime), reply_json(PathJson,  [status(200)]), !);
        reply_json(_, [status(500)]).

delete_path(Request):-
		cors_enable,
        http_read_json_dict(Request, DictIn),
        CityOrig = DictIn.get(cityOrig), CityDest = DictIn.get(cityDest),
        (remove_path(CityOrig, CityDest), reply_json(_,  [status(200)]), !);
        reply_json(_, [status(500)]).
        
:- Port is 2228,
gethostname(Host),
format('~nHTTP server ready:  http://~w:~w ~n~n',[Host,Port]),
http_daemon([port(Port),user(root),fork(false)]).
