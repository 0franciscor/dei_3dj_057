:- module(inc1, [add_city/3, update_city/3, remove_city/1,
create_truck/7, update_truck/7, remove_truck/1,
create_delivery/7, update_delivery/7, remove_delivery/1,
create_path/7, update_path/7, remove_path/2,
count/8, resetar/0]).
                
:- use_module(library(http/json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json_convert)).
:- use_module(library(apply)).

:- dynamic idArmazem/2.
:- dynamic caracteristicasCam/6.
:- dynamic entrega/6.
:- dynamic dadosCam_t_e_ta/6.

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
	\+dadosCam_t_e_ta(_, CityOrig, CityDest, _, _, _), assertz(dadosCam_t_e_ta(Truck, CityOrig, CityDest, PathTime, Energy, AditionalTime)).
    
update_path(Truck, CityOrig, CityDest, PathTime, Energy, AditionalTime, PathJson):-
	dadosCam_t_e_ta(_, CityOrig, CityDest, _, _, _), retract(dadosCam_t_e_ta(_, CityOrig, CityDest, _, _, _)), assertz(dadosCam_t_e_ta(Truck, CityOrig, CityDest, PathTime, Energy, AditionalTime)).
    
remove_path(CityOrig, CityDest):-
	dadosCam_t_e_ta(_, CityOrig, CityDest, _, _, _), retract(dadosCam_t_e_ta(_, CityOrig, CityDest, _, _, _)).

% ---------------------------------------------------------------------------
pathprolog_tojson(Truck, CityOrig, CityDest, PathTime, Energy, AditionalTime, NoJson):-
		atom_string(Truck, TruckJs), atom_string(CityOrig, CityOrigJs), atom_string(CityDest, CityDestJs), atom_string(PathTime, PathTimeJs), atom_string(Energy, EnergyJs), atom_string(AditionalTime, AditionalTimeJs), 
		NoJson=json([truck = TruckJs, origin = CityOrigJs, destination = CityDestJs, totalTime = PathTimeJs, energy = EnergyJs, aditionalTime = aditionalTime]).

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