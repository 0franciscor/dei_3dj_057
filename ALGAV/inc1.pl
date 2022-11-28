:- module(inc1, [create_city/2, update_city/2, remove_city/1,
create_truck/6, update_truck/6, remove_truck/1,
create_delivery/6, update_delivery/6, remove_delivery/1,
create_path/6, update_path/6, remove_path/2,
count/8, resetar/0]).
                
:- use_module(library(http/json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json_convert)).
:- use_module(library(apply)).

:- dynamic idArmazem/2.
:- dynamic caracteristicasCam/6.
:- dynamic entrega/6.
:- dynamic dadosCam_t_e_ta/6.

% -------------------- City Basics ------------------------------------------

create_city(Id, Name):- 
	\+idArmazem(_, Id), assertz(idArmazem(Name, Id)).
    
update_city(Id, Name):- 
	idArmazem(_, Id), retract(idArmazem(_, Id)), assertz(idArmazem(Name, Id)).
    
remove_city(Id):- 
	idArmazem(_, Id), retract(idArmazem(_, Id)).

% -------------------- Truck Basics ------------------------------------------

create_truck(Id, Tare, Capacity, BateryCapacity, Autonomy,TimeToCharge):- 
	\+caracteristicasCam(Id, _, _, _, _, _), assertz(caracteristicasCam(Id, Tare, Capacity, BateryCapacity, Autonomy, TimeToCharge)).
    
update_truck(Id, Tare, Capacity, BateryCapacity, Autonomy,TimeToCharge):- 
	caracteristicasCam(Id, _, _, _, _, _), retract(caracteristicasCam(Id, _, _, _, _, _)), assertz(caracteristicasCam(Id, Tare, Capacity, BateryCapacity, Autonomy, TimeToCharge)).
    
remove_truck(Id):- 
	caracteristicasCam(Id, _, _, _, _, _), retract(caracteristicasCam(Id, _, _, _, _, _)).

% -------------------- Delivery Basics ------------------------------------------

create_delivery(Id, Date, Mass, Destination, LoadTime, UnloadTime):-
	\+entrega(Id, _, _, _, _, _), assertz(entrega(Id, Date, Mass, Destination, LoadTime, UnloadTime)).
    
update_delivery(Id, Date, Mass, Destination, LoadTime, UnloadTime):-
	entrega(Id, _, _, _, _, _), retract(entrega(Id, _, _, _, _, _)), assertz(entrega(Id, Date, Mass, Destination, LoadTime, UnloadTime)).
    
remove_delivery(Id):-
	entrega(Id, _, _, _, _, _), retract(entrega(Id, _, _, _, _, _)).

% -------------------- Path Basics ------------------------------------------

create_path(Truck, CityOrig, CityDest, PathTime, Energy, AditionalTime):-
	\+dadosCam_t_e_ta(_, CityOrig, CityDest, _, _, _), assertz(dadosCam_t_e_ta(Truck, CityOrig, CityDest, PathTime, Energy, AditionalTime)).
    
update_path(Truck, CityOrig, CityDest, PathTime, Energy, AditionalTime):-
	dadosCam_t_e_ta(_, CityOrig, CityDest, _, _, _), retract(dadosCam_t_e_ta(_, CityOrig, CityDest, _, _, _)), assertz(dadosCam_t_e_ta(Truck, CityOrig, CityDest, PathTime, Energy, AditionalTime)).
    
remove_path(CityOrig, CityDest):-
	dadosCam_t_e_ta(_, CityOrig, CityDest, _, _, _), retract(dadosCam_t_e_ta(_, CityOrig, CityDest, _, _, _)).

% ----------------------------------------------------------------------------

count(LC, LT, LD, LP, TC, TT, TD, TP):-
		findall((_,_), idArmazem(_,_), LC),
        findall((_,_,_,_,_,_), caracteristicasCam(_,_,_,_,_,_), LT),
        findall((_,_,_,_,_,_), entrega(_,_,_,_,_,_), LD),
        findall((_,_,_,_,_,_), dadosCam_t_e_ta(_,_,_,_,_,_), LP),
        length(LC, TC),
        length(LT, TT),
        length(LD, TD),
        length(LP, TP).
        
resetar():-
	retractall(idArmazem(_,_)).