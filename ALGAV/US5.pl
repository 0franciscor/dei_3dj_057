:-consult('US1.pl').


update(H, H1, IDTRUCK, [X|Y],BAT, TE, NDL):-energy(IDTRUCK, H, H1, [X|Y], E), deschargeTruck(BAT,E, TE),
    deleteDelivery(H1, [X|Y], NDL).


 
findMin([_|[]],_,_,T):-!
findMin([H|[H1|T]],IDTRUCK, [X|Y],  STIME, WHOUSE ):-  calculateTime(H, H1, IDTRUCK, [X|Y],T), WHOUSE IS H1,
    findMin([H|T], IDTRUCK, [X|Y], ST, WH), ((T < ST, STIME IS T, WHOUSE IS WH); STIME IS ST). 


calculateTime(H, H1, IDTRUCK, [X|Y], TIME):- tempo(IDTRUCK,H,H1,[X|Y],T), unloadTime([X|T],UNLOADTIME),
    checkIfCharges(IDTRUCK,[X|Y], H1,T,TE,_,CHARGETIME),
    ((CHARGETIME>=UNLOADTIME, T1 IS CHARGETIME); T1 IS UNLOADTIME), TIME is T + T1

  
removeWH(WH, [H|T], NL):- removeFromDel(WH, [H|T], NL)


calculateMin([H|T], IDTRUCK, [X|Y],BAT,T,W, TRUCKENERGY, NEWDELIVERYLIST,NEWWAREHOUSELIST):-findMin([H|T], IDTRUCK, [X|Y], T,W),
    update(H, W, IDTRUCK, [X|Y],BAT, TRUCKENERGY, NEWDELIVERYLIST), removeWH(H,[H|T],NEWWAREHOUSELIST)


Minorante([_|[]], _,_,_,_,_,_,_):-! 
Minorante([H|T],IDTRUCK,[X|Y], BATERY, TENERGY, NDLIST, WHLIST,  TOTALTIME, LIST,LIST1  ):- 
    calculateMin([H|T], IDTRUCK, [X|Y],BAT, TIME, WH,TE, NDL, NWL),  
    Minorante(NWL, IDTRUCK,NDL, BAT, T ,WAREHOUSE,TE,NDL,WHLIST,TOTALTIME, L, L1), TOTALTIME1 IS TOTALTIME + TIME, append(WAREHOUSE, L1, L).



findMax([_|[]],_,_,T):-!
findMax([H|[H1|T]],IDTRUCK, [X|Y],  STIME, WHOUSE ):-  calculateTime(H, H1, IDTRUCK, [X|Y],T), WHOUSE IS H1,
    findMax([H|T], IDTRUCK, [X|Y], ST, WH), ((T > ST, STIME IS T, WHOUSE IS WH); STIME IS ST). 


calculateMax([H|T], IDTRUCK, [X|Y],BAT,T,W, TRUCKENERGY, NEWDELIVERYLIST,NEWWAREHOUSEL):- findMax([H|T], IDTRUCK, [X|Y], T,W),
    update(H, W, IDTRUCK, [X|Y],BAT, TRUCKENERGY, NEWDELIVERYLIST), removeWH(H,[H|T],NEWWAREHOUSELIST).

Majorante([_|[]], _,_,_,_,_,_,_):-!
Majorante([H|T],IDTRUCK,[X|Y], BATERY, TENERGY, NDLIST, WHLIST,  TOTALTIME, LIST,LIST1 ):- 
    calculateMax([H|T], IDTRUCK, [X|Y],BAT, TIME, WH,TE, NDL, NWL), 
    Majorante(NWL, IDTRUCK,NDL, BAT, T ,WAREHOUSE,TE,NDL,WHLIST,TOTALTIME, L, L1), TOTALTIME1 IS TOTALTIME + TIME, append(WAREHOUSE, L1, L).


