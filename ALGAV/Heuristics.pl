:-consult('US1.pl').


% Largest Mass first Heuristic %
extractMass([X],[M]):- entrega(X,_,M,_,_,_).
extractMass([H|T],[H1|T1]):- extractMass(T,T1), entrega(H,_,H1,_,_,_).

extractWarehouse([],[Matosinhos]):- findMatosinhos(Matosinhos).
extractWarehouse([H|T],[H1|T1]):- extractWarehouse(T,T1), entrega(_,_,H,H1,_,_).

largestMassFirst(E,L):- extractMass(E,M), sort(M, M1), reverse(M1, M2), extractWarehouse(M2,L1), findMatosinhos(Matosinhos),append([Matosinhos],L1,L).

