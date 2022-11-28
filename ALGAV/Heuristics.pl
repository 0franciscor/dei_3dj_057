:-consult('US1.pl').


% Largest Mass first Heuristic %
extractMass([X],[M]):- entrega(X,_,M,_,_,_).
extractMass([H|T],[H1|T1]):- extractMass(T,T1), entrega(H,_,H1,_,_,_).

largestMassFirst([H|T],L):- extractMass([H|T],M), sort(M, M1), reverse(M1, L).
