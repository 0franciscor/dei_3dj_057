
allPaths(L, LF) :- deleteMatosinhos(5,L,L2),findall(LAux, permutation(L2,LAux),L3),appendMatosinhos([5],L3,LF).

deleteMatosinhos(A,L,L1):- ((member(A,L),!,delete(L,A,L1)); true).

appendMatosinhos(_,[],[]):-!.
appendMatosinhos(A,[L|LL],[L2|T]):- appendMatosinhos(A,LL,T), append(A,L,L1), append(L1,A,L2).


