:-consult('bc_armazens.pl').

allPaths(L, LF) :-(checkIfExistCity(L),!,deleteMatosinhos(5,L,L2),findall(LAux,
permutation(L2,LAux),L3),appendMatosinhos([5],L3,LF)); write('One of the entered coordinates does not correspond to a warehouse that is in the system.').

deleteMatosinhos(A,L,L1):- ((member(A,L),!,delete(L,A,L1)); true).

checkIfExistCity([]):-!.
checkIfExistCity([H|T]):- ((idArmazem(_,H),!,checkIfExistCity(T));false).

appendMatosinhos(_,[],[]):-!.
appendMatosinhos(A,[L|LL],[L2|T]):- appendMatosinhos(A,LL,T), append(A,L,L1), append(L1,A,L2).


