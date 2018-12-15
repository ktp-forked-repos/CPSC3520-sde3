/*
	Name: Matthew Lowery
	Date: 12/5/2018
	Course: CPSC3520
	Instructor: Dr. Robert Schalkoff
	Assignment: sde3 - cmeans implementation in SWI-Prolog
*/

/* Required predicates */
distanceR2([],[],Dsq) :- Dsq is 0.
distanceR2([A|B],[C|D],Dsq) :-
    distanceR2(B,D,Z),
    I is A-C,
    J is I*I,
    Dsq is Z+J.
    
distanceSqAllMeans(_,[],[]).
distanceSqAllMeans(A,[C|D],[E|F]) :-
    distanceR2(A,C,E),
    distanceSqAllMeans(A,D,F).
    
listMin([A|B],M) :- 
    listMin(B,A,M).
listMin([],M,M).
listMin([A|B],Min0,M) :-
    Min1 is min(A,Min0),
    listMin(B,Min1,M).
    

listMinPos([A|B],M) :-
    listMin(B,A,N),
    listMinPos([A|B],N,M).
listMinPos([N|_],N,M) :- M is 0.
listMinPos([_|B],N,M) :-
    listMinPos(B,N,M1),
    M is M1+1.
    
    
elsum([L1],[L2],[S]) :- S is L1 + L2.
elsum([A|B],[C|D],[I|J]) :- elsum(B,D,J),I is A + C.

scaleList([A|B],0,[E|F]) :- 
    scaleList(B,1,F),
    E is A / 1.
scaleList([List],Scale,[Answer]) :-
    Answer is List / Scale.
scaleList([A|B],Scale,[E|F]) :- 
    scaleList(B,Scale,F),
    E is A / Scale.
    
zeroes(0,[]).
zeroes(1,[TheList]) :- TheList is 0.0.
zeroes(Size,[A|B]) :-
    X is Size-1,
    zeroes(X,B), A is 0.0.

zeroMeansSet(1,Dim,[TheSet]) :-
    zeroes(Dim,TheSet).
zeroMeansSet(Cmeans,Dim,[A|B]) :-
    X is Cmeans-1,
    zeroMeansSet(X,Dim,B),
    zeroes(Dim,A).

zeroVdiff([V1],[V2]) :-
    V1 = V2.
zeroVdiff([A|B],[C|D]) :-
    zeroVdiff(B,D), A = C.
    
zeroSetDiff([S1],[S2]) :-
    S1 = S2.
zeroSetDiff([A|B],[C|D]) :-
    zeroSetDiff(B,D),
    zeroVdiff([A],[C]).

zeroCounts(1,[CountsList]) :- CountsList is 0.
zeroCounts(C,[A|B]) :-
    X is C-1,
    zeroCounts(X,B), A is 0.
    
updateCounts(0,[A|B],[C|B]) :-
    C is A+1.
updateCounts(P,[A|B],[A|D]) :-
    X is P-1,
    updateCounts(X,B,D).
    
updateMeansSum(V,0,[A|B],[C|B]) :- 
    elsum(V,A,C).
updateMeansSum(V,P,[A|B],[A|D]) :-
    X is P-1,
    updateMeansSum(V,X,B,D).

formNewMeans([],[],[]).
formNewMeans([A|B],[C|D],[E|F]) :-
    scaleList(A,C,E),
    formNewMeans(B,D,F).

/* helper predicate to find dimension and c from Currmeans */
dimension([],D) :-
    D is 0.
dimension([_|B],D) :-
    dimension(B,X), D is X+1.

/* helper predicate to initialize counts and meanssum to zeroes */
initZeroes([],[],[]).
initZeroes([A|B],NewCounts) :-
    dimension([A|B],C),
    zeroCounts(C,NewCounts).
    

reclassify([A|B],Currmeans,UpdatedMeans) :-
    initZeroes(Currmeans,NewC),
    distanceSqAllMeans(A,Currmeans,X),
    listMinPos(X,Y),
    updateCounts(Y,NewC,NewC1),
    updateMeansSum(A,Y,Currmeans,NewMS),
    formNewMeans(NewMS,NewC1,NewMeans),
    reclassify(B,NewMeans,NewC1,NewMS,UpdatedMeans).
reclassify([],_,NewC,NewMS,UpdatedMeans) :-
    formNewMeans(NewMS,NewC,UpdatedMeans).
reclassify([A|B],Currmeans,NewC,NewMS,UpdatedMeans) :-
    distanceSqAllMeans(A,Currmeans,X),
    listMinPos(X,Y),
    updateCounts(Y,NewC,NewC1),
    updateMeansSum(A,Y,NewMS,NewMS1),
    formNewMeans(NewMS1,NewC1,NewMeans),
    reclassify(B,NewMeans,NewC1,NewMS1,UpdatedMeans).
    
cmeans(_,A,A).
cmeans(H,MZ0,MuFinal) :-
    reclassify(H,MZ0,MuCurrent),
    zeroSetDiff(MZ0,MuCurrent),
    cmeans(H,MuCurrent,MuFinal).
