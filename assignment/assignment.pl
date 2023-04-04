head([H|_],H).

myMin(X,Y,Y):-
    X>=Y.
myMin(X,Y,X):-
    X<Y.

myMax(X,Y,X):-
    X>=Y.
myMax(X,Y,Y):-
    X<Y.

gatherActivities(AIds):-
    gatherActivities(AIds,[]),!.
gatherActivities(AIds,B):-
    activity(Id,_),
    \+member(Id,B),
    gatherActivities(AIds,[Id|B]).
gatherActivities(AIds,AIds).

pickWorker(N,N).
pickWorker(W,N):-
    N>1,
    N0 is N - 1,
    pickWorker(W,N0).

gatherWorkerActivities(_,[],[]).
gatherWorkerActivities(PId,[AId-PId|Assigment],[AId|Activities]):-
    gatherWorkerActivities(PId,Assigment,Activities),!.
gatherWorkerActivities(PId,[_-DId|Assigment],Activities):-
    PId =\= DId,
    gatherWorkerActivities(PId,Assigment,Activities),!.

findMaxIndex(Assignment,Max):-
    findMaxIndex(Assignment,Assignment,0,Max).

findMaxIndex(_,[],Max,Max).
findMaxIndex(As,[_-Index|Assignment],Temp,Max):-
    myMax(Index,Temp,NT),
    findMaxIndex(As,Assignment,NT,Max),!.

addTimes(APIds,R):-addTimes(APIds,APIds,R,0).
addTimes(_,[],R,R):-!.
addTimes(APIds,[H|A],R,C):-
    activity(H,act(Start,Finish)),
    Time is Finish - Start,
    NC is C + Time,
    addTimes(APIds,A,R,NC).

check(PId,AId,Assignment,MaxTime):-
    activity(AId, act(Ab, Ae)),
    gatherWorkerActivities(PId, Assignment, APIds),
    NewTime is Ae - Ab,
    addTimes(APIds,SoFarTime),
    OverallTime is NewTime + SoFarTime,
    OverallTime =< MaxTime,
    valid(Ab, Ae, APIds).

toASPFormatList(ASA, N, List) :-
    findall(X, toASPFormat(ASA, N, X), List).

toASPFormat(ASA,N,ASP):-
    pickWorker(W,N),
    gatherWorkerActivities(W,ASA,Activities),
    addTimes(Activities,Time),
    ASP = W-Activities-Time.

assignment(NPersons,MaxTime,ASP,ASA):-
    gatherActivities(AIds),
    assign(AIds,NPersons,MaxTime,ASA),
    toASPFormatList(ASA,NPersons,ASP).

assign(AIds,NPersons,MaxTime,Assignment):-
    assign(AIds,AIds,NPersons,MaxTime,[],Assignment).
assign(_,[],_,_,Assignment,Assignment).
assign(A,[AId|AIds],NPersons,MaxTime,SoFarAssignment,Assignment):-
    findMaxIndex(SoFarAssignment,SoFarMax), %we must not assign the "(n+1)-nth" worker before the "n-nth", if we are to exclude mirror solutions. If we need someone different from the previous "(n-1)" workers, we shall always call him the "n-nth".
    succ(SoFarMax,S),
    myMin(S,NPersons,M),
    pickWorker(PId,M),
    check(PId,AId,SoFarAssignment,MaxTime),
    assign(A,AIds,NPersons,MaxTime,[AId-PId|SoFarAssignment],Assignment).

valid(_,_,[]).
valid(Ab1,Ae1,[APId|APIds]):-
    activity(APId,act(Ab2,Ae2)),
    (
        Ab1 > Ae2; Ab2 > Ae1
    ),
    valid(Ab1,Ae1,APIds).
