head([H|_],H).

min(X,Y,Y):-
    X>=Y.
min(X,Y,X):-
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

addTimes(APIds,R):-addTimes(APIds,APIds,R,0).
addTimes(_,[],R,R):-!.
addTimes(APIds,[H|A],R,C):-
    activity(H,act(Start,Finish)),
    Time is Finish - Start,
    NC is C + Time,
    addTimes(APIds,A,R,NC).

assignment(NPersons,MaxHours,Assignment,Assignment):-
    gatherActivities(AIds),
    assign(AIds,NPersons,MaxHours,Assignment).

assign([],_,_,[]).
assign([AId|AIds], NPersons, MaxTime, [AId-PId|Assignment]) :-
    assign(AIds, NPersons, MaxTime, Assignment),
    length([AId|AIds],L), %in the first assignation we have only 1 possible distinct option
    min(NPersons,L,M), %in the second two, in the third three and soon, up until our N
    pickWorker(PId,M), %so following the opposite direction, in the last and until our N
    activity(AId, act(Ab, Ae)), %we have our N (max) assignations available, then N-1, N-2
    gatherWorkerActivities(PId, Assignment, APIds), %which is the size of the "so-far" assignation list
    NewTime is Ae - Ab, %until we reach an empty list
    addTimes(APIds,SoFarTime),
    OverallTime is NewTime + SoFarTime,
    OverallTime =< MaxTime,
    valid(Ab, Ae, APIds).

valid(_,_,[]).
valid(Ab1,Ae1,[APId|APIds]):-
    activity(APId,act(Ab2,Ae2)),
    (
        Ab1 > Ae2; Ab2 > Ae1
    ),
    valid(Ab1,Ae1,APIds).
