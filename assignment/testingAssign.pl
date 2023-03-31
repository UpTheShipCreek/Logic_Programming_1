head([H|_],H).

min(X,Y,Y):-
    X>=Y.
min(X,Y,X):-
    X<Y.
max(X,Y,X):-
    X>=Y.
max(X,Y,Y):-
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
assign([AId|AIds], NPersons, MaxHours, [AId-PId|Assignment]) :-
    assign(AIds, NPersons, MaxHours, Assignment),
    length([AId|AIds],L), %in the first assignation we only have 1 distinct option
    max(L,1,L1),
    min(NPersons,L1,M), %in the second two, in the third three and soon, up until our N
    pickWorker(PId,M),
    activity(AId, act(Ab, Ae)),
    gatherWorkerActivities(PId, Assignment, APIds),
    valid(Ab, Ae, APIds, MaxHours).


valid(_,_,[],_).
valid(Ab1,Ae1,[APId|APIds],MaxHours):-
    addTimes([APId|APIds],Hours),
    Time is Ae1 - Ab1,
    OT is Hours + Time,
    OT =< MaxHours,
    activity(APId,act(_,Ae2)),
    Ab1 > Ae2,
    valid(Ab1,Ae1,APIds,MaxHours).


%assign(AIds,NPersons,MaxHours,Assignmnet):-
%    assign(AIds,AIds,NPersons,MaxHours,[],Assignmnet).
%assign(_,[],_,_,Assignment,Assignment):-!.
% assign(AIds,[AId|Activities],NPersons,MaxHours,SoFarAssignments,Assigments):-
%    length(SoFarAssignments,L),
%    L1 is L + 1,
%    min(NPersons,L1,M),
%    pickWorker(PId,M),
%   activity(AId,act(Ab,Ae)),
%    gatherWorkerActivities(PId,SoFarAssignments,APIds),
%    valid(Ab,Ae,APIds,MaxHours),
%    assign(AIds,Activities,NPersons,MaxHours,[AId-PId|SoFarAssignments],Assigments).

