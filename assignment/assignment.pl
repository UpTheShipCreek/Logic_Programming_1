activity(a01, act(0,3)).
activity(a02, act(0,4)).
activity(a03, act(1,5)).
activity(a04, act(4,6)).
activity(a05, act(6,8)).
activity(a06, act(6,9)).
activity(a07, act(9,10)).
activity(a08, act(9,13)).
activity(a09, act(11,14)).
activity(a10, act(12,15)).
activity(a11, act(14,17)).
activity(a12, act(16,18)).
activity(a13, act(17,19)).
activity(a14, act(18,20)).
activity(a15, act(19,20)).

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

assignment(NPersons,Assignment):-
    gatherActivities(AIds),
    assign(AIds,NPersons,Assignment).

assign([],_,[]):-!.
assign([AId|AIds],NPersons,[AId-PId|Assignment]):-
    assign(AIds,NPersons,Assignment),
    pickWorker(PId,NPersons),
    activity(AId,act(Ab,Ae)),
    gatherWorkerActivities(PId,Assignment,APIds),
    valid(Ab,Ae,APIds). % Is current assignment consistent with previous ones

valid(_,_,[]).
valid(Ab1,Ae1,[APId|APIds]):-
    activity(APId,act(Ab2,Ae2)),
    Ab1 >= Ae2+1,
    valid(Ab1,Ae1,APIds),!.
