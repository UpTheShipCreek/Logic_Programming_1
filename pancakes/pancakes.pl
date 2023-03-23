head([H|_],H).
tail(List,T):-
    reverse(List,Reverse),head(Reverse,T).

sorted([_]):-!.
sorted([E|List]):-
    head(List,H),
    E=<H,
    sorted(List).

sublist([],_).
sublist(S,L):-
    append(S,_,L),
    S \= [].

final_state(State):-
    sorted(State).

move(L1,L2,Operator):-
    sublist(Sub,L1),
    (\+length(Sub,0),\+length(Sub,1)), %those moves do nothing in our case
    tail(Sub,Operator),
    reverse(Sub,Rev),
    append(Sub,Rest,L1),append(Rev,Rest,L2).

dfs(Initial,State,Operators,Operators,States,States):-
    final_state(State),!.

dfs(Initial,CurrentState,SoFarOperators,Operators,SoFarStates,States):-
    move(CurrentState,NewState,Operator),
    \+member(NewState,SoFarStates),
    append(SoFarStates,[NewState],NewSoFarStates),
    append(SoFarOperators,[Operator],NewOperators),
    dfs(Initial,NewState,NewOperators,Operators,NewSoFarStates,States).

pancakes_dfs(Initial,Operators,States) :-
   dfs(Initial,Initial,[],Operators,[Initial],States).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pancakes_ids(Initial,Operators,States):-
    pancakes_ids_iter(Initial,Initial,0,[],Operators,[Initial],States).

pancakes_ids_iter(Initial,Initial,Limit,[],Operators,[Initial],States):-
    ids(Initial,Initial,Limit,[],Operators,[Initial],States),!.

pancakes_ids_iter(Initial,Initial,Limit,[],Operators,[Initial],States):-
    NLimit is Limit + 1,
    pancakes_ids_iter(Initial,Initial,NLimit,[],Operators,[Initial],States).

ids(Initial,State,Limit,Operators,Operators,States,States):-
    final_state(State),!.

ids(Initial,CurrentState,Limit,SoFarOperators,Operators,SoFarStates,States):-
    length(SoFarOperators,Length), Length<Limit,
     move(CurrentState,NewState,Operator),
    \+member(NewState,SoFarStates),
    append(SoFarStates,[NewState],NewSoFarStates),
    append(SoFarOperators,[Operator],NewOperators),
    ids(Initial,NewState,Limit,NewOperators,Operators,NewSoFarStates,States).




