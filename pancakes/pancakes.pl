head([H|_],H).

tail(List,T):-
    reverse(List,Reverse),head(Reverse,T).

sublist([],_).
sublist(S,L):-
    append(S,_,L),
    S \= [].

firstN(_,0,[]).
firstN([E|L],N,[E|R]):-
    succ(N0,N),
    firstN(L,N0,R).

move(L1,L2,Operator):-
    sublist(Sub,L1),
    (\+length(Sub,0),\+length(Sub,1)), %those moves do nothing in our case
    tail(Sub,Operator),
    reverse(Sub,Rev),
    append(Sub,Rest,L1),append(Rev,Rest,L2).

dfs(Initial,Final,State,Operators,Operators,States,States):-
    State == Final.

dfs(Initial,Final,CurrentState,SoFarOperators,Operators,SoFarStates,States):-
    move(CurrentState,NewState,Operator),
    \+member(NewState,SoFarStates),
    append(SoFarStates,[NewState],NewSoFarStates),
    append(SoFarOperators,[Operator],NewOperators),
    dfs(Initial,Final,NewState,NewOperators,Operators,NewSoFarStates,States).

pancakes_dfs(Initial,Operators,States) :-
   sort(Initial,Final),
   dfs(Initial,Final,Initial,[],Operators,[Initial],States).
