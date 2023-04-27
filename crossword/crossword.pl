:-debug.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% General_Purpose %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
head([H|_],H).
tail([_|T],T).

not_inst(X):-       %instatiation check
   \+(\+(X=0)),
   \+(\+(X=1)).

index(_, [], _):-fail.
index(Element,[Element|_],1).
index(Element,[_|List],Index):-
  index(Element,List,Index0),succ(Index0,Index).

mergesort([], []).
mergesort([A], [A]).
mergesort([A, B | Rest], S) :-
  divide([A, B | Rest], L1, L2),
  mergesort(L1, S1),
  mergesort(L2, S2),
  myMerge(S1, S2, S).
divide([], [], []).
divide([A], [A], []).
divide([A, B | R], [A | Ra], [B | Rb]) :-  divide(R, Ra, Rb).
myMerge(A, [], A).
myMerge([], B, B).
myMerge([A | Ra], [B | Rb], [A | M]) :-
  A=(TA,_),B=(TB,_),
  TA >= TB,
  myMerge(Ra, [B | Rb], M).
myMerge([A | Ra], [B | Rb], [B | M]) :-
  A=(TA,_),B=(TB,_),
  TA < TB,
  myMerge([A | Ra], Rb, M).

count_to(N,N).
count_to(C,N):-     %C takes all int values starting at N and counting down till 1
    N>1,
    N0 is N - 1,
    count_to(C,N0).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% General_Purpose %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% Block_Manipulation %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
block_inst(R,C):-   %Block check for the instantiated blocks
    \+not_inst(R), \+not_inst(C),  %need to be instantiated
    R>0,C>0,
    dimension(N),
    R=<N,C=<N,!.
block_R(R,Y):-      %Find all blocks possible in a certain row
    \+not_inst(R),not_inst(Y),
    dimension(N),
    count_to(Y,N).
block_C(X,C):-      %Find all blocks possible in a certain column
    \+not_inst(C),not_inst(X),
    dimension(N),
    count_to(X,N).
block_not_inst(X,Y):-  %Find all blocks possible
    not_inst(X), not_inst(Y),
    dimension(N),
    count_to(X,N),
    count_to(Y,N).
block(X,Y):-        %General block function
    (\+not_inst(X), \+not_inst(Y))->block_inst(X,Y);
    (\+not_inst(X),not_inst(Y))->block_R(X,Y);
    (\+not_inst(Y),not_inst(X))->block_C(X,Y);
    (not_inst(X),not_inst(Y))->block_not_inst(X,Y).

white(Row,Column):- %Finds all white blocks or checks if a block is white
    block(Row,Column),
    \+black(Row,Column).


cont_h(white(X,Y),white(X,Y1)):-
    succ(Y,Y1).

cont_v(white(X,Y),white(X1,Y)):-
    succ(X,X1).

find_cont(List,ListoLists):-
    find_cont(List,List,[],ListoLists).
    
find_cont(_,[],ListoLists,ListoLists).
find_cont(_,[W],[],[]):- \+head(W,_).
find_cont(List,[W],[HLL|RLL],ListoLists):-
   length(HLL,Length1),Length1>0,
   reverse(HLL,THLL),head(THLL,Previous_W),(
   cont_h(Previous_W,W),append(HLL,[W],NewHLL),find_cont(List,[],[NewHLL|RLL],ListoLists);
   \+cont_h(Previous_W,W),find_cont(List,[],[[W]|[HLL|RLL]],ListoLists)).
find_cont(List,[W|RWhites],[],ListoLists):-
    length(RWhites,Length),Length>0,head(RWhites,Next_W),(
    cont_h(W,Next_W),find_cont(List,RWhites,[[W]],ListoLists);
    \+cont_h(W,Next_W),find_cont(List,RWhites,[],ListoLists)).
find_cont(List,[W|RWhites],[HLL|RLL],ListoLists):-
    length(HLL,Length1),Length1>0,length(RWhites,Length2),Length2>0,
    head(RWhites,Next_W),reverse(HLL,THLL),head(THLL,Previous_W),(
    cont_h(Previous_W,W),append(HLL,[W],NewHLL),find_cont(List,RWhites,[NewHLL|RLL],ListoLists);
    \+cont_h(Previous_W,W),cont_h(W,Next_W),find_cont(List,RWhites,[[W]|[HLL|RLL]],ListoLists);
    \+cont_h(Previous_W,W),\+cont_h(W,Next_W),find_cont(List,RWhites,[HLL|RLL],ListoLists)).

find_cont_h([],[[]]). %tries to break the list of all possible slots into continuous sublists aka words
find_cont_h([X,Y],[[X,Y|L]|R]):-
   cont_h(X,Y),
   find_cont_h([],[L|R]),!.
find_cont_h([X,Y|T],[[X|L]|R]):-
   length(T,Length),Length>0,
   cont_h(X,Y),
   find_cont_h([Y|T],[L|R]).
find_cont_h([X,Y],[[X]]):-
   \+cont_h(X,Y),
   find_cont_h([],[[]]).
find_cont_h([X,Y|T],[H|R]):-
   length(T,Length),Length>0,
   \+cont_h(X,Y),reverse(H,Reverse),head(Reverse,OH),(
   cont_h(OH,X)->append(X,NH,H),find_cont_h([Y|T],[NH|R]);
   \+cont_h(OH,X)->find_cont_h([Y|T],[H|R])).

find_cont_v([],[]). %tries to break the list of all possible slots into continuous sublists aka words
find_cont_v([_],[]).
find_cont_v([X,Y|T],[[X|L]|R]):-
   cont_v([X,Y]),
   find_cont_v([Y|T],[L|R]).
find_cont_v([X,Y|T],[[X]|R]):-
   \+cont_v([X,Y]),
   find_cont_v([Y|T],R).

horizontal_slots(List):-dimension(N),horizontal_slots(List,[],N). %creates a list including all the viable slots aka white blocks
horizontal_slots(List,List,0):-!.
horizontal_slots(L,List,C):-
    findall(white(C,X),white(C,X),E),
    reverse(E,R),
    succ(C0,C),
    horizontal_slots(L,[R|List],C0).

vertical_slots(List):-dimension(N), vertical_slots(List,[],N).
vertical_slots(List,List,0):-!.
vertical_slots(L,List,C):-
    findall(white(X,C),white(X,C),E),
    reverse(E,R),
    succ(C0,C),
    vertical_slots(L,[R|List],C0).

slots(List):-
    horizontal_slots(H_Slots),vertical_slots(V_Slots),
    find_cont_h(H_Slots,HW_Slots),find_cont_v(V_Slots,HV_Slots),
    append(HW_Slots,HV_Slots,List).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% Block_Manipulation %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% Word_Manipulation %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
words_to_lengths(Matches):-   %Creates a list of duplets of form (Length,Word)
    words(Words),words_to_lengths_h(Words,Matches).
words_to_lengths_h([],[]).
words_to_lengths_h([Word|Words],[(L,Word)|Lengths]):-
    name(Word,List),length(List,L),words_to_lengths_h(Words,Lengths).

extract_words_from_match([],[]).
extract_words_from_match([(_,Word)|Matches],[Word|Words]):-
    extract_words_from_match(Matches,Words).

short_words_by_length(Sorted_Words):-   %Sorts words by decreasing length order
    words_to_lengths(Matches),
    mergesort(Matches,Sorted_Matches),
    extract_words_from_match(Sorted_Matches,Sorted_Words),!.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% Word_Manipulation %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
extract_slots_from_matches([],[]).
extract_slots_from_matches([M|Matches],[S|Slots]):-
    M = (_,S),
    extract_slots_from_matches(Matches,Slots).
match_word_to_slot(Word,Slot,Match,AlreadyMatched):-
    name(Word,List),
    length(List,Length),length(Slot,Length),
    word_to_slot_helper(List,Slot,Match,AlreadyMatched).
word_to_slot_helper([],[],[],_).
word_to_slot_helper([Letter|Letters],[Slot|Slots],[(Letter,Slot)|Matches],AlreadyMatched):-
    extract_slots_from_matches(AlreadyMatched,AMSlots),
    \+index(Slot,AMSlots,_),    %try to match a whole word only if it doesn't have any slot already matched
    word_to_slot_helper(Letters,Slots,Matches,AlreadyMatched).
word_to_slot_helper([Letter|Letters],[Slot|Slots],[(Letter,Slot)|Matches],AlreadyMatched):-
    extract_slots_from_matches(AlreadyMatched,AMSlots),
    index(Slot,AMSlots,Index),    %if a slot is already matched, make sure that you are trying to match the same letter to that slot
    index((Letter,_),AlreadyMatched,Index),
    word_to_slot_helper(Letters,Slots,Matches,AlreadyMatched).

find_valid_match(Word,[Slot|Slots],Match,AlreadyMatched):-
    match_word_to_slot(Word,Slot,Match,AlreadyMatched);
    (\+match_word_to_slot(Word,Slot,_,AlreadyMatched),find_valid_match(Word,Slots,Match,AlreadyMatched)).

crossword(Solution):-
    short_words_by_length(Words),
    slots(Slots),
    crossword(Solution,Words,Words,Slots,[]).

crossword(Solution,_,[],_,[],Solution).
crossword(Solution,Words,[W|RW],Slots,AlreadyMatched):-
    find_valid_match(W,Slots,M,AlreadyMatched),
    crossword(Solution,Words,RW,Slots,[M|AlreadyMatched]).


