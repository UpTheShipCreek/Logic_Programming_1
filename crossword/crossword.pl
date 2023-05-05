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

zip([], [], []).
zip([X|Xs], [Y|Ys], [(X,Y)|Zs]) :- zip(Xs,Ys,Zs).

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

find_cont_h(List,ListoLists):-
    find_cont_h(List,List,[],ListoLists).
find_cont_h(_,[],ListoLists,ListoLists).
find_cont_h(_,[W],[],[]):- \+head(W,_).
find_cont_h(List,[W],[HLL|RLL],ListoLists):-
   length(HLL,Length),Length>0,
   reverse(HLL,THLL),head(THLL,Previous_W),(
   cont_h(Previous_W,W),append(HLL,[W],NewHLL),find_cont_h(List,[],[NewHLL|RLL],ListoLists);
   \+cont_h(Previous_W,W),find_cont_h(List,[],[HLL|RLL],ListoLists)).
find_cont_h(List,[W|RWhites],[],ListoLists):-
    length(RWhites,Length),Length>0,head(RWhites,Next_W),(
    cont_h(W,Next_W),find_cont_h(List,RWhites,[[W]],ListoLists);
    \+cont_h(W,Next_W),find_cont_h(List,RWhites,[],ListoLists)).
find_cont_h(List,[W|RWhites],[HLL|RLL],ListoLists):-
    length(HLL,Length1),Length1>0,length(RWhites,Length2),Length2>0,
    head(RWhites,Next_W),reverse(HLL,THLL),head(THLL,Previous_W),( % finding the next and previous whites for comparison
    cont_h(Previous_W,W),append(HLL,[W],NewHLL),find_cont_h(List,RWhites,[NewHLL|RLL],ListoLists);
    \+cont_h(Previous_W,W),cont_h(W,Next_W),find_cont_h(List,RWhites,[[W]|[HLL|RLL]],ListoLists);
    \+cont_h(Previous_W,W),\+cont_h(W,Next_W),find_cont_h(List,RWhites,[HLL|RLL],ListoLists)).

find_cont_v(List,ListoLists):-
    find_cont_v(List,List,[],ListoLists).
find_cont_v(_,[],ListoLists,ListoLists).
find_cont_v(_,[W],[],[]):- \+head(W,_).
find_cont_v(List,[W],[HLL|RLL],ListoLists):-
   length(HLL,Length),Length>0,
   reverse(HLL,THLL),head(THLL,Previous_W),(
   cont_v(Previous_W,W),append(HLL,[W],NewHLL),find_cont_v(List,[],[NewHLL|RLL],ListoLists);
   \+cont_v(Previous_W,W),find_cont_v(List,[],[HLL|RLL],ListoLists)).
find_cont_v(List,[W|RWhites],[],ListoLists):-
    length(RWhites,Length),Length>0,head(RWhites,Next_W),(
    cont_v(W,Next_W),find_cont_v(List,RWhites,[[W]],ListoLists);
    \+cont_v(W,Next_W),find_cont_v(List,RWhites,[],ListoLists)).
find_cont_v(List,[W|RWhites],[HLL|RLL],ListoLists):-
    length(HLL,Length1),Length1>0,length(RWhites,Length2),Length2>0,
    head(RWhites,Next_W),reverse(HLL,THLL),head(THLL,Previous_W),(
    cont_v(Previous_W,W),append(HLL,[W],NewHLL),find_cont_v(List,RWhites,[NewHLL|RLL],ListoLists);
    \+cont_v(Previous_W,W),cont_v(W,Next_W),find_cont_v(List,RWhites,[[W]|[HLL|RLL]],ListoLists);
    \+cont_v(Previous_W,W),\+cont_v(W,Next_W),find_cont_v(List,RWhites,[HLL|RLL],ListoLists)).

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

h_wordslots(WordSlots):- %Getting the lists of slot lists and extracting the slots for all the horizontal words
    horizontal_slots(Lists),h_wordslots(WordSlots,[],Lists,Lists),!.
h_wordslots(WordSlots,WordSlots,[],_).
h_wordslots(WordSlots,T,[E|L],List):-
    find_cont_h(E,H),append(H,T,N),h_wordslots(WordSlots,N,L,List).

v_wordslots(WordSlots):- %Getting the lists of slot lists and extracting the slots for all the vertical words
    vertical_slots(Lists),v_wordslots(WordSlots,[],Lists,Lists),!.
v_wordslots(WordSlots,WordSlots,[],_).
v_wordslots(WordSlots,T,[E|L],List):-
    find_cont_v(E,H),append(H,T,N),v_wordslots(WordSlots,N,L,List).

slots(List):-
    h_wordslots(HW_Slots),v_wordslots(VW_Slots),
    append(HW_Slots,VW_Slots,List).
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

extract_words_from_matches([],[]).
extract_words_from_matches([(_,Word)|Matches],[Word|Words]):-
    extract_words_from_matches(Matches,Words).

short_words_by_length(Sorted_Words):-   %Sorts words by decreasing length order
    words_to_lengths(Matches),
    mergesort(Matches,Sorted_Matches),
    extract_words_from_matches(Sorted_Matches,Sorted_Words),!.

fill_alrdy_matched([],_,[],[]). %creates two lists with the matches that seperates all the slots to two lists, a list that contains every slot filled and a list that contains the rest
fill_alrdy_matched([Slot|Slots],Matches,[(Letter,Slot)|F],Rest):- %the matches list needs to be flat
    member((Letter,Slot),Matches),fill_alrdy_matched(Slots,Matches,F,Rest),!.
fill_alrdy_matched([Slot|Slots],Matches,F,[Slot|Rest]):- %the matches list needs to be flat
    \+member((_,Slot),Matches),fill_alrdy_matched(Slots,Matches,F,Rest),!.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% Word_Manipulation %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% Formatting&Prints %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
format_solution(Word_Slots,Word_List):-     %Turn a list of a elements of the form (Word,Slot) to a sorted version of the word list
    length(Word_Slots,Length),
    length(Word_List,Length),
    h_wordslots(HW_Slots),v_wordslots(VW_Slots),
    reverse(HW_Slots,Horizontal),reverse(VW_Slots,Vertical),
    length(HW_Slots,L),
    %x_s(Horizontal,H),x_s(Vertical,V),
    %format_solution(Word_Slots,Word_Slots,Word_List,H,V,L).
    format_solution(Word_Slots,Word_Slots,Word_List,Horizontal,Vertical,L),!.

format_solution(_,[],_,_,_,_).
format_solution(Word_Slots,[(Word,Slot)|Rest],Word_List,H,V,L):-
    ((index(Slot,H,Index),index(Word,Word_List,Index)); %if the slot is horizontal, then the index on the slot array is the index of the word in the word array
    (index(Slot,V,Index),I is Index + L, index(Word,Word_List,I))), %if the slot is vertical, then the index of the word in the word array is the index of the slot plus the number of horizontal words before it
    format_solution(Word_Slots,Rest,Word_List,H,V,L).

grid(Grid):-dimension(N),grid(Grid,[],N),!.
grid(Grid,Grid,N):-length(Grid,N).
grid(Grid,SoFarGrid,N):-
    length(Column,N),
    grid(Grid,[Column|SoFarGrid],N).

fill_grid(ToPrint,Grid):-
    fill_grid(ToPrint,ToPrint,Grid),!.
fill_grid(_,[],_).
fill_grid(ToPrint,[(Letter,white(R,C))|Rest],Grid):-
    name(L,[Letter]),
    index(Row,Grid,R),
    index(L,Row,C),
    fill_grid(ToPrint,Rest,Grid).

print_crossword(ToPrint):-
    flatten(ToPrint,Flat),
    grid(Grid),
    fill_grid(Flat,Grid),
    print_grid(Grid).

print_grid(Grid):-
    print_grid_rows(Grid).
print_grid_rows([]).
print_grid_rows([Row|Rows]):- 
    print_grid_row(Row), nl, 
    print_grid_rows(Rows).
print_grid_row([]).
print_grid_row([X|Xs]) :- 
    (var(X)-> write('###'); 
    write(' '),write(X),write(' ')),
    print_grid_row(Xs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% Formatting&Prints %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% Main %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
crossword(F):-
    match_words_to_slots(ToPrint,Solution),
    format_solution(Solution,F),
    print_crossword(ToPrint).

match_words_to_slots(Matches,Word_Slots):-  % Matches = [[(112, x(4)), (105, x(9)), (114, x(14)), (117, x(19)), (115, x(24))],...], (ended up using the white(Row,Column) format instead of the x(N))
    short_words_by_length(Words),           %Word_Slots= [(word,[x4,x9,x14,x19,x24])...]
    slots(Slots),
    match_words_to_slots(Words,Slots,[],Matches,[],Word_Slots).

% Predicate to match words to slots
match_words_to_slots([],[],Matches,Matches,WS,WS):-!.
match_words_to_slots([Word|Words],Slots,SoFarMatches,Matches,SoFarWS,Word_Slots) :- 
    % Check if the word can fit into the slot
    flatten(SoFarMatches,FlatMatches), %flatten the already matched slots so you can easily search through it 
    find_fit(Word,Slots,(Word,Slot),FlatMatches),
    name(Word,LName),
    zip(LName,Slot,M),
    %delete(Word,Words,NWords),
    delete(Slot,Slots,NewSlots),
    append(SoFarMatches,[M],NM),
    % Recurse with the remaining words and slots
    match_words_to_slots(Words,NewSlots,NM,Matches,[(Word,Slot)|SoFarWS],Word_Slots),!.

find_fit(Word,[Slot|Slots],Fit,FlatMatches):-
    fits(Word,Slot,FlatMatches), Fit = (Word,Slot);
    find_fit(Word,Slots,Fit,FlatMatches).

fits(Word,Slot,FlatMatches) :-     %Fit a word into a slot
    name(Word,WList),
    length(Slot,L),
    %length(S,L),    %create an empty list of the same length, so as to be able to assign and check more seemlessly
    length(WList, L),
    fits_chars(WList,Slot,FlatMatches),!.
    %fill_alrdy_matched(Slot,FlatMatches,AlMatches,Rest),
    %fits_chars(WList,Slot,FlatMatches,AlMatches,Rest),!.

fits_chars(Word,Slot,FlatMatches):-
    fill_alrdy_matched(Slot,FlatMatches,AlMatches,Rest),
    fits_chars(Word,Slot,AlMatches,Rest).
fits_chars([],[],_,_).
fits_chars([Char|RWord],[Slot|Slots],AlMatches,Rest):-
    (member(Slot,Rest); (member((MatchedChar,Slot),AlMatches),MatchedChar == Char)), %if a slot is assigned a character make sure it matches, else just assign it                          
    fits_chars(RWord,Slots,AlMatches,Rest),!.                         
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% Main %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% Trash %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
x_symbolism(ListoWhites,ListoX):-
   dimension(D),
   x_symbolism(ListoWhites,ListoWhites,[],ListoX,D).
x_symbolism(_,[],ListoX,ListoX,_):-!.
x_symbolism(ListoWhites,[white(Row,Column)|RLoW],SoFarListoX,ListoX,D):-
   N is Column + (D*(Row-1)),append(SoFarListoX,[x(N)],New),
   x_symbolism(ListoWhites,RLoW,New,ListoX,D).

x_s(XLists):-slots(WLists),x_s(WLists,XLists).
x_s([],[]).
x_s([WL|ListoWLists],[XL|ListoXLists]):-
   x_symbolism(WL,XL),
   x_s(ListoWLists,ListoXLists).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% Trash %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%