% This line consults the knowledge bases from this file,
% instead of needing to consult the files individually.
% This line MUST be included in the final submission.
:- ['transport_kb', 'slots_kb'].

% Data Structures
% day_timing(Week, Day).
% journey(Week_Num, Week_Day, Start_Hour, Start_Minute, Total_Duration, Routes).
% route(Line, Start_Station, End_Station, Duration).

% Predicates
% SLOTS
group_days(Group, Day_Timings):-
    X = day_timing(Week, Day),
    findall(X, scheduled_slot(Week, Day, _, _, Group), CrudeResult),
    remove_duplicates(CrudeResult, Day_Timings).

day_slots(Group, Week, Day, Slots):-
    findall(X, scheduled_slot(Week, Day, X, _, Group), CrudeResult),
    remove_duplicates(CrudeResult, Slots).
    
earliest_slot(Group, Week, Day, Slot):-
    day_slots(Group, Week, Day, SlotList),
    sort(SlotList, [H|_]),
    Slot is H.

% TRANSPORTATION
proper_connection(Station_A, Station_B, Duration, Line):-
    (connection(Station_A,_,_,Line); connection(_,Station_A,_,Line)),
    (connection(Station_B,_,_,Line); connection(_,Station_B,_,Line)),
    (get_duration(Station_A, Station_B, Line, 0, Duration); get_duration_backward(Station_A, Station_B, Line, 0, Duration)).

% TRANSPORTATION's helper methods

get_duration(Station_A, Station_C, Line, Acc, Duration):- !,
    Station_A \== Station_C,
    connection(Station_A, Station_C, X, Line),
    Duration is Acc + X.

get_duration(Station_A, Station_C, Line, Acc, Duration):-
    Station_A \== Station_C,
    connection(Station_A, Station_B, X, Line),
    New_Acc is Acc + X,
    New_Acc =< Duration,
    get_duration(Station_B, Station_C, Line, New_Acc, Duration).

get_duration_backward(Station_A, Station_C, Line, Acc, Duration):- !,
    \+ unidirectional(Line),
    Station_A \== Station_C,
    connection(Station_C, Station_A, X, Line),
    Duration is Acc + X.

get_duration_backward(Station_A, Station_C, Line, Acc, Duration):-
    \+ unidirectional(Line),
    Station_A \== Station_C,
    connection(Station_B, Station_A, X, Line),
    New_Acc is Acc + X,
    New_Acc =< Duration,
    get_duration(Station_B, Station_C, Line, New_Acc, Duration).
    

% General Helper Methods
remove_duplicates([], []).

remove_duplicates([Head | Tail], Result) :-
    member(Head, Tail), !,
    remove_duplicates(Tail, Result).

remove_duplicates([Head | Tail], [Head | Result]) :-
    remove_duplicates(Tail, Result).