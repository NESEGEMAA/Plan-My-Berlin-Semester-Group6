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

append_connection(Conn_Source, Conn_Destination, Conn_Duration, Conn_Line, Routes_So_Far, Routes):-
    proper_connection(Conn_Source, Conn_Destination, Conn_Duration, Conn_Line),
    check_available(Conn_Source, Conn_Destination, Conn_Duration, Conn_Line, Routes_So_Far, ProperRoute),
    append(Routes_So_Far, ProperRoute, Routes).

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

check_available(Station_A, Station_B, Duration_4, Line, [], route(Line, Station_A, Station_B, Duration_4)).
check_available(Station_A, Station_B, Duration_4, Line, [H|T], ProperRoute):- !,
    H \== route(Line, _, _, _),
    check_available(Station_A, Station_B, Duration_4, Line, T, ProperRoute).

check_available(Station_A, Station_B, Duration_4, Line, [H|_], ProperRoute):-
    H = route(Line, Station_X, Station_Y, Duration_3),
    proper_connection(Station_A, Station_Y, Duration_1, Line),
    proper_connection(Station_X, Station_B, Duration_2, Line),
    ((
        Duration_3 > Duration_1,
        Duration_3 > Duration_2,
        ProperRoute is route(Line, Station_X, Station_Y, Duration_3)
    );
    (
        Duration_1 > Duration_3,
        Duration_1 > Duration_4,
        ProperRoute is route(Line, Station_A, Station_Y, Duration_1) 
    );
    (
        ProperRoute is route(Line, Station_A, Station_B, Duration_4)
    )).
    

% General Helper Methods
remove_duplicates([], []).

remove_duplicates([Head | Tail], Result) :-
    member(Head, Tail), !,
    remove_duplicates(Tail, Result).

remove_duplicates([Head | Tail], [Head | Result]) :-
    remove_duplicates(Tail, Result).