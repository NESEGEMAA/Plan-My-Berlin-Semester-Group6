:- use_module(library(lists)).
:- [slots_kb, transport_kb].

%implementing Slots -> group_days/2

group_days(Group, Day_Timings) :-
    findall(day_timing(Week_Num, Week_Day),
            scheduled_slot(Week_Num, Week_Day, _, _, Group),
            Day_Timings1),
    remove_repetitions(Day_Timings1,Day_Timings).

%Predicate that helps remove repetitions

remove_repetitions([],[]).

remove_repetitions([H|T], T1):-
    member(H,T),
    !,
    remove_repetitions(T,T1).
remove_repetitions([H|T], [H|T1]):-
    remove_repetitions(T,T1).


%implementing Slots -> day_slots/4

day_slots(Group, Week, Day, Slots):-
    findall(Slot,
            scheduled_slot(Week, Day, Slot, _, Group),
            Slots).

%implementing Slots -> earliest_slot/4

earliest_slot(Group, Week, Day, Slot):-
    day_slots(Group, Week, Day, Slots),
    sort(Slots,[H|_]),
    Slot is H.

%implementing Transportation -> proper_connection/4

proper_connection(Station_A, Station_B, Duration, Line):-
    proper_connection_helper(Station_A, Station_B, Duration, Line),
    Duration \== 0.

proper_connection(Station_A, Station_B, Duration, Line):-
    \+unidirectional(Line),
    proper_connection_helper(Station_B, Station_A, Duration, Line),
    Duration \== 0.

proper_connection_helper(Station_A, Station_B, Duration, Line):-
    connection(Station_A, X, Duration1, Line),
    ((X == Station_B) -> (Duration is Duration1)
    ;
    (proper_connection_helper(X, Station_B, Duration2, Line),
    Duration is Duration1 + Duration2)).
    

%implementing Transportation -> append_connection/6

append_connection(Conn_Source,Conn_Destination,Conn_Duration,Conn_Line,[],[route(Conn_Line, Conn_Source, Conn_Destination, Conn_Duration)]):- !.

append_connection(Conn_Source,Conn_Destination,Conn_Duration,Conn_Line,Routes_So_Far,Routes):-
    last(Routes_So_Far, E),
    E = route(Line, Source, Destination, Duration),
    Destination == Conn_Source,
    Line == Conn_Line,
    !,
    delete_last(Routes_So_Far, Corrected_Routes_So_Far),
    Correct_Duration is Duration + Conn_Duration,
    append(Corrected_Routes_So_Far,[route(Line, Source, Conn_Destination, Correct_Duration)],Routes).

append_connection(Conn_Source, Conn_Destination, Conn_Duration, Conn_Line, Routes_So_Far, Routes):-
    append(Routes_So_Far, [route(Conn_Line, Conn_Source, Conn_Destination, Conn_Duration)], Routes).

%DeleteLast Predicate

delete_last([],[]).
delete_last([H|[]], []):- 
    \+is_list(H),
    !.
delete_last([H|T], [H|List]):-  
    delete_last(T, List).

%implementing Transportation -> connected/8

connected(Source, Destination, Week, Day, Max_Duration, Max_Routes, Duration, Routes):-
    connected_helper(Source, Destination, Week, Day, Max_Duration, Max_Routes, Duration, Routes, 0, []),
    !.

connected_helper(Source,Source,_,_,_,_,DurationAcc,RoutesAcc,DurationAcc,RoutesAcc):- !.

connected_helper(Source, Destination, Week, Day, Max_Duration, Max_Routes, Duration, Routes, DurationAcc, RoutesAcc):-   
    line(Transport,Line),
    \+strike(Line, Week, Day),
    proper_connection(Source, X, Duration1, Transport),
    append_connection(Source, X, Duration1, Transport, RoutesAcc, RoutesAcc1),
    DurationAcc1 is DurationAcc + Duration1,
    length(RoutesAcc1, RouteCount),
    DurationAcc1 =< Max_Duration,
    RouteCount =< Max_Routes,
    connected_helper(X, Destination, Week, Day, Max_Duration, Max_Routes, Duration, Routes, DurationAcc1, RoutesAcc1).
