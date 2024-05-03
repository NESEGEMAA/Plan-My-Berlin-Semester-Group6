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
    (var(Line) -> line(Line,_),
    proper_connection_helper(Station_A,Station_B,Duration,Line)
    ;
    proper_connection_helper(Station_A,Station_B,Duration,Line)
    ).

proper_connection_helper(Station_A, Station_B, Duration, Line):-
    connection(Station_A,_,_,Line),
    (var(Station_B)
    -> proper_connection_forwards(Station_A, Station_B, Duration, Line, [Station_A], 0)
    ; 
    proper_connection_forwards(Station_A, Station_B, Duration, Line, [Station_A], 0),
    !
    ),
    Duration \== 0.

proper_connection_helper(Station_A, Station_B, Duration, Line):-
    \+unidirectional(Line),
    (var(Station_B)
    -> proper_connection_backwards(Station_A, Station_B, Duration, Line, [Station_A], 0)
    ; 
    proper_connection_backwards(Station_A, Station_B, Duration, Line, [Station_A], 0),
    !
    ),
    Duration \== 0.

proper_connection_forwards(Station_A, Station_A, DurationAcc,_, _, DurationAcc).
proper_connection_forwards(Station_A, Station_B, Duration, Line, PreviouslyVisited, DurationAcc):-
    connection(Station_A, X, Duration1, Line),
    (member(X, PreviouslyVisited) -> Duration is 0
    ;
    (append(PreviouslyVisited, [X], PreviouslyVisited1),
    DurationAcc1 is DurationAcc + Duration1,
    proper_connection_forwards(X, Station_B, Duration, Line, PreviouslyVisited1, DurationAcc1))).

proper_connection_backwards(Station_A, Station_A, DurationAcc,_, _, DurationAcc).
proper_connection_backwards(Station_A, Station_B, Duration, Line, PreviouslyVisited, DurationAcc):-
    connection(X, Station_A, Duration1, Line),
    (member(X, PreviouslyVisited) ->  Duration is 0
    ;
    (append(PreviouslyVisited, [X], PreviouslyVisited1),
    DurationAcc1 is DurationAcc + Duration1,
    proper_connection_backwards(X, Station_B, Duration, Line, PreviouslyVisited1, DurationAcc1))).
    
%implementing Transportation -> append_connection/6

append_connection(Conn_Source,Conn_Destination,Conn_Duration,Conn_Line,[],[route(Conn_Line, Conn_Source, Conn_Destination, Conn_Duration)]):- !.

append_connection(Conn_Source,Conn_Destination,Conn_Duration,Conn_Line,Routes_So_Far,Routes):-
    last(Routes_So_Far, E),
    E = route(Line, Source, Destination, Duration),
    not(member(route(Conn_Line, Conn_Source, Conn_Destination, Conn_Duration), Routes_So_Far)),
    Source \== Conn_Destination,
    Destination == Conn_Source,
    Line == Conn_Line,
    !,
    delete_last(Routes_So_Far, Corrected_Routes_So_Far),
    Correct_Duration is Duration + Conn_Duration,
    append(Corrected_Routes_So_Far,[route(Line, Source, Conn_Destination, Correct_Duration)],Routes).

append_connection(Conn_Source, Conn_Destination, Conn_Duration, Conn_Line, Routes_So_Far, Routes):-
    last(Routes_So_Far, E),
    E = route(_, Source, _, _),
    not(member(route(Conn_Line, Conn_Source, Conn_Destination, Conn_Duration), Routes_So_Far)),
    Source \== Conn_Destination,
    append(Routes_So_Far, [route(Conn_Line, Conn_Source, Conn_Destination, Conn_Duration)], Routes).

%DeleteLast Predicate

delete_last([],[]).
delete_last([H|[]], []):- 
    \+is_list(H),
    !.
delete_last([H|T], [H|List]):-  
    delete_last(T, List).

%implementing Transportation -> connected/8 and Transportation -> connected/10

connected(Source, Destination, Week, Day, Max_Duration, Max_Routes, Duration, Routes):-
    connected(Source, Destination, Week, Day, Max_Duration, Max_Routes, Duration, [Source], [], Routes).

connected(Source, Destination, Week, Day, Max_Duration, Max_Routes, Duration, Prev_Stations, Routes_So_Far, Routes):-
    connected(Source, Destination, Week, Day, Max_Duration, Max_Routes, Duration, Routes, 0, Routes_So_Far, [], Prev_Stations).

connected(Source, Source, _, _, Max_Duration, Max_Routes, DurationAcc, RoutesAcc, DurationAcc, RoutesAcc, _, _):-
    DurationAcc \== 0,
    DurationAcc =< Max_Duration,
    length(RoutesAcc, L),
    L =< Max_Routes.

connected(Source, Destination, Week, Day, Max_Duration, Max_Routes, Duration, Routes, DurationAcc, RoutesAcc, LineAcc, Prev_Stations):-
    DurationAcc < Max_Duration,
    length(RoutesAcc, L),
    L < Max_Routes,
    get_all_stations(Source, Stations),
    filter_by_duration(Source, Stations, Max_Duration, FinalStations),
    filter_by_visited_lines(FinalStations, LineAcc, FinalStations1),
    filter_by_visited_stations(FinalStations1, Prev_Stations, FinalStations2),
    remove_ring(FinalStations2, LineAcc, CorrectedFinalStationsNotRandom),
    random_permutation(CorrectedFinalStationsNotRandom, CorrectedFinalStations),
    member(station_data(X, Duration1, Line), CorrectedFinalStations),
    Max_Duration >= Duration1 + DurationAcc,
    line(Line, Mode),
    not(strike(Mode, Week, Day)),
    append(Prev_Stations, [Destination], New_Prev_Stations),
    append(LineAcc, [Line], NewLineAcc),
    DurationAcc1 is DurationAcc + Duration1,
    append_connection(Source, X, Duration1, Line, RoutesAcc, RoutesAcc1),
    connected(X, Destination, Week, Day, Max_Duration, Max_Routes, Duration, Routes, DurationAcc1, RoutesAcc1, NewLineAcc, New_Prev_Stations).

get_all_stations(Source, TestAns):-
    findall(station_data(X,Y,Z),
            proper_connection(Source, X, Y, Z),
            TestAns1),
    remove_repetitions(TestAns1, TestAns).

filter_by_duration(_, [], _, []).

filter_by_duration(Source, [H|T], Max_Duration, [H|FinalStations]):-
    H = station_data(_, Duration,_),
    Duration =< Max_Duration,
    filter_by_duration(Source, T, Max_Duration, FinalStations).

filter_by_duration(Source, [H|T], Max_Duration, FinalStations):-  
    H = station_data(_, Duration,_),
    Duration > Max_Duration,
    filter_by_duration(Source, T, Max_Duration, FinalStations).

filter_by_visited_lines([], _, []).

filter_by_visited_lines([H|FirstList], SecondList, Unique) :-
    H = station_data(_,_,X),
    member(X, SecondList),
    !,
    filter_by_visited_lines(FirstList, SecondList, Unique).

filter_visited_lines([H|FirstList], SecondList, [H|Unique]) :-
    H = station_data(_,_,X),
    \+ member(X, SecondList),
    filter_by_visited_lines(FirstList, SecondList, Unique).

filter_by_visited_stations([], _, []).

filter_by_visited_stations([H|T], Prev_Stations, New_Prev_Stations):-
    member(H, Prev_Stations),
    !,
    filter_by_visited_stations(T, Prev_Stations, New_Prev_Stations).

filter_by_visited_stations([H|T], Prev_Stations, [H|New_Prev_Stations]):-
    not(member(H, Prev_Stations)),
    filter_by_visited_stations(T, Prev_Stations, New_Prev_Stations).

remove_ring([], _, []).

remove_ring([H|T], Lines, FinalStations):-
    H = station_data(_, _, Line),
    remove_ring(T, Lines, FinalStations1),
    (Line == s41 ->
    (member(s42, Lines) ->
        FinalStations = FinalStations1
        ;
        FinalStations = [H|FinalStations1])
    ;
    Line == s42 ->
    (member(s41, Lines) ->
        FinalStations = FinalStations1
        ;
        FinalStations = [H|FinalStations1])
    ;
    FinalStations = [H|FinalStations1]
    ).


%implementing Time Conversion -> mins_to_twentyfour_hr

mins_to_twentyfour_hr(Minutes, TwentyFour_Hours, TwentyFour_Mins):-
    ProperMinutes is Minutes mod 1440,
    TwentyFour_Hours is ProperMinutes//60,
    TwentyFour_Mins is ProperMinutes mod 60.

%implementing Time Conversion -> twentyfour_hr_to_mins

twentyfour_hr_to_mins(TwentyFour_Hours, TwentyFour_Mins, Minutes):-
    Proper_TwentyFour_Hours is (TwentyFour_Hours mod 24) + TwentyFour_Mins//60,
    Proper_TwentyFour_Mins is TwentyFour_Mins mod 60,
    Minutes is (Proper_TwentyFour_Hours*60 + Proper_TwentyFour_Mins) mod 1440.

%implementing Time Conversion -> slot_to_mins

slot_to_mins(Slot_Num, Minutes):-
    slot(Slot_Num, Start_Hour, Start_Minute),
    twentyfour_hr_to_mins(Start_Hour, Start_Minute, TwentyFour_Mins),
    Minutes is TwentyFour_Mins.

%implementing the main predicate: travel_plan

%notes:
%Home_Stations is a list of all possible sources
%The destination is always tegel if it is reachable, otherwise borsigwerke
%A journey consists of Week_Num, Week_Day, Start_Hour, Start_Minute, Total_Duration, Routes


%group struct is (Week_Num, Week_Day, Slot_Time)

info_getter(Week_Num, Week_Day, Slot, Group, Slot_Time):-
    scheduled_slot(Week_Num, Week_Day, _, _, Group),
    earliest_slot(Group, Week_Num, Week_Day, Slot),
    slot_to_mins(Slot, Slot_Time).

info_getter_list(Group, List):-
    findall(group(Week_Num, Week_Day, Slot_Time),
    info_getter(Week_Num, Week_Day, _, Group, Slot_Time),
    List1),
    remove_repetitions(List1, List).

travel_plan(Starting_List, Group, Max_Duration, Max_Routes, Journeys):-
    info_getter_list(Group, Day_List),
    travel_plan_helper(Starting_List, Max_Duration, Max_Routes, Day_List, Journeys).

travel_plan_helper(_,_,_,[],[]).

travel_plan_helper(Starting_List, Max_Duration, Max_Routes, [H|T], Journeys):-
    travel_plan_helper2(Starting_List, Max_Duration, Max_Routes, H, JourneyList),
    length(JourneyList, L),
    L > 0,
    travel_plan_helper(Starting_List, Max_Duration, Max_Routes, T, Journeys2),
    append(JourneyList, Journeys2, Journeys).

travel_plan_helper2([], _, _, _, []).

travel_plan_helper2(Starting_List, Max_Duration, Max_Routes, GroupData, Journey):-
    random_permutation(Starting_List, Starting_List_Randomized),
    Starting_List_Randomized = [H|T],
    GroupData = group(Week_Num, Week_Day, Slot_Time),
    (connected(H, tegel, Week_Num, Week_Day, Max_Duration, Max_Routes, Duration, Routes) -> 
    StartingTime is Slot_Time - Duration,
    mins_to_twentyfour_hr(StartingTime, Start_Hour, Start_Minute),
    Journey1 = journey(Week_Num, Week_Day, Start_Hour, Start_Minute, Duration, Routes),
    Journey = [Journey1]
    ;
    (connected(H, borsigwerke, Week_Num, Week_Day, Max_Duration, Max_Routes, Duration, Routes) -> 
    StartingTime is Slot_Time - Duration,
    mins_to_twentyfour_hr(StartingTime, Start_Hour, Start_Minute),
    Journey1 = journey(Week_Num, Week_Day, Start_Hour, Start_Minute, Duration, Routes),
    Journey = [Journey1])
    ;
    travel_plan_helper2(T, Max_Duration, Max_Routes, GroupData, Journey)
    ).