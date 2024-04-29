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

% General Helper Methods
remove_duplicates([], []).

remove_duplicates([Head | Tail], Result) :-
    member(Head, Tail), !,
    remove_duplicates(Tail, Result).

remove_duplicates([Head | Tail], [Head | Result]) :-
    remove_duplicates(Tail, Result).