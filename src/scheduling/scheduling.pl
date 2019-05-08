%
% Scheduling meetings assignment.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- compile(benchmarksMeeting).

:- lib(ic).
%:- import sumlist/2 from ic_global.
:- lib(branch_and_bound).
:- lib(ic_edge_finder). % Provides the disjunctive/2 constraint

enable(global) :- true. % To turn ic_edge_finder on/off

% Schedule meetings for the given number of persons, each with their own preferences.
%   The cost function first takes into account the end time which should be as low as possible,
%   then it considers the number of violations (which should also be as low as possible).
%
% @param NbofPerson     The number of persons to meet up with.
% @param Durations      The durations of the meeting for each person (an array).
% @param OnWeekend      An array with value indicating
%                           willingness of each person to meet on weekends.
% @param Ranks          An array holding the ranks of every person.
% @param Pcs            Precedence constraints.
% @param StartingDay    Which day of the week is day 0 (0 Monday, 1 Tuesday, ..., 6 Sunday).
meeting(N, Durations, OnWeekend, Ranks, Pcs, StartingDay, StartTimes, EndTime, Violations) :-
    % --- Declare domains ---
    dim(StartTimes, [N]),
    sum(Durations[1..(N-1)], TotalDuration),
    MaxStart is (TotalDuration + 2*(N-1) + StartingDay),
    StartTimes :: 0..MaxStart,
    % --- Generate constraints ---
    timing_constraints(N, StartTimes, Durations, OnWeekend, StartingDay),
    non_overlapping(N, StartTimes, Durations),
    precedences(Pcs, StartTimes),
    % --- Define cost function ---
    violations(N, Ranks, StartTimes, Violations, MaxViolations),
    implied_constraints(N, Ranks, StartTimes, Durations, OnWeekend, Pcs),
    Cost #= MaxViolations * (StartTimes[N] + Durations[N]) + Violations,
    % --- Branch and bound ---
    minimize(labeling(StartTimes), Cost),
    %minimize(search(StartTimes, 0, first_fail, indomain, complete, []), Cost),
    EndTime is StartTimes[N] + Durations[N].

% Enforce timing constraints :
%   - no meetings during weekends if person doesn't want it
%   - minister comes last
%
% @param N              The number of persons.
% @param StartTimes     The start times variable array.
% @param Durations      Array of durations for each meeting with each person.
% @param OnWeekend      Weekend preferences per person.
% @param StartingDay    Which day of the week is day 0 (0 Monday, 1 Tuesday, ..., 6 Sunday).
timing_constraints(N, StartTimes, Durations, OnWeekend, StartingDay) :-
    (   for(I, 1, N),
        foreacharg(Start, StartTimes),
        foreacharg(Duration, Durations),
        foreacharg(W, OnWeekend),
        param(StartTimes, N, StartingDay) do
            (W \= 1 -> % Weekend constraints
                (Duration > 5 ->
                    fail % Cannot lead to a solution
                ;
                    % This just says the meeting cannot overlap with the
                    %   first weekend that follows. mod/3 can't be used by the way,
                    %   that's why there's an auxiliary variable.
                    X :: 0..6,
                    _Q * 7 + X #= Start + StartingDay,
                    X + Duration #< 6
                    % ((Start + StartingDay) mod 7) + Duration #< 5
                )
            ; true ),
            (I \= N -> Start #< StartTimes[N] ; true) % Minister comes last
    ).

% Make sure no meetings overlap.
%
% @param N              The number of persons
% @param StartTimes     The start times variable array.
% @param Durations      Array of durations for each meeting with each person.
non_overlapping(N, StartTimes, Durations) :-
    (enable(global) ->
        disjunctive(StartTimes, Durations)
    ;
        (   for(I, 1, N),
            foreacharg(Start, StartTimes),
            foreacharg(Duration, Durations),
            param(StartTimes, Durations, N) do
            (for(J, I+1, N), param(Start, Duration, StartTimes, Durations) do
                OtherStart is StartTimes[J],
                OtherDuration is Durations[J],
                (Start + Duration =< OtherStart) or (OtherStart + OtherDuration =< Start)
            )
        )
    ).

% Set up the precedence constraints.
%
% @param Precedences    The precedence constraints (an array).
% @param StartTimes     The start times variable array.
precedences(Precedences, StartTimes) :-
    (foreacharg(Precedence, Precedences), param(StartTimes) do
        StartTimes[Precedence[1]] #< StartTimes[Precedence[2]]
    ).

% Set up constraints to calculate violations for the given ranks & start times.
%
% @param N              The number of persons.
% @param Ranks          The ranks of the persons (an array).
% @param StartTimes     The start times of each meeting for each person (an array).
violations(N, Ranks, StartTimes, Violations, MaxViolations) :-
    (for(I, 1, N-1),
     fromto([], InViolations, OutViolations, ViolationList),
     param(StartTimes, N, Ranks) do
        Rank is Ranks[I],
        (for(J, I+1, N-1),
         fromto([], In, Out, List),
         param(Rank, Ranks, StartTimes, I) do
            OtherRank is Ranks[J],
            (Rank < OtherRank -> Out = [(StartTimes[I] #> StartTimes[J])|In] ;
            (Rank > OtherRank -> Out = [(StartTimes[I] #< StartTimes[J])|In] ;
                Out = In))
        ),
        append(InViolations, List, OutViolations)
    ),
    length(ViolationList, MaxViolations),
    write('Max # violations : '), write(MaxViolations), nl,
    %sumlist(ViolationList, Violations).
    Violations #= sum(ViolationList).

% Experiments with implied constraints.
%
% @param N      The number of persons.
% @param Ranks  The rank of each person (an array).
% @param StartTimes     The start times of each meeting for each person (an array).
% @param Durations      The durations of meetings for each person (an array).
% @param OnWeekend      An array with value indicating
%                           willingness of each person to meet on weekends.
% @param Pcs            Precedence constraints.
implied_constraints(N, Ranks, StartTimes, Durations, OnWeekend, Pcs) :-
    collection_to_list(Pcs, Precedences),
    (   for(I, 1, N-1),
        param(N, Ranks, StartTimes, Durations, OnWeekend, Precedences) do
        (for(J, I+1, N-1), param(I, Ranks, StartTimes, Durations, OnWeekend, Precedences) do
            Rank is Ranks[I], OtherRank is Ranks[J],
            (   \+ member(I, Precedences),
                \+ member(J, Precedences),
                Durations[I] =:= Durations[J],
                OnWeekend[I] =:= OnWeekend[J] ->
                (Rank < OtherRank -> StartTimes[I] #< StartTimes[J] ;
                (Rank > OtherRank -> StartTimes[I] #> StartTimes[J] ; true))
            ; true)
        )
    ).

% Automate benchmarking for the schedule meetings function.
%
% @param Verbose    If intermediate results should be printed out.
benchmark(Verbose) :-
    Tests = [   bench1a,
                bench1b,
                bench1c,
                bench2a,
                bench2b,
                bench2c,
                bench3a,
                bench3b,
                bench3c,
                bench3d,
                bench3e,
                bench3f,
                bench3g],
    (Verbose -> write('Running tests (schedule meetings) ...'), nl, log ; true),
    (   foreach(Test, Tests), param(Verbose),
        fromto(0, InTime, OutTime, TotalTime) do
        (Verbose -> write('-> Search prodecure started : '), write(Test), nl ; true),
        statistics(hr_time, Start), % http://eclipseclp.org/doc/bips/kernel/env/statistics-2.html
        call(Test, StartTimes, EndTime, Violations),
        (Verbose -> write('Start times : '), write(StartTimes), nl ; true),
        (Verbose -> write('End time : '), write(EndTime), nl ; true),
        (Verbose -> write('Violations : '), write(Violations), nl ; true),
        statistics(hr_time, End),
        Time is End - Start,
        % (Verbose -> write('Backtracks : '), write(Backtracks), nl ; true),
        (Verbose -> write('Time : '), write(Time), nl, log ; true),
        OutTime is InTime + Time
    ),
    write('Total time : '), write(TotalTime), nl.
