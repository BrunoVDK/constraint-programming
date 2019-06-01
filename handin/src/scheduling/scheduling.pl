%
% Scheduling meetings assignment.
%   For clarification ; branch and bound is done by defining a cost
%   function. If a solution is found with cost MAX, a constraint is put on the cost,
%   Cost #< MAX. Then new solutions are sought. This will prune the search tree.
%
%   The cost function in our implementation prioritises the total duration and considers
%       the number of violations next.
%
%   The weekend constraints just say that if a person doesn't want so, a meeting will never
%       overlap with the weekend that follows.
%   If a person does not want to meet on weekends but the duration of his meeting is longer
%       than 5 days this will lead to an immediate failure.
%
%   We added implied constraints ; if two people have the same preferences and
%       their meeting is of the same duration, but their rank is different,
%       then an ordering is imposed based on their rank. Precedence
%       constraints are prioritised of course.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- compile(benchmarksMeeting).

:- lib(ic).
%:- import sumlist/2 from ic_global.
:- lib(branch_and_bound).
:- lib(ic_edge_finder). % Provides the disjunctive/2 constraint

enable(edge_finder) :- true. % To turn ic_edge_finder on/off

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
% @note The starting time domains needn't be tight because the main source of pruning is the cost
%           function, so start times that are too high are filtered out.
meeting(N, Durations, OnWeekend, Ranks, Pcs, StartingDay, StartTimes, EndTime, Violations) :-
    % --- Declare domains ---
    dim(StartTimes, [N]),
    sum(Durations[1..(N-1)], TotalDuration),
    StartTimes :: 0..7*(N-1), % Needn't be tighter, doesn't speed up
    % --- Generate constraints ---
    timing_constraints(N, StartTimes, Durations, OnWeekend, StartingDay),
    non_overlapping(N, StartTimes, Durations),
    precedences(Pcs, StartTimes),
    implied_constraints(N, Ranks, StartTimes, Durations, OnWeekend, Pcs),
    % trivial_constraints(N, Ranks, StartTimes, Durations, OnWeekend, Pcs),
    % --- Define cost function ---
    violations(N, Ranks, StartTimes, Violations, MaxViolations),
    min_violations(Pcs, Ranks, MinViolations),
    Cost #= MaxViolations * (StartTimes[N] + Durations[N]) + Violations,
    Cost #> MaxViolations * (TotalDuration + Durations[N]) + MinViolations,
    % --- Branch and bound ---
    %minimize(labeling(StartTimes), Cost),
    bb_min(search(StartTimes, 0, input_order, indomain_min, complete, [backtrack(Backtracks)]), Cost, bb_options{strategy:continue}),
    %write('Backtracks : '), write(Backtracks), nl,
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
    (enable(edge_finder) ->
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
    (foreacharg([](I,J), Precedences), param(StartTimes) do
        StartTimes[I] #< StartTimes[J]
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
            %(Rank < OtherRank -> #>(StartTimes[I], StartTimes[J], Bool), Out = [Bool|In] ;
            %(Rank > OtherRank -> #<(StartTimes[I], StartTimes[J], Bool), Out = [Bool|In] ;
            %Out = In))
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
% @param N              The number of persons.
% @param Ranks          The rank of each person (an array).
% @param StartTimes     The start times of each meeting for each person (an array).
% @param Durations      The durations of meetings for each person (an array).
% @param OnWeekend      An array with value indicating
%                           willingness of each person to meet on weekends.
% @param Precedences    Precedence constraints.
% @note These constraints could be generalized.
implied_constraints(N, Ranks, StartTimes, Durations, OnWeekend, Pcs) :-
    collection_to_list(Pcs, Precedences),
    (for(I, 1, N-1),
     param(N, Ranks, StartTimes, Durations, OnWeekend, Precedences) do
        (for(J, I+1, N-1),
         param(I, Ranks, StartTimes, Durations, OnWeekend, Precedences) do
            Rank is Ranks[I], OtherRank is Ranks[J],
            (\+ member(I, Precedences),
             \+ member(J, Precedences),
             OnWeekend[I] =:= OnWeekend[J],
             Durations[I] =:= Durations[J] ->
                (Rank =< OtherRank ->
                    StartTimes[I] #< StartTimes[J]
                ;
                    StartTimes[I] #> StartTimes[J]
                )
            ; true)
        )
    ).

% Trivial constraints : if all meetings can be done on weekends, the order is
%  nearly entirely dependent on rank (with the exception of precedence constraints).
%
% @param N              The number of persons.
% @param Ranks          The rank of each person (an array).
% @param StartTimes     The start times of each meeting for each person (an array).
% @param Durations      The durations of meetings for each person (an array).
% @param OnWeekend      An array with value indicating
%                           willingness of each person to meet on weekends.
% @param Precedences    Precedence constraints.
% @note These are unrealistic, they're 'overfitting' the benchmarks and deal with
%        bench2b in particular. They're not used in the experiments in the report.
trivial_constraints(N, Ranks, StartTimes, _Durations, OnWeekend, Pcs) :-
    collection_to_list(OnWeekend, List),
    (\+ member(0, List) ->
        collection_to_list(Pcs, Precedences),
        (for(I, 1, N-1),
         param(N, Ranks, StartTimes, Precedences) do
            (for(J, I+1, N-1),
             param(I, Ranks, StartTimes, Precedences) do
                Rank is Ranks[I], OtherRank is Ranks[J],
                (\+ member(I, Precedences),
                 \+ member(J, Precedences) ->
                    (Rank =< OtherRank ->
                        StartTimes[I] #< StartTimes[J]
                    ;
                        StartTimes[I] #> StartTimes[J]
                    )
                ; true)
            )
        )
    ; true).

% Calculate the minimum number of violations when considering the given
%   precedence constraints and the given ranks.
%
% @param Precedences    The precedences
% @note Putting a minimum on the cost only benefits few puzzles, e.g. the one
%        where the minimum end time equals the total duration (bench2b).
min_violations(Precedences, Ranks, MinViolations) :-
    extend_precedences(Precedences, AllPreferences),
    (foreach([](I,J), AllPreferences), fromto(0, In, Out, MinViolations), param(Ranks) do
        (Ranks[I] > Ranks[J] -> Out is In + 1 ; Out is In)
    ),
    write('Min # violations : '), write(MinViolations), nl.

% For a given array of precedence constraints, extend it so that all transitive relations are
%   exposed.
%
% @param Precedences            An array of precedence constraints.
% @param ExtendedPreferences    A list with the same precedence constraints as well as
%                                any transitive constraints.
% @note Assumes absence of circular (erroneous) references.
% @note Not written in the best way, there's redundancy.
extend_precedences(Precedences, ExtendedPreferences) :-
    array_list(Precedences, PrecedenceList),
    (foreach([](I,J), PrecedenceList),
     fromto(PrecedenceList, In, Out, OutPreferences),
     param(PrecedenceList) do
        findall([](I,X), (path(J,X,PrecedenceList), X \= I), P1),
        findall([](J,X), (path(J,X,PrecedenceList), X \= J), P2),
        append(In, P1, P3),
        append(P3, P2, Out)
    ),
    sort(OutPreferences, ExtendedPreferences).
path(J, X, PrecedenceList) :-
    member([](J,Y), PrecedenceList),
    path(Y, X, PrecedenceList).
path(X, X, _).

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
    (Verbose -> write('Running tests (schedule meetings) ...') ; true),
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
        (Verbose -> write('Time : '), write(Time), nl ; true),
        OutTime is InTime + Time
    ),
    write('Total time : '), write(TotalTime), nl.
