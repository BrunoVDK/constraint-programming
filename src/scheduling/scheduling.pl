%
% Scheduling meetings assignment.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- compile('../utils.pl'). % Import utility functions
:- compile(benchmarksMeeting).

:- lib(ic).
:- lib(branch_and_bound).
:- lib(ic_edge_finder). % Provides the disjunctive/2 constraint

% Schedule meetings for the given number of persons, each with their own preferences.
%   The cost function first takes into account the end time which should be as low as possible,
%   then it considers the number of violations (which should also be as low as possible).
%
% @param NbofPerson     The number of persons to meet up with.
% @param Durations      The durations of the meeting for each person (an array).
% @param OnWeekend      An array with value indicating
%                           willingness of each person to meet on weekends.
% @param Rank           An array holding the ranks of every person.
% @param Precs          Precedense constraints.
% @param StartingDay    Which day of the week is day 0 (0 Monday, 1 Tuesday, ..., 6 Sunday).
meeting(N, Durations, OnWeekend, _Rank, Precs, _StartingDay, Start, EndTime , 0) :-
    dim(Start, [N]),
    sum(Durations[1..N], Sum),
    Start :: 0..(2*N+Sum),
    %Saturday is StartingDay - 5,
    (for(I, 1, N), foreacharg(W, OnWeekend), param(OnWeekend, N, Start, Durations) do
        (I \= N -> Start[I] + Durations[I] #=< Start[I+1] ; true),
        (W \= 1 -> % Weekend constraints
            (Durations[I] > 5 ->
                fail % Cannot lead to a solution
            ;
                true
                %S is Start[I],
                %Duration = Durations[I],
                %StartI mod 7 + Duration #< Saturday
            )
        ;true)
    ),
    % disjunctive(Start, Durations),
    (foreacharg(Precedence, Precs), param(Start) do
        Start[Precedence[1]] #< Start[Precedence[2]]
    ),
    maxlist(Start, EndTime),
    minimize(labeling(Start), EndTime).

% Automate benchmarking for the schedule meetings function.
%
% @param Verbose    If intermediate results should be printed out.
benchmark(Verbose) :-
    Tests = [   test1,
                test1b,
                test1c,
                test2,
                bench1a,
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
                bench3f],
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
