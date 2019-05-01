%
% Automatic benchmarks for the Sudoku CLP solver.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- compile('puzzles/minimum_puzzles_100'). % Minimum sudokus
:- compile('puzzles/sudex_toledo'). % Example puzzles

nb_minimum(100). % Number of minimum puzzles to use

% Solve the puzzle with the given name and measure time and number of backtracks.
%
% @param Puzzle The puzzle to solve.
solve(Puzzle) :-
    sudoku_named(Puzzle, _, _, true).

% Automatic benchmarking with the provided puzzles.
%
% @param Verbose    Flag denoting whether or not intermediate results should be printed.
benchmark(Verbose) :-
    findall(Name, puzzles(_, Name), Puzzles),
    length(Puzzles, Nb),
    (   foreach(Puzzle, Puzzles), param(Verbose),
        fromto(0, InTime, OutTime, TotalTime),
        fromto(0, InBacktracks, OutBacktracks, TotalBacktracks) do
        sudoku_named(Puzzle, Time, Backtracks, Verbose),
        OutTime is InTime + Time,
        OutBacktracks is InBacktracks + Backtracks
    ),
    print_results(TotalTime, TotalBacktracks, Nb).

% Automatic benchmarking with the minimum puzzles.
%
% @param Verbose    Flag denoting whether or not intermediate results should be printed.
% @note The minimum puzzles are puzzles with 17 pre-filled cells. Less isn't possible.
%       Courtesy of Gordon Royle : http://staffhome.ecm.uwa.edu.au/~00013890/sudokumin.php
%       Minimum sudokus were converted to a readable format with a Haskell script included
%       in the repository.
benchmark_minimum(Verbose) :-
    nb_minimum(Nb), % Could use findall but this is more flexible
    (   for(I, 1, Nb),
        fromto(0, InTime, OutTime, TotalTime),
        fromto(0, InBacktracks, OutBacktracks, TotalBacktracks),
        param(Verbose) do
            minimum_puzzles(Puzzle, I),
            sudoku(Puzzle, Time, Backtracks, Verbose),
            OutTime is InTime + Time,
            OutBacktracks is InBacktracks + Backtracks
    ),
    print_results(TotalTime, TotalBacktracks, Nb).

% Print the results of the tests.
%
% @param Time       The total time spent searching for solutions.
% @param Backtracks The total recorded number of backtracks.
% @param Nb         The number of puzzles that were considered.
print_results(Time, Backtracks, Nb) :-
    write('Total time : '), write(Time), write(' s'), nl,
    AverageTime is Time / Nb,
    write('Average time : '), write(AverageTime), write(' s'), nl,
    write('Total backtracks : '), write(Backtracks), nl,
    AverageBacktracks is Backtracks / Nb,
    write('Average backtracks : '), write(AverageBacktracks), nl.

