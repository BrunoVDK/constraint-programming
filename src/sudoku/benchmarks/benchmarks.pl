%
% Automatic benchmarks for the Sudoku CLP solver.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- compile(minimum_puzzles). % Minimum sudokus
:- compile(sudex_toledo). % Example puzzles

% Automatic benchmarking with the provided puzzles.
benchmark :-
    write('sudex'), nl.

% Automatic benchmarking with the minimum puzzles.
%
% @note The minimum puzzles are puzzles with 17 pre-filled cells. Less isn't possible.
%       Courtesy of Gordon Royle : http://staffhome.ecm.uwa.edu.au/~00013890/sudokumin.php
%       Minimum sudokus were converted to a readable format with a Haskell script included
%       in the repository.
benchmark_minimum :-
    % TODO benchmark code depending on what's desired (for later)
    % eg. do timing + nb_backtracks per puzzle, calculate average and variation
    minimum_puzzles(P, 1),
    write(P), nl.
