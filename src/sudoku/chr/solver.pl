%
% Sudoku solver with CHR.
%   The solver reads in a puzzle, sets up a model and searches for a solution.
%       It is assumed that for a puzzle of size N, blocks have a dimension of K x K
%       with K * K = N. This deals with several of the variants including hexadoku.
%
%   Several models can be made use of :
%       - classic : the classic ('primal') model
%       - dual3/dual4 : one variant of the dual model
%       - combined : classic + dual4
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- use_module(library(chr)).
:- use_module(model/channeling).
:- use_module(library(lists)).

%:- chr_option(check_guard_bindings, on).
:- chr_option(optimize, full).
:- chr_option(debug, on).

:- ['../utils']. % Import utility functions
:- ['../benchmarks/benchmarks'].

% Solve the Sudoku with the given name.
%
% @param Name       The name of the sudoku puzzle.
% @param Time       The time it took to solve the puzzle.
% @param Verbose    Flag denoting whether or not intermediate results should be printed.
sudoku_named(Name, Time, 0, Verbose) :-
    (Verbose -> write('Puzzle name : '), write(Name), nl ; true),
    puzzles(Puzzle, Name), % Find the puzzle with the given name
    sudoku(Puzzle, Time, 0, Verbose).

% Solve the given Sudoku puzzle.
%
% @param Puzzle     The sudoku puzzle to solve.
% @param Time       The time it took to solve the puzzle.
% @param Verbose    Flag denoting whether or not intermediate results should be printed.
sudoku(Puzzle, Time, 0, Verbose) :-
    length(Puzzle, N), % Get the puzzle dimension
    K is integer(sqrt(N)), % Get the dimension of blocks
    (Verbose -> write('Puzzle size : '), write(N), nl ; true),
    % Start solving procedure
    statistics(walltime, [_|[_]]),
    (Verbose -> write('Search prodecure started.'), nl ; true),
    solve(Puzzle, N, K), !,
    statistics(walltime, [_|[TimeMs]]),
    Time is TimeMs / 1000,
    (Verbose -> write('Solution found ...'), nl ; true),
    (Verbose -> write('Solution : '), print_sudoku(Puzzle), nl ; true),
    (Verbose -> write('Time : '), write(Time), nl ; true).
