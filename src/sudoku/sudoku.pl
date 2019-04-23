%
% Sudoku CLP solver.
%   The solver reads in a puzzle, sets up a model and searches for a solution.
%       It is assumed that for a puzzle of size N, blocks have a dimension of K x K
%       with K * K = N. This deals with several of the variants including hexadoku.
%
%   Several models can be made use of :
%       - classic : the classic ('primal') model
%       - dual : the dual model
%       - abstract : the abstract model (Laburthe)
%       - natural : the natural combined model
%       - linear : the linear programming model
%       - misc : the model from Thibault's thesis
%       - combined : the model which combines the classic model with the most performant model
%       - channeling : the model with only the channeling constraints of the combined model
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- lib(ic).
:- compile('../utils.pl'). % Import utility functions
:- compile('benchmarks/benchmarks.pl').

% Solve the Sudoku with the given name.
%
% @param Name   The name of the sudoku puzzle.
% @param Time   The time it took to solve the puzzle.
sudoku_named(Name, Time) :-
    write('Puzzle name : '), write(Name), nl,
    puzzles(Puzzle, Name), % Find the puzzle with the given name
    sudoku(Puzzle, Time).

% Solve the given Sudoku puzzle.
%
% @param Puzzle   The sudoku puzzle to solve.
% @param Time   The time it took to solve the puzzle.
sudoku(Puzzle, Time) :-
    length(Puzzle, N), % Get the puzzle dimension
    K is integer(sqrt(N)), % Get the dimension of blocks
    write('Puzzle size : '), write(N), nl,
    % Set up model
    setup_model(classic, Puzzle, N, K, Variables),
    % Start search procedure
    write('Search prodecure started.'), nl,
    statistics(hr_time, Start), % http://eclipseclp.org/doc/bips/kernel/env/statistics-2.html
    search(Variables, 0, first_fail, indomain, complete, [backtrack(Backtracks)]),
    statistics(hr_time, End),
    Time is End - Start,
    % Display solution
    write('Solution : '), nl,
    print_sudoku(Puzzle),
    write('Backtracks : '), write(Backtracks), nl,
    write('Time : '), write(Time), nl.

:- compile('model/classic.pl').

%
% Print the given Sudoku puzzle.
%
% @param    Puzzle An array representing a (solved) Sudoku puzzle.
print_sudoku(Puzzle) :-
    (foreach(Row, Puzzle) do
        (foreach(Element, Row) do
            write(Element), write(" ")
        ),
        nl
    ).
