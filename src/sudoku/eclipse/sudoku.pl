%
% Sudoku CLP solver.
%   The solver reads in a puzzle, sets up a model and searches for a solution.
%       It is assumed that for a puzzle of size N, blocks have a dimension of K x K
%       with K * K = N. This deals with several of the variants including hexadoku.
%
%   Several models can be made use of :
%       - classic : the classic ('primal') model
%       - member : model enforcing singular occurrence of each value in each row/column/block
%       - laburthe : experiments with Laburthe's models (primal + dual + abstract)
%       - boolean : two boolean models
%       - dual : four variants of the dual model
%       - channeling : the model with only the channeling constraints of the combined model
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

use_model(dual). % The model that is to be used

:- lib(ic).
:- import alldifferent/1 from ic_global.
:- import bool_channeling/3 from ic_global.

:- compile('../utils.pl'). % Import utility functions
:- compile('../benchmarks/benchmarks').

%:- compile('model/channeling').
:- compile('model/dual').
%:- compile('model/boolean').
%:- compile('model/member').
%:- compile('model/classic').
%:- compile('model/laburthe').

% Solve the Sudoku with the given name.
%
% @param Name       The name of the sudoku puzzle.
% @param Time       The time it took to solve the puzzle.
% @param Backtracks The recorded number of backtracks.
% @param Verbose    Flag denoting whether or not intermediate results should be printed.
sudoku_named(Name, Time, Backtracks, Verbose) :-
    (Verbose -> write('Puzzle name : '), write(Name), nl ; true),
    puzzles(Puzzle, Name), % Find the puzzle with the given name
    sudoku(Puzzle, Time, Backtracks, Verbose).

% Solve the given Sudoku puzzle.
%
% @param Puzzle     The sudoku puzzle to solve.
% @param Time       The time it took to solve the puzzle.
% @param Backtracks The recorded number of backtracks.
% @param Verbose    Flag denoting whether or not intermediate results should be printed.
sudoku(Puzzle, Time, Backtracks, Verbose) :-
    length(Puzzle, N), % Get the puzzle dimension
    K is integer(sqrt(N)), % Get the dimension of blocks
    (Verbose -> write('Puzzle size : '), write(N), nl ; true),
    % Set up model
    use_model(Model),
    statistics(hr_time, Start), % http://eclipseclp.org/doc/bips/kernel/env/statistics-2.html
    setup_model(Model, Puzzle, N, K, Variables),
    % Start search procedure
    (Verbose -> write('Search prodecure started.'), nl ; true),
    search(Variables, 0, first_fail, indomain, complete, [backtrack(Backtracks)]), !,
    statistics(hr_time, End),
    Time is End - Start,
    (Verbose -> write('Solution found ...'), nl ; true),
    % Display solution
    read_solution(Model, Variables, Puzzle, N, K, Solution),
    (Verbose -> write('Solution : '), print_sudoku(Solution), nl ; true),
    (Verbose -> write('Backtracks : '), write(Backtracks), nl ; true),
    (Verbose -> write('Time : '), write(Time), nl ; true).

%
% Print the given Sudoku puzzle.
%
% @param Puzzle     An array representing a (solved) Sudoku puzzle.
print_sudoku(Puzzle) :-
    nl,
    (foreach(Row, Puzzle) do
        (foreach(Element, Row) do
            write(Element), write(" ")
        ),
        nl
    ).
