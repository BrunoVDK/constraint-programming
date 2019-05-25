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
%       - combined : primal (classic) + dual combination (channeling)
%       - combined_member : primal (classic) + member combination (channeling)
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- lib(ic).
:- import alldifferent/1 from ic_global.
:- import bool_channeling/3 from ic_global.

:- compile('../utils.pl'). % Import utility functions
:- compile('../benchmarks/benchmarks').

:- compile('model/channeling'). % Choose the model here

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
    statistics(hr_time, Start), % http://eclipseclp.org/doc/bips/kernel/env/statistics-2.html
    setup_model(Puzzle, N, K, Variables),
    % Start search procedure
    (Verbose -> write('Search prodecure started.'), nl ; true),
    search(Variables, 0, first_fail, indomain_min, complete, [backtrack(Backtracks)]), !,
    statistics(hr_time, End),
    Time is End - Start,
    (Verbose -> write('Solution found ...'), nl ; true),
    % Display solution
    read_solution(Variables, Puzzle, N, K, Solution),
    (Verbose -> write('Solution : '), print_sudoku(Solution), nl ; true),
    (Verbose -> write('Backtracks : '), write(Backtracks), nl ; true),
    (Verbose -> write('Time : '), write(Time), nl ; true).

% Convert the given 2-dimensional list to an array.
%
% @param    The list to convert to an array.
% @param    The array representing the same collection as the given list.
list_2d_to_array(List, Array) :-
    length(List, N),
    dim(Array, [N,N]),
    array_list(Array, Rows),
    (foreach(ListRow, List), foreach(Row, Rows) do
        array_list(Row, ListRow)
    ).
