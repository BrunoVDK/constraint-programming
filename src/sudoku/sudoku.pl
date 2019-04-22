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
%   The following flags may be used :
%       - eliminate_redundancy : if redundant constraints are to be removed (Demoen, 2012)
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- lib(ic_global).
:- compile('../utils.pl'). % Import utility functions
:- compile('benchmarks/benchmarks.pl').

% Solve the Sudoku with the given name.
%
% @param Name   The name of the sudoku puzzle.
sudoku_named(Name) :-
    puzzles(Puzzle, Name), % Find the puzzle with the given name
    sudoku(Puzzle).

% Solve the given Sudoku puzzle.
%
% @param Puzzle   The sudoku puzzle to solve.
sudoku(Puzzle) :-
    length(Puzzle, N), % Get the puzzle dimension
    K is integer(sqrt(N)), % Get the dimension of blocks
    write('Puzzle size = '), write(N), nl,
    % Set up model
    setup_model(classic, Puzzle, N, K, Variables),
    % Start search procedure
    write('Search prodecure started.'), nl,
    search(Variables, 0, first_fail, indomain, complete, [backtrack(Backtracks)]),
    % Display solution
    write('Solution : '), nl,
    print_sudoku(Puzzle),
    write('Backtracks : '), write(Backtracks), nl.

:- compile('model/classic.pl').

% Set up the classic model for the given puzzle.
%
% @param Puzzle     The puzzle list to declare domains for.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
setup_model(classic, Puzzle, N, K, flatten(Puzzle)) :-
    list_2d_to_array(Puzzle, PuzzleArray),
    declare_domains_classic(PuzzleArray, N, K),
    generate_constraints_classic(PuzzleArray, N, K).

% Set up the dual model for the given puzzle.
%
% @param Puzzle     The puzzle list to declare domains for.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
setup_model(dual, Puzzle, N, K, flatten(Puzzle)) :-
    list_2d_to_array(Puzzle, PuzzleArray),
    declare_domains_dual(PuzzleArray, N, K),
    generate_constraints_dual(PuzzleArray, N, K).

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
