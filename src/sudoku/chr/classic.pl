%
% Classic model for the Sudoku CHR solver.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

combo :- fail. % Set true/fail if two viewpoints are to be combined.

% Set up the classic model for the given puzzle.
%
% @param Puzzle     The input puzzle (a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
setup_model(classic, Puzzle, N, K) :-
    fail.

% Set up the classic model for the given puzzle.
%
% @param Puzzle     The input puzzle (a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
solve(classic, Puzzle, N, K) :-
    fail.
