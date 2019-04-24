%
% Template model for the Sudoku CLP solver.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

% Set up the model for the given puzzle.
%
% @param Puzzle     The input puzzle (a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
setup_model(template, Puzzle, N, K, flatten(Puzzle)) :-
    list_2d_to_array(Puzzle, PuzzleArray),
    declare_domains_classic(PuzzleArray, N, K),
    generate_constraints_classic(PuzzleArray, N, K).

% Declare the domains for the given puzzle.
%
% @param Puzzle The puzzle array to declare the domains for.
% @param N      The dimension of the puzzle.
declare_domains_template(Puzzle, N, _K) :-
    fail.

% Generate the constraints for the given puzzle.
%
% @param Puzzle The puzzle array to generate constraints for.
% @param N      The dimension of the puzzle.
% @param K      The dimension of blocks.
generate_constraints_template(Puzzle, N, K) :-
    fail.

% Transform the assignments to the decision variables to a solved puzzle.
%
% @param Variables  The decision variables (should be assigned).
% @param Puzzle     The input puzzle (this is a list).
% @param Solution   The puzzle's solution corresponding to the assignments to the variables.
read_solution(_, Puzzle, Solution) :-
    fail.
