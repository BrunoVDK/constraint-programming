%
% Classic model for the Sudoku CLP solver (CHR version).
%
% The following flags may be used :
%       - eliminate_redundancy : if redundant constraints are to be removed (Demoen, 2012)
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

combo :- fail. % Set true/fail if two viewpoints are to be combined.
eliminate_redundancy :- fail.

% Set up the classic model for the given puzzle.
%
% @param Puzzle     The input puzzle (a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
setup_model(classic, Puzzle, N, K, flatten(Puzzle)) :-
    list_2d_to_array(Puzzle, PuzzleArray),
    declare_domains_classic(PuzzleArray, N, K),
    generate_constraints_classic(PuzzleArray, N, K).

% Declare the domains for the given puzzle.
%
% @param Puzzle The puzzle array to declare the domains for.
% @param N      The dimension of the puzzle.
declare_domains_classic(Puzzle, N, _K) :-
    Puzzle :: 1..N. % All squares should have an integer from 1 to N

% Generate the constraints for the given puzzle.
%
% @param Puzzle The puzzle array to generate constraints for.
% @param N      The dimension of the puzzle.
% @param K      The dimension of blocks.
% @note The part selection and use of concatenation was inspired by code
%               provided by ECLiPSe. https://eclipseclp.org/examples/sudoku.ecl.txt
generate_constraints_classic(Puzzle, N, K) :-
    fail.

% Define rows and columns that are to be skipped if redundant 'big constraints' are disregarded.
% This corresponds to the first Missing(6) model in appendix of (Demoen, 2012).
skip(I, 9) :- eliminate_redundancy, (I = 2 ; I = 5 ; I = 8).

% Transform the assignments to the decision variables to a solved puzzle.
%
% @param Variables  The decision variables (should be assigned).
% @param Puzzle     The input puzzle (this is a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
% @param Solution   The puzzle's solution corresponding to the assignments to the variables.
read_solution(classic, _, Puzzle, _, _, Puzzle).
