%
% Dual model for the Sudoku CLP solver (CHR version).
%   This one corresponds to dual3 in the ECLiPSe version.
%   - Block x Position implies Value
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

% Set up the model for the given puzzle.
%
% @param Puzzle     The input puzzle (a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
setup_model(dual, Puzzle, N, K, Vars) :-
    dim(Variables, [N,N]),
    declare_domains_dual(Variables, N, K),
    generate_constraints_dual(Variables, Puzzle, N, K),
    Vars is collection_to_list(Variables).

% Declare the domains for the given puzzle.
%
% @param Variables  The variable array to declare the domains for.
% @param N          The dimension of the puzzle.
declare_domains_dual(Variables, N, _K) :-
    Variables :: 1..N. % Every dual model has the same variable domains

% Generate the constraints for the given puzzle.
%
% @param Variables  The variables to generate constraints for.
% @param Puzzle     The input puzzle (a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
generate_constraints_dual(Variables, Puzzle, N, K) :-
    fail.

% Transform the assignments to the decision variables to a solved puzzle.
%
% @param Variables  The decision variables (should be assigned).
% @param Puzzle     The input puzzle (this is a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
% @param Solution   The puzzle's solution corresponding to the assignments
%                       to the variables.
read_solution(dual, Variables, _, N, K, Solution) :-
    fail.
