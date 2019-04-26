%
% Member model for the Sudoku CLP solver.
%   Meaning that every integer from 1 to 9 is made to be a member of every
%   row, column and block.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- import occurrences/3 from ic_global.

% Set up the model for the given puzzle.
%
% @param Puzzle     The input puzzle (a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
setup_model(member, Puzzle, N, K, flatten(Puzzle)) :-
    list_2d_to_array(Puzzle, PuzzleArray),
    declare_domains_member(PuzzleArray, N, K),
    generate_constraints_member(PuzzleArray, N, K).

% Declare the domains for the given puzzle.
%
% @param Puzzle The puzzle array to declare the domains for.
% @param N      The dimension of the puzzle.
declare_domains_member(Puzzle, N, _K) :-
    Puzzle :: 1..N.

% Generate the constraints for the given puzzle.
%
% @param Puzzle The puzzle array to generate constraints for.
% @param N      The dimension of the puzzle.
% @param K      The dimension of blocks.
generate_constraints_member(Puzzle, N, K) :-
    % Each value should occur just once in each row/column
    (multifor([I,V], 1, N), param(N, Puzzle) do
        ic_global:occurrences(V, Puzzle[I,1..N], 1),
        ic_global:occurrences(V, Puzzle[1..N,I], 1)
    ),
    % Each value should occur just once in each block
    (multifor([V,R,C], 1, N, [1,K,K]), param(K, Puzzle) do
        ic_global:occurrences(V, flatten(Puzzle[R..R+K-1,C..C+K-1]), 1)
    ).

% Transform the assignments to the decision variables to a solved puzzle.
%
% @param Variables  The decision variables (should be assigned).
% @param Puzzle     The input puzzle (this is a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
% @param Solution   The puzzle's solution corresponding to the assignments to the variables.
read_solution(member, _, Puzzle, _N, _K, Puzzle).
