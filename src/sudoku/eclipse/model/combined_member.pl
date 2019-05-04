%
% Combined (classic+member) model for the Sudoku CLP solver.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

% Set up the combined model for the given puzzle.
%
% @param Puzzle     The input puzzle (a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
setup_model(Puzzle, N, K, flatten(Variables)) :-
    list_2d_to_array(Puzzle, PuzzleArray),
    dim(MemberArray, [N,N]),
    declare_domains_combined(PuzzleArray, MemberArray, N, K),
    generate_constraints_combined(PuzzleArray, MemberArray, N, K),
    collection_to_list(MemberArray, Member),
    append(Puzzle, Member, Variables).

% Declare the domains for the given puzzle.
%
% @param Primal The puzzle array to declare the domains for.
% @param Member The member variables array to generate constraints for.
% @param N      The dimension of the puzzle.
declare_domains_combined(Primal, Member, N, _K) :-
    Primal :: 1..N,
    Member :: 1..N.

% Generate the constraints for the given puzzle.
%
% @param Primal The puzzle array to generate constraints for.
% @param Member The member variables array to generate constraints for.
% @param N      The dimension of the puzzle.
% @param K      The dimension of blocks.
generate_constraints_combined(Primal, Member, N, K) :-
    % Primal
    (for(I,1,N), param(Primal, N) do % Go through every row & column
        alldifferent(Primal[I,1..N]), % Different integer in row cells
        alldifferent(Primal[1..N,I]) % Different integer in column cells
    ),
    (multifor([I,J], 1, N, K), param(Primal, K) do
        alldifferent(flatten(Primal[I..I+K-1,J..J+K-1])) % Different integers in block cells
    ),
    % Member
    (foreachelem(P, Primal), foreachelem(M, Member) do (nonvar(P) -> M is P ; true)),
    (multifor([I,V], 1, N), param(N, Member) do
        ic_global:occurrences(V, Member[I,1..N], 1),
        ic_global:occurrences(V, Member[1..N,I], 1)
    ),
    (multifor([V,R,C], 1, N, [1,K,K]), param(K, Member) do
        ic_global:occurrences(V, flatten(Member[R..R+K-1,C..C+K-1]), 1)
    ),
    % Channeling
    (multifor([Row,Column,Value], 1, N), param(Primal, Member) do
        #=(Primal[Row,Column], Value, Bool),
        #=(Member[Row,Column], Value, Bool)
    ).

% Transform the assignments to the decision variables to a solved puzzle.
%
% @param Variables  The decision variables (should be assigned).
% @param Puzzle     The input puzzle (this is a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
% @param Solution   The puzzle's solution corresponding to the assignments to the variables.
read_solution(_, Puzzle, _, _, Puzzle).
