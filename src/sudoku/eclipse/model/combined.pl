%
% Combined (classic+dual4) model for the Sudoku CLP solver.
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
    dim(DualArray, [N,N]),
    declare_domains_combined(PuzzleArray, DualArray, N, K),
    generate_constraints_combined(PuzzleArray, DualArray, N, K),
    collection_to_list(DualArray, Dual),
    append(Puzzle, Dual, Variables).

% Declare the domains for the given puzzle.
%
% @param Primal The puzzle array to declare the domains for.
% @param Dual   The dual variables array to generate constraints for.
% @param N      The dimension of the puzzle.
declare_domains_combined(Primal, Dual, N, _K) :-
    Primal :: 1..N,
    Dual :: 1..N.

% Generate the constraints for the given puzzle.
%
% @param Primal The puzzle array to generate constraints for.
% @param Dual   The dual variables array to generate constraints for.
% @param N      The dimension of the puzzle.
% @param K      The dimension of blocks.
generate_constraints_combined(Primal, Dual, N, K) :-
    % Primal
    (for(I,1,N), param(Primal, N) do % Go through every row & column
        alldifferent(Primal[I,1..N]), % Different integer in row cells
        alldifferent(Primal[1..N,I]) % Different integer in column cells
    ),
    (multifor([I,J], 1, N, K), param(Primal, K) do
        alldifferent(flatten(Primal[I..I+K-1,J..J+K-1])) % Different integers in block cells
    ),
    % Dual
    % (1) register pre-filled cells
    % (2) row constraints
    (for(R, 1, N), param(Dual, N, K) do %
        alldifferent(Dual[R,1..N]), % (2)
        (for(C, 1, N), param(K, R, Dual) do
            (nonvar(X) ->
                block(K, R, C, Block),
                position(K, R, C, Position),
                Dual[Block,X] is Position % (1)
            ; true)
        )
    ),
    % Channeling
    (multifor([Row,Column,Value], 1, N), param(Primal, Dual, K) do
        #=(Primal[Row,Column], Value, Bool),
        block(K, Row, Column, Block),
        position(K, Row, Column, Position),
        #=(Dual[Block,Value], Position, Bool)
    ).

% Transform the assignments to the decision variables to a solved puzzle.
%
% @param Variables  The decision variables (should be assigned).
% @param Puzzle     The input puzzle (this is a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
% @param Solution   The puzzle's solution corresponding to the assignments to the variables.
read_solution(_, Puzzle, _, _, Puzzle).
