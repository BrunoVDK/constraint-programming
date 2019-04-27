%
% Channeling-only model for the Sudoku CLP solver.
%   This is described in (Dotu, 2003), which discusses the quasigroup completion problem.
%   In it, two dual models and their respective channeling constraints are considered.
%   It is proven that any assignment to the dual models satisfying those channeling
%       constraints satisfies the 'primal' constraints.
%   No alldifferent constraints are needed (this may even perform better, eg. Walsch, 2001).
% N.B. : Sudoku is more complex than quasigroup completion so more dual models are needed.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

variant(extended) :- fail. % Set to true/fail if you want to use channeling between dual models

% Set up the model for the given puzzle.
%
% @param Puzzle     The input puzzle (a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
setup_model(channeling, Puzzle, N, K, flatten(Puzzle)) :-
    % This time the domain constraints are in the constraint generation
    %   function because it's easier to write it that way.
    list_2d_to_array(Puzzle, PuzzleArray),
    generate_constraints_channeling(PuzzleArray, N, K).

% Generate the constraints for the given puzzle.
%   This includes constraints on domains.
%
% @param Puzzle The puzzle array to generate constraints for.
% @param N      The dimension of the puzzle.
% @param K      The dimension of blocks.
generate_constraints_channeling(Puzzle, N, K) :-
    % Declare variables and generate domain constraints
    dim(Dual1, [N,N]), Dual1 :: 1..N,
    dim(Dual2, [N,N]), Dual2 :: 1..N,
    dim(Dual3, [N,N]), Dual3 :: 1..N,
    Puzzle :: 1..N,
    (multifor([Row,Column,Value], 1, N), param(Puzzle, Dual1, Dual2, Dual3, K) do
        (variant(extended) ->
            #=(Puzzle[Row,Column], Value, Bool),
            #=(Dual1[Row,Value], Column, Bool),
            #=(Dual2[Column,Value], Row, Bool),
            block(K, Row, Column, Block),
            position(K, Row, Column, Position),
            #=(Dual3[Block,Value], Position, Bool)
        ; % No channeling between dual models
            #=(Puzzle[Row,Column], Value, Bool1),
            #=(Dual1[Row,Value], Column, Bool1),
            #=(Puzzle[Row,Column], Value, Bool2),
            #=(Dual2[Column,Value], Row, Bool2),
            block(K, Row, Column, Block),
            position(K, Row, Column, Position),
            #=(Puzzle[Row,Column], Value, Bool3),
            #=(Dual3[Block,Value], Position, Bool3)
        )
    ).

% Transform the assignments to the decision variables to a solved puzzle.
%
% @param Variables  The decision variables (should be assigned).
% @param Puzzle     The input puzzle (this is a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
% @param Solution   The puzzle's solution corresponding to the assignments to the variables.
read_solution(channeling, _Variables, Puzzle, _N, _K, Puzzle).
