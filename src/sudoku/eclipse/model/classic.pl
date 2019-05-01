%
% Classic model for the Sudoku CLP solver.
%
% The following flags may be used :
%       - eliminate_redundancy : if redundant constraints are to be removed (Demoen, 2012)
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

combo :- fail. % Set true/fail if two viewpoints are to be combined.
eliminate_redundancy :- fail. % (Demoen)
:- import alldifferent_matrix/1 from ic_global.

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
    (for(I,1,N), param(Puzzle, N) do % Go through every row & column
        % Note that only ECLiPSe 7.0 supports the '*' subscript which is equivalent to '1..N'
        %   See changelog http://eclipseclp.org/relnotes/rel70.html
        % alldifferent(Puzzle[I,*]), % Different integer in row cells
        % alldifferent(Puzzle[*,I])) % Different integer in column cells
        (\+ skip(I, N) ->
            alldifferent(Puzzle[I,1..N]), % Different integer in row cells
            alldifferent(Puzzle[1..N,I]) % Different integer in column cells
        ;
            true
        )
        % alldifferent_matrix(Puzzle) % Much slower, less backtracks
    ),
    % This code goes through I = 1->N, J = 1->N, step = K
    %   It considers each block and enforces the constraint that each of the block's
    %   elements should be different
    %   multifor/3 is described here http://eclipseclp.org/doc/tutorial/tutorial025.html
    (multifor([I,J], 1, N, K), param(Puzzle, K) do
        % In ECLiPSe 7.0 subscript gives an array, in 6.1 it gives a list
        % (-> use concat/flatten depending on version)
        % alldifferent(concat(Puzzle[I..I+K-1,J..J+K-1])) % Different integers in block cells
        alldifferent(flatten(Puzzle[I..I+K-1,J..J+K-1])) % Different integers in block cells
    ).

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
