%
% Classic model for the Sudoku CLP solver.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

% Declare the domains for the given Sudoku puzzle.
%
% @param    Puzzle The puzzle array to declare the domains for.
% @param    N The dimension of the puzzle.
declare_domains_classic(Puzzle, N, _K) :-
    Puzzle :: 1..N. % All squares should have an integer from 1 to 9

% Generate the constraints for the given Sudoku puzzle.
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
        alldifferent(Puzzle[I,1..N]), % Different integer in row cells
        alldifferent(Puzzle[1..N,I])) % Different integer in column cells
    ,
    % This code goes through I = 1->9, J = 1->9, step = K
    %   It considers each block and enforces the constraint that each of the block's
    %   elements should be different
    %   multifor/3 is described here http://eclipseclp.org/doc/tutorial/tutorial025.html
    (multifor([I,J], 1, N, K), param(Puzzle, K) do
        % In ECLiPSe 7.0 subscript gives an array, in 6.1 it gives a list
        % (-> use concat/flatten depending on version)
        % alldifferent(concat(Puzzle[I..I+K-1,J..J+K-1])) % Different integers in block cells
        alldifferent(flatten(Puzzle[I..I+K-1,J..J+K-1])) % Different integers in block cells
    ).
