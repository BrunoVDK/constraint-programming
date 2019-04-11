%
% Sudoku classic viewpoint
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0
%

:- lib(ic).
:- compile(sudex_toledo).

% Solve the Sudoku with the given name.
%
% @param    Name The name of the sudoku puzzle.
sudoku(Name) :-
    puzzles(Puzzle, Name), % Find the puzzle with the given name
    dim(Puzzle, [N,N]), % Get the puzzle dimension
    declare_domains(Puzzle, N),
    generate_constraints(Puzzle, N),
    labeling(Puzzle).

% Declare the domains for the given Sudoku puzzle.
%
% @param    Puzzle The puzzle to declare the domains for.
% @param    N The dimension of the puzzle.
declare_domains(Puzzle, N) :-
    Puzzle :: 1..N. % All squares should have an integer from 1 to 9

% Generate the constraints for the given Sudoku puzzle.
%
% @param    Puzzle The puzzle to generate constraints for.
% @param    N The dimension of the puzzle.
% @note     The part selection and use of concatenation was inspired by code
%               provided by ECLiPSe. https://eclipseclp.org/examples/sudoku.ecl.txt
generate_constraints(Puzzle, N) :-
    (for(I,1,N), param(Puzzle) do % Go through every row & column
        alldifferent(Puzzle[I,*]), % Different integer in every element of every row
        alldifferent(Puzzle[*,I])) % Different integer in every element of every column
    ,
    RootN is integer(sqrt(N)), % Get the dimension of blocks
    % This code goes through I = 1->9, J = 1->9, step = RootN
    %   Then it
    %   multifor/3 is described here http://eclipseclp.org/doc/tutorial/tutorial025.html
    (multifor([I,J], 1, N, RootN), param(Puzzle, RootN) do
        % Different integer in every element of every block
        alldifferent(concat(Puzzle[I..I+RootN-1,J..J+RootN-1]))
    ).
