%%
%% Sudoku CLP solver.
%%
%% @author   Michaël Dooreman & Bruno Vandekerkhove
%% @version  1.0
%%

:- lib(ic).
:- compile('../utils.pl'). % Import utility functions
:- compile(sudex_toledo). % Example puzzles

% Solve the Sudoku with the given name.
%
% @param    Name The name of the sudoku puzzle.
sudoku(Name) :-
    puzzles(PuzzleList, Name), % Find the puzzle with the given name
    % Transform the input list to an array of arrays
    %   First, the rows are transformed to arrays.
    %   The resulting list of arrays is transformed to an array.
    (foreach(RowList, PuzzleList), foreach(Row, Rows) do
        array_list(Row, RowList)
    ),
    array_list(Puzzle, Rows),
    % Get the puzzle dimension
    dim(Puzzle, [N,N]),
    write('Input dimension = '), write(N), nl,
    % Solve the Sudoku
    declare_domains(Puzzle, N),
    generate_constraints(Puzzle, N),
    labeling(Puzzle),
    print_sudoku(Puzzle).

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
        % Note that only ECLiPSe 7.0 supports the '*' subscript which is equivalent to '1..N'
        %   See changelog http://eclipseclp.org/relnotes/rel70.html
        alldifferent(Puzzle[I,*]), % Different integer in every element of every row
        alldifferent(Puzzle[*,I])) % Different integer in every element of every column
    ,
    RootN is integer(sqrt(N)), % Get the dimension of blocks
    % This code goes through I = 1->9, J = 1->9, step = RootN
    %   It considers each block and enforces the constraint that each of the block's
    %   elements should be different
    %   multifor/3 is described here http://eclipseclp.org/doc/tutorial/tutorial025.html
    %   concat/2 is described here http://eclipseclp.org/doc/bips/lib/matrix_util/concat-2.html
    (multifor([I,J], 1, N, RootN), param(Puzzle, RootN) do
        % Different integer in every element of every block
        alldifferent(concat(Puzzle[I..I+RootN-1,J..J+RootN-1]))
    ).

%%
%%  TESTING
%%

%
% Print the given Sudoku puzzle.
%
% @param    Puzzle An array representing a (solved) Sudoku puzzle.
%
print_sudoku(Puzzle) :-
    (foreach(Row, Puzzle) do
        (foreach(Element, Row) do
            write(Element), write(" ")
        ),
        nl
    ).
