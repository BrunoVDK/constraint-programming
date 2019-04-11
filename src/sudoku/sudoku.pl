%
% Sudoku classic viewpoint
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0
%

:- lib(ic)

% Entry point
sudoku(Name) :-
    puzzles(Puzzle, Name),
    dim(Puzzle, [N,N]),
    declare_domains(Puzzle, N),
    generate_constraints(Puzzle, N),
    search(Puzzle).

% Declare the domains
declare_domains(Puzzle, N) :-
    Puzzle :: 1..N.

% Generate the constraints
generate_constraints(Puzzle, N) :-
    for(I,1,N), param(Puzzle) do
        alldifferent(Puzzle[I,*]),
        alldifferent(Puzzle[*,J]),
    RootN is integer(sqrt(N)),
    multifor([I,J], 1, N, RootN), param(Puzzle, RootN) do
        alldifferent(concat(Puzzle[I..I+RootN-1,J..J+RootN-1]).
