%
% Automatic benchmarks for the Sudoku CLP solver.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- compile(minimum_puzzles). % Minimum sudokus
:- compile(sudex_toledo). % Example puzzles

% Automatic benchmarking with the provided puzzles.
benchmark :-
    Puzzles = [ lambda,
                hard17,
                eastermonster,
                tarek_052,
                goldennugget,
                coloin,
                extra2,
                extra3,
                extra4,
                inkara2012,
                clue18,
                clue17,
                sudowiki_nb28,
                sudowiki_nb49],
    length(Puzzles, Nb),
    (foreach(Puzzle, Puzzles), fromto(0, In, Out, Total) do
        sudoku_named(Puzzle, Time),
        Out is In + Time
    ),
    write('Total time : '), write(Total), write(' ms'), nl,
    Average is Total / Nb,
    write('Average time : '), write(Average), write(' s'), nl.

% Automatic benchmarking with the minimum puzzles.
%
% @note The minimum puzzles are puzzles with 17 pre-filled cells. Less isn't possible.
%       Courtesy of Gordon Royle : http://staffhome.ecm.uwa.edu.au/~00013890/sudokumin.php
%       Minimum sudokus were converted to a readable format with a Haskell script included
%       in the repository.
benchmark_minimum :-
    nb_minimum(Nb), % Number of minimum puzzles to use
    (for(I, 1, Nb), fromto(0, In, Out, Total) do
        minimum_puzzles(Puzzle, I),
        sudoku(Puzzle, Time),
        Out is In + Time
    ),
    write('Total time : '), write(Total), write(' s'), nl,
    Average is Total / Nb,
    write('Average time : '), write(Average), write(' s'), nl.
nb_minimum(10).
