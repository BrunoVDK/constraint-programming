%
% Utility functions for the Sudoku solvers.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

% Logging/Debugging utility functions.
enable(debug) :- true.
log :- (enable(debug) -> write('---'), nl ; true).
log(Var) :- (enable(debug) -> write(Var), nl ; true).

% For a given Sudoku row and column, determine the block where the cell is located.
%
% @param K          The block size.
% @param Row        The row of the cell.
% @param Column     The column of the cell.
block(K, Row, Column, Block) :-
    Block is K * ((Row-1) // K) + ((Column-1) // K) + 1.

% For a given Sudoku row and column, determine the block position where the cell is located.
%
% @param K          The block size.
% @param Row        The row of the cell.
% @param Column     The column of the cell.
position(K, Row, Column, Position) :-
    Position is ((Column-1) mod K) + K * ((Row-1) mod K) + 1.

% For a given Sudoku block and cell index (relative to that block),
%   determine the row where the respective cell is located.
%
% @param K       The block size.
% @param Block   The block in which the cell is located.
% @param Cell    The block cell.
row(K, Block, Cell, Row) :-
    BlockRow is (Block-1) // K,
    Row is (BlockRow * K) + ((Cell-1) // K) + 1.

% For a given Sudoku block and cell index (relative to that block),
%   determine the column where the respective cell is located.
%
% @param K       The block size.
% @param Block   The block in which the cell is located.
% @param Cell    The block cell.
column(K, Block, Cell, Column) :-
    BlockColumn is (Block-1) mod K,
    Column is (BlockColumn * K) + ((Cell-1) mod K) + 1.

% Generate domain for given N. This is simply the list of integers from 1 to N.
create_domain(N, Domain) :-
    findall(V, between(1,N,V), Domain).

%
% Print the given Sudoku puzzle.
%
% @param Puzzle     An array representing a (solved) Sudoku puzzle.
print_sudoku(Puzzle) :-
    findall(_, (member(Row,Puzzle), nl, member(El,Row), write(El), write(" ")), _).

% Calculate the sum of the elements in the given list.
%
% @param List   The list (eg. of integers).
% @note We implemented this ourselves to make this file compatible
%           with both the CLP as well as the CHR implementation.
sumlist(List, Sum) :- sumlist(List, 0, Sum).
sumlist([], Sum, Sum).
sumlist([X|Xs], Acc, Sum) :-
    NewAcc is Acc + X,
    sumlist(Xs, NewAcc, Sum).
