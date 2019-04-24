%
% Utility functions.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

% Convert the given 2-dimensional list to an array.
%
% @param    The list to convert to an array.
% @param    The array representing the same collection as the given list.
list_2d_to_array(List, Array) :-
    length(List, N),
    dim(Array, [N,N]),
    array_list(Array, Rows),
    (foreach(ListRow, List), foreach(Row, Rows) do
        array_list(Row, ListRow)
    ).

% For a given Sudoku row and column, determine the block where the cell is located.
%
% @param K          The block size.
% @param Row        The row of the cell.
% @param Column     The column of the cell.
block(K, Row, Column, Block) :-
    Block is K * ((Row-1) // K) + ((Column-1) // K) + 1.

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

% Checks whether the given column and block intersect.
%
% @param N      The puzzle size.
% @param K      The block size.
% @param Column The column number.
% @param Block  The block number.
block_column(N, K, Column, Block) :-
    between(1, N, 1, Block),
    Max is K * ((Block-1) mod K) + 3,
    between(1, N, 1, Column),
    Column =< Max,
    Column > Max - K.
