%
% Member viewpoint for the Sudoku CHR solver.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- module(channeling, [solve/3,register_puzzle/3]).
:- use_module(library(chr)).
:- use_module(library(lists)).

:- chr_constraint occ/3, occ_row/2, occ_col/2, occ_block/2.

% ---------------------------
%        CHR rule base
% ---------------------------

occ(R,C,V)

occ_row(V,Vars,Domains,1) <=> maplist(exclude_val(V), Domains).
occurrences(V,Vars,Domains,0) <=> nth1(

% -----------------------------------------------
%  Entry point + utility functions for the model
% -----------------------------------------------

exclude_val(V, List) :- exclude(is(V), List)).

% Solve the given Sudoku puzzle with the dual model.
%
% @param Puzzle     The input puzzle as a list of lists.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
solve(Puzzle, N, K) :-
    create_domain(N, D),
    domain(D),
    register_puzzle(Puzzle, N, K).

% Register the pre-filled cells in the given puzzle.
%
% @param Puzzle     The input puzzle as a list of lists.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
% @note No safety checks are done.
register_puzzle(Puzzle, N, K) :-
    flatten(Puzzle, FlatPuzzle),
    unequalities(FlatPuzzle),
    findall(Col, (between(1,K,I), maplist(nth1(I),Puzzle,Col)), Columns),
    findall(Block, (between(1,K,I),between(1,K,J),block(K,R,C,B)), Cells),
    maplist(unequal, Puzzle), % The domains for cells that aren't assigned
    maplist(assign_value, Cells, FlatPuzzle). % Pre-filled cells
value(X,V) :- nonvar(V) -> assignment(B,P,V) ; true.
