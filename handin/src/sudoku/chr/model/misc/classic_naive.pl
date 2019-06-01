%
% Classic model for the Sudoku CHR solver.
%   This was just an early (naive) version for experimentation's sake.
%   It's extremely inefficient as it uses passive constraints.
%   To see it work you can test it out with the tarek_052 puzzle which takes about a minute ...
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- module(classic_naive, [solve/3,register_puzzle/3]).
:- use_module(library(chr)).
:- use_module(library(lists)).

:- chr_constraint cell/5, value/5, input_order/1.

% ---------------------------
%        CHR rule base
% ---------------------------

% Alldifferent constraints
value(R,C1,_,_,V1), value(R,C2,_,_,V2) ==> C1 \= C2, V1 == V2 | fail.
value(R1,C,_,_,V1), value(R2,C,_,_,V2) ==> R1 \= R2, V1 == V2 | fail.
value(_,_,B,P1,V1), value(_,_,B,P2,V2) ==> P1 \= P2, V1 == V2 | fail.

% Input Order heuristic
input_order(N), cell(R,C,B,P,V) # passive
    <=> var(V) | between(1,N,Val), V is Val, value(R,C,B,P,Val), input_order(N).
input_order(N), value(_,_,_,_,_) <=> input_order(N).
input_order(_) <=> true.

% -----------------------------------------------
%  Entry point + utility functions for the model
% -----------------------------------------------

% Solve the given Sudoku puzzle with the classic model.
%
% @param Puzzle     The input puzzle as a list of lists.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
solve(Puzzle, N, K) :-
    register_puzzle(Puzzle, N, K),
    input_order(N).

% Register the pre-filled cells in the given puzzle.
%
% @param Puzzle     The input puzzle as a list of lists.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
% @note No safety checks are done.
% @note Findall won't work here as it backtracks which undoes the insertion of constraints.
register_puzzle(Puzzle, N, K) :-
    flatten(Puzzle, FlatPuzzle),
    findall(R-C-B-P, (between(1,N,R),between(1,N,C),block(K,R,C,B),position(K,R,C,P)), Cells),
    maplist(generate_domain, Cells, FlatPuzzle).
generate_domain(R-C-B-P, V) :- (var(V) -> cell(R,C,B,P,V) ; value(R,C,B,P,V)).
