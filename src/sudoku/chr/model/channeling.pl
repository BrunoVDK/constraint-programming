%
% Channeling constraints only model for the Sudoku CHR solver.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- module(channeling, [solve/3,register_puzzle/3]).
:- use_module(library(chr)).
:- use_module(library(lists)).

:- chr_constraint primal/4, dual1/4, dual2/4, dual3/4, first_fail/1, dualpos/4.
% dualpos(R,C,B,P) moet het zijn

% ---------------------------
%        CHR rule base
% ---------------------------

% Constraint propagation
% hier dan gewoon <var assignment> X <next model eg. dual1 met zelfde pos> X <pos omzetting> en in dat geval assignment
primal(R,C,1,[E]), dual1(B,P,L,D) # passive, dualpos(R,C,B,D) # passive <=> L \= 1

% First Fail heuristic
first_fail(L,N), primal # passive
    <=> choose_val(Val,Domain,N), V = Val, assignment(B,P,Val), first_fail(1,N).
first_fail(I,N) <=> I < N | NewI is I + 1, first_fail(NewI,N).
first_fail(_,_) <=> true.

% The value heuristic
choose_val(X, List, _) :- member(X, List). % nth1(N, List, X).

% -----------------------------------------------
%  Entry point + utility functions for the model
% -----------------------------------------------

% Solve the given Sudoku puzzle with the dual model.
%
% @param Puzzle     The input puzzle as a list of lists.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
solve(Puzzle, N, K) :-
    register_puzzle(Puzzle, N, K),
    first_fail(1,N).

% Register the pre-filled cells in the given puzzle.
%
% @param Puzzle     The input puzzle as a list of lists.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
% @note No safety checks are done.
register_puzzle(Puzzle, N, K) :-
    flatten(Puzzle, FlatPuzzle),
    create_domain(N, D),
    findall(R-C-B-P-N-D, (between(1,N,R),between(1,N,C),block(K,R,C,B),position(K,R,C,P)), Cells),
    maplist(generate_domain, Cells, FlatPuzzle), % The domains for cells that aren't assigned
    maplist(assign_value, Cells, FlatPuzzle). % Pre-filled cells
generate_domain(B-P-N-Domain, V) :- var(V) -> variable_domain(B,P,V,N,Domain) ; true.
assign_value(B-P-_-_, V) :- nonvar(V) -> assignment(B,P,V) ; true.
