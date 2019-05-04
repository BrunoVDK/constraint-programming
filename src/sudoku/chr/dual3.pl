%
% Dual model for the Sudoku CHR solver.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- module(dual3, [solve/3,register_puzzle/3]).
:- use_module(library(chr)).
:- use_module(library(lists)).

:- chr_constraint assignment/3, variable_domain/5, first_fail/2, input_order/1, blocksize/1.

% ---------------------------
%        CHR rule base
% ---------------------------

% Constraint propagation (forward checking)
assignment(B,_,Val) \ variable_domain(B,P,V,L,Domain) # passive <=>
    select(Val,Domain,NewDomain) | NewL is L-1, NewL > 0, variable_domain(B,P,V,NewL,NewDomain).
assignment(B1,P1,Val), blocksize(K) # passive \ variable_domain(B2,P2,V,L,Domain) # passive <=>
    row(K,B1,P1,R), row(K,B2,P2,R), select(Val,Domain,NewDomain) |
    NewL is L-1, NewL > 0, variable_domain(B2,P2,V,NewL,NewDomain).
assignment(B1,P1,Val), blocksize(K) # passive \ variable_domain(B2,P2,V,L,Domain) # passive <=>
    column(K,B1,P1,R), column(K,B2,P2,R), select(Val,Domain,NewDomain) |
    NewL is L-1, NewL > 0, variable_domain(B2,P2,V,NewL,NewDomain).
assignment(_,_,_) <=> true.

% First Fail heuristic
first_fail(L,N), variable_domain(B,P,V,L,Domain) # passive
    <=> choose_val(Val,Domain,N), V = Val, assignment(B,P,Val), first_fail(1,N).
first_fail(I,N) <=> I < N | NewI is I + 1, first_fail(NewI,N).
first_fail(_,_), blocksize(_) <=> true.

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
    blocksize(K),
    register_puzzle(Puzzle, N, K),
    %input_order(N),
    first_fail(1,N).

% Register the pre-filled cells in the given puzzle.
%
% @param Puzzle     The input puzzle as a list of lists.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
% @note No safety checks are done.
% @note Findall won't work here as it backtracks which undoes the insertion of constraints.
% @note Probably should have been written with recursion.
register_puzzle(Puzzle, N, K) :-
    flatten(Puzzle, FlatPuzzle),
    create_domain(N, D),
    findall(B-P-N-D, (between(1,N,R),between(1,N,C),block(K,R,C,B),position(K,R,C,P)), Cells),
    maplist(generate_domain, Cells, FlatPuzzle), % The domains for cells that aren't assigned
    maplist(assign_value, Cells, FlatPuzzle). % Pre-filled cells
generate_domain(B-P-N-Domain, V) :- var(V) -> variable_domain(B,P,V,N,Domain) ; true.
assign_value(B-P-_-_, V) :- nonvar(V) -> assignment(B,P,V) ; true.
