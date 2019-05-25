%
% Classic model for the Sudoku CHR solver (alternative, first version).
%
% Available variable heuristics include first_fail and input_order.
% Available value heuristics include indomain_min and indomain_max.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- module(classic_alt, [solve/3,register_puzzle/3]).
:- use_module(library(chr)).
:- use_module(library(lists)).

% val/3 : Row x Column x Value
% var/5 : Row x Column x Corresponding puzzle variable x Domain length (first fail!) x Domain
% first_fail/1 : Counter x Maximum domain length
:- chr_constraint val/3, var/5, first_fail/2, input_order/0, blocksize/1.

% ---------------------------
%        CHR rule base
% ---------------------------

% Constraint propagation (forward checking)
%val(R,_,Val) \ var(R,C,V,_,[Val,LV]) # passive <=> val(R,C,LV), V = LV.
%val(R,_,Val) \ var(R,C,V,_,[LV,Val]) # passive <=> val(R,C,LV), V = LV.
%val(_,C,Val) \ var(R,C,V,_,[Val,LV]) # passive <=> val(R,C,LV), V = LV.
%val(_,C,Val) \ var(R,C,V,_,[LV,Val]) # passive <=> val(R,C,LV), V = LV.
val(R,_,Val) \ var(R,C,V,L,Domain) # passive <=>
    select(Val,Domain,NewDomain) | L > 1, NewL is L-1, var(R,C,V,NewL,NewDomain).
val(_,C,Val) \ var(R,C,V,L,Domain) # passive <=>
    select(Val,Domain,NewDomain) | L > 1, NewL is L-1, var(R,C,V,NewL,NewDomain).
val(R1,C1,Val), blocksize(K) # passive \ var(R2,C2,V,L,Domain) # passive <=>
    block(K,R1,C1,B), block(K,R2,C2,B), select(Val,Domain,NewDomain)
    | L > 1, NewL is L-1, var(R2,C2,V,NewL,NewDomain).
%val(_,_,_) \ var(R,C,Var,1,[Val]) # passive <=> Var = Val, val(R,C,Val). % skip first_fail
val(_,_,_) <=> true.

% Input Order heuristic
%input_order, var(R,C,Var,_,Domain) # passive
    %<=> choose_val(Val,Domain), Var = Val, val(R,C,Val), input_order.

% First Fail heuristic
first_fail(Counter,Max), var(R,C,Var,Counter,Domain) # passive
    <=> choose_val(Val,Domain), Var = Val, val(R,C,Val), first_fail(1,Max).
first_fail(Counter,Max) <=> Counter < Max | NewCounter is Counter + 1, first_fail(NewCounter,Max).
first_fail(_,_), blocksize(_) <=> true.

% The value heuristic
choose_val(X, List) :- member(X, List).

% -----------------------------------------------
%  Entry point + utility functions for the model
% -----------------------------------------------

% Solve the given Sudoku puzzle with the classic model.
%
% @param Puzzle     The input puzzle as a list of lists.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
solve(Puzzle, N, K) :-
    blocksize(K),
    register_puzzle(Puzzle, N, K),
    %input_order.
    first_fail(1,N).

% Register the pre-filled cells in the given puzzle.
%
% @param Puzzle     The input puzzle as a list of lists.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
% @note No safety checks are done.
% @note Findall won't work here as it backtracks which undoes the insertion of constraints.
% @note Probably should have been written with recursion.
register_puzzle(Puzzle, N, _K) :-
    flatten(Puzzle, FlatPuzzle),
    create_domain(N, D),
    %reverse(Domain, D), % for indomain_max
    findall(R-C-N-D, (between(1,N,R),between(1,N,C)), Cells),
    maplist(generate_domain, Cells, FlatPuzzle), % The domains for cells that aren't assigned
    maplist(assign_value, Cells, FlatPuzzle). % Pre-filled cells
generate_domain(R-C-N-Domain, V) :- var(V) -> var(R,C,V,N,Domain) ; true.
assign_value(R-C-_-_, V) :- nonvar(V) -> val(R,C,V) ; true.
