%
% Channeling constraints only model for the Sudoku CHR solver.
%  Bit of a nightmare, done out of curiosity. Turned out not to be such a great idea
%  to do it this way.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- module(channeling, [solve/3,register_puzzle/3]).
:- use_module(library(chr)).
:- use_module(library(lists)).

:- chr_constraint primal/3, varprimal/4, dual1/3, vardual1/4, dual2/3, vardual2/4, dual3/3, vardual3/4, first_fail/2, blocksize/1, boardsize/1, variable/3.

% ---------------------------
%        CHR rule base
% ---------------------------

% Propagate assignments
primal(R,C,V) \ vardual1(R,V,_,_) # passive <=> dual1(R,V,C).
primal(R,C,V) \ vardual2(C,V,_,_) # passive <=> dual2(C,V,R).
primal(R,C,V), blocksize(K) # passive \ vardual3(B,V,_,_) # passive <=> block(K,R,C,B), position(K,R,C,P) | dual3(B,V,P).
dual1(R,V,C) \ varprimal(R,C,_,_) # passive <=> primal(R,C,V).
dual1(R,V,C) \ vardual2(C,V,_,_) # passive <=> dual2(C,V,R).
dual1(R,V,C), blocksize(K) # passive \ vardual3(B,V,_,_) # passive <=> block(K,R,C,B), position(K,R,C,P) | dual3(B,V,P).
dual2(C,V,R) \ varprimal(R,C,_,_) # passive <=> primal(R,C,V).
dual2(C,V,R) \ vardual1(R,V,_,_) # passive <=> dual1(R,V,C).
dual2(C,V,R), blocksize(K) # passive \ vardual3(B,V,_,_) # passive <=> block(K,R,C,B), position(K,R,C,P) | dual3(B,V,P).
dual3(B,V,P), blocksize(K) # passive \ varprimal(R,C,_,_) # passive <=> row(K,B,P,R), column(K,B,P,C) | primal(R,C,V).
dual3(B,V,P), blocksize(K) # passive \ vardual1(R,V,_,_) # passive <=> row(K,B,P,R), column(K,B,P,C) | dual1(R,V,C).
dual3(B,V,P), blocksize(K) # passive \ vardual2(C,V,_,_) # passive <=> row(K,B,P,R), column(K,B,P,C) | dual2(C,V,R).

% Constraint propagation (primal)
primal(R,C,_) \ vardual1(R,V,L,D) # passive <=> select(C,D,NewD) | NewL is L-1, NewL > 0, vardual1(R,V,NewL,NewD).
primal(R,C,_) \ vardual2(C,V,L,D) # passive <=> select(R,D,NewD) | NewL is L-1, NewL > 0, vardual2(C,V,NewL,NewD).
primal(R,C,_), blocksize(K) # passive \ vardual3(B,V,L,D) # passive <=> block(K,R,C,B), position(K,R,C,P), select(P,D,NewD) | NewL is L-1, NewL > 0, vardual3(B,V,NewL,NewD).
primal(R,C,V), variable(R,C,Var) # passive <=> Var is V.
primal(_,_,_) <=> true.

% Constraint propagation (dual1)
dual1(R,V,_) \ varprimal(R,C,L,D) # passive <=> select(V,D,NewD) | NewL is L-1, NewL > 0, varprimal(R,C,NewL,NewD).
dual1(R,V,_) \ vardual2(C,V,L,D) # passive <=> select(R,D,NewD) | NewL is L-1, NewL > 0, vardual2(C,V,NewL,NewD).
dual1(R,V,C), blocksize(K) # passive, boardsize(N) # passive \ vardual3(B,V,L,D) # passive <=> between(1,N,X), block(K,R,X,B), position(K,R,C,P), select(P,D,NewD) | NewL is L-1, NewL > 0, vardual3(B,V,NewL,NewD).
dual1(R,V,C), variable(R,C,Var) # passive <=> Var is V.
dual1(_,_,_) <=> true.

% Constraint propagation (dual2)
dual2(C,V,_) \ varprimal(R,C,L,D) # passive <=> select(V,D,NewD) | NewL is L-1, NewL > 0, varprimal(R,C,NewL,NewD).
dual2(C,V,_) \ vardual1(R,V,L,D) # passive <=> select(C,D,NewD) | NewL is L-1, NewL > 0, vardual1(R,V,NewL,NewD).
dual2(C,V,R), blocksize(K) # passive, boardsize(N) # passive \ vardual3(B,V,L,D) # passive <=> between(1,N,X), block(K,X,C,B), position(K,R,C,P), select(P,D,NewD) | NewL is L-1, NewL > 0, vardual3(B,V,NewL,NewD).
dual2(C,V,R), variable(R,C,Var) # passive <=> Var is V.
dual2(_,_,_) <=> true.

% Constraint propagation (dual3)
dual3(B,V,_), blocksize(K) # passive \ varprimal(R,C,L,D) # passive <=> block(K,R,C,B), select(V,D,NewD) | NewL is L-1, NewL > 0, varprimal(R,C,NewL,NewD).
dual3(B,V,P), blocksize(K) # passive, boardsize(N) # passive \ vardual1(R,V,L,D) # passive <=> between(1,N,X), column(K,B,X,C), column(K,B,P,C), select(C,D,NewD) | NewL is L-1, NewL > 0, vardual1(R,V,NewL,NewD).
dual3(B,V,P), blocksize(K) # passive, boardsize(N) # passive \ vardual2(C,V,L,D) # passive <=> between(1,N,X), row(K,B,X,R), row(K,B,P,R), select(R,D,NewD) | NewL is L-1, NewL > 0, vardual2(C,V,NewL,NewD).
blocksize(K) # passive \ dual3(B,V,P), variable(R,C,Var) # passive <=> row(K,B,P,R), column(K,B,P,C) | Var is V.
dual3(_,_,_) <=> true.

% First-fail + indomain_min heuristic
first_fail(L,N), varprimal(R,C,L,Domain) # passive
    <=> member(V,Domain), primal(R,C,V), first_fail(1,N).
first_fail(L,N), vardual1(R,V,L,Domain) # passive
    <=> member(C,Domain), dual1(R,V,C), first_fail(1,N).
first_fail(L,N), vardual2(C,V,L,Domain) # passive
    <=> member(R,Domain), dual2(C,V,R), first_fail(1,N).
first_fail(L,N), vardual3(B,V,L,Domain) # passive
    <=> member(P,Domain), dual3(B,V,P), first_fail(1,N).
first_fail(I,N) <=> I < N | NewI is I + 1, first_fail(NewI,N).
first_fail(_,_), blocksize(_), boardsize(_) <=> true.

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
    boardsize(N),
    register_puzzle(Puzzle, N, K),
    first_fail(1,N).

% Register the pre-filled cells in the given puzzle.
%
% @param Puzzle     The input puzzle as a list of lists.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
% @note No safety checks are done.
register_puzzle(Puzzle, N, _K) :-
    flatten(Puzzle, FlatPuzzle),
    create_domain(N, D),
    % Create domains and assign pre-filled cells
    findall(R-C-N-D, (between(1,N,R),between(1,N,C)), Cells),
    maplist(generate_domain, Cells, FlatPuzzle),
    maplist(assign_value, Cells, FlatPuzzle).

% Generate the domain for the variables.
% This sets up variable constraints.
generate_domain(R-C-N-Domain, V) :-
    vardual1(R,C,N,Domain),
    vardual2(R,C,N,Domain),
    vardual3(R,C,N,Domain),
    (var(V) -> varprimal(R,C,N,Domain), variable(R,C,V) ; true).

% Assign a value to a variable.
% This sets assigned variables constraints.
assign_value(R-C-_-_, V) :-
    (nonvar(V) -> primal(R,C,V) ; true).
