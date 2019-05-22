%
% Channeling constraints only model for the Sudoku CHR solver.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- module(channeling, [solve/3,register_puzzle/3]).
:- use_module(library(chr)).
:- use_module(library(lists)).

:- chr_constraint primal/3, varprimal/5, vardual1/4, vardual2/4, vardual3/4, first_fail/2, variable/3, blocksize/1, cleanup/0.

% ---------------------------
%        CHR rule base
% ---------------------------

% Primal is assigned, assign duals
primal(R,_,V) \ vardual1(R,V,_,_) # passive <=> true.
primal(_,C,V) \ vardual2(C,V,_,_) # passive <=> true.
primal(R,C,V), blocksize(K) # passive \ vardual3(B,V,_,_) # passive <=> block(K,R,C,B) | true.

% Dual is assigned, assign primal
vardual1(R,V,1,[C]), varprimal(R,C,Var,_,_) # passive <=> primal(R,C,V), Var is V.
vardual2(C,V,1,[R]), varprimal(R,C,Var,_,_) # passive <=> primal(R,C,V), Var is V.
blocksize(K) # passive \ vardual3(B,V,1,[P]), varprimal(R,C,Var,_,_) # passive <=> row(K,B,P,R), column(K,B,P,C) | primal(R,C,V), Var is V.

% Reification logic
primal(R,_,V) \ varprimal(R,C,Var,L,D) # passive <=> select(V,D,NewD) | NewL is L-1, NewL > 0, varprimal(R,C,Var,NewL,NewD).
primal(_,C,V) \ varprimal(R,C,Var,L,D) # passive <=> select(V,D,NewD) | NewL is L-1, NewL > 0, varprimal(R,C,Var,NewL,NewD).
primal(R,C,V), blocksize(K) # passive \ varprimal(R2,C2,Var,L,D) # passive <=> block(K,R,C,B), block(K,R2,C2,B), select(V,D,NewD) | NewL is L-1, NewL > 0, varprimal(R2,C2,Var,NewL,NewD).
primal(R,C,_) \ vardual1(R,V,L,D) # passive <=> select(C,D,NewD) | NewL is L-1, NewL > 0, vardual1(R,V,NewL,NewD).
primal(R,C,_) \ vardual2(C,V,L,D) # passive <=> select(R,D,NewD) | NewL is L-1, NewL > 0, vardual2(C,V,NewL,NewD).
primal(R,C,_), blocksize(K) # passive \ vardual3(B,V,L,D) # passive <=> block(K,R,C,B), position(K,R,C,P), select(P,D,NewD) | NewL is L-1, NewL > 0, vardual3(B,V,NewL,NewD).
primal(_,_,_) <=> true.

% First-fail + indomain_min heuristic
first_fail(L,N), varprimal(R,C,Var,L,Domain) # passive
    <=> member(V,Domain), primal(R,C,V), Var is V, first_fail(1,N).
first_fail(I,N) <=> I < N | NewI is I + 1, first_fail(NewI,N).
first_fail(_,_), blocksize(_) <=> true.

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
    (var(V) -> varprimal(R,C,V,N,Domain) ; true).
% We used a constraint called pos/4 for conversion from RxC -> BxP
%  But this just made the constraint store larger and it is apparently a problem
%  for performance.
% Fruhwirth basically addressed this by using a 4-coordinate approach.
%  This is probably the way to go.

% Assign a value to a variable.
% This sets assigned variables constraints.
assign_value(R-C-_-_, V) :-
    (nonvar(V) -> primal(R,C,V) ; true).
