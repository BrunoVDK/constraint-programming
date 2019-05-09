%
% Combined (classic+dual4) model for the Sudoku CHR solver.
%   dual4 means -> Block x Value => Position
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- module(combined, [solve/3]).
:- use_module(library(chr)).
:- use_module(library(lists)).

% cell/3 : Block x Position x Corresponding puzzle variable = to read solution
% val/3 : Block x Value x Position = assigned cell
% var/4 : Block x Value x Domain length x Domain = decision variable
:- chr_constraint cell/3, val/3, var/4, first_fail/2, rowpos/4, colpos/4, val_classic/3, var_classic/4, blocksize/1.

% ---------------------------
%        CHR rule base
% ---------------------------

% Constraint propagation (forward checking)
val_classic(R,C,_) \ var_classic(R,C,_,_) # passive <=> true.
val_classic(R,_,Val) \ var_classic(R,C,L,Domain) # passive <=>
    select(Val,Domain,NewDomain) | NewL is L-1, NewL > 0, var_classic(R,C,NewL,NewDomain).
val_classic(_,C,Val) \ var_classic(R,C,L,Domain) # passive <=>
    select(Val,Domain,NewDomain) | NewL is L-1, NewL > 0, var_classic(R,C,NewL,NewDomain).
val_classic(R1,C1,Val), blocksize(K) # passive \ var_classic(R2,C2,L,Domain) # passive <=>
    block(K,R1,C1,B), block(K,R2,C2,B), select(Val,Domain,NewDomain)
    | NewL is L-1, NewL > 0, var_classic(R2,C2,NewL,NewDomain).
val_classic(_,_,_) <=> true.

% Constraint propagation (forward checking)
val(B,V,_) \ var(B,V,_,_) # passive <=> true.
val(B,_,Pos) \ var(B,V,L,Domain) # passive <=>
    select(Pos,Domain,NewDomain) | NewL is L-1, NewL > 0, var(B,V,NewL,NewDomain).
val(B1,V,Pos) \ rowpos(B1,Pos,B2,Delete) # passive, var(B2,V,_,Domain) # passive
    <=> subtract(Domain,Delete,NewDomain) |
    length(NewDomain,NewL), NewL > 0, var(B2,V,NewL,NewDomain).
val(B1,V,Pos) \ colpos(B1,Pos,B2,Delete) # passive, var(B2,V,_,Domain) # passive
    <=> subtract(Domain,Delete,NewDomain) |
    length(NewDomain,NewL), NewL > 0, var(B2,V,NewL,NewDomain).
val(B,Value,P), cell(B,P,Variable) # passive <=> Variable = Value.
val(_,_,_) <=> true.

% First Fail heuristic
blocksize(K) # passive \ first_fail(Counter,Max), var(B,V,Counter,Domain) # passive
    <=> choose_val(Val,Domain), row(K,B,Val,R), column(K,B,Val,C),
        val_classic(R,C,V), val(B,V,Val), first_fail(1,Max).
blocksize(K) # passive \ first_fail(Counter,Max), var_classic(R,C,Counter,Domain) # passive
    <=> choose_val(Val,Domain), block(K,R,C,B), position(K,R,C,P),
        val_classic(R,C,Val), val(B,Val,P), first_fail(1,Max).
first_fail(Counter,Max)
    <=> Counter < Max | NewCounter is Counter + 1, first_fail(NewCounter,Max).
first_fail(_,_) \ rowpos(_,_,_,_) # passive <=> true. % Constraints that weren't propagated
first_fail(_,_) \ colpos(_,_,_,_) # passive <=> true. % Constraints that weren't propagated
first_fail(_,_), blocksize(_) <=> true. % Only remove once as there is max one of each

% The value heuristic
choose_val(Val, Domain) :- member(Val, Domain).

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
    register_puzzle_classic(Puzzle, N, K),
    register_puzzle_dual(Puzzle, N, K),
    first_fail(1,N).

% Register the pre-filled cells in the given puzzle (classic model).
%
% @param Puzzle     The input puzzle as a list of lists.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
% @note No safety checks are done.
% @note Findall won't work here as it backtracks which undoes the insertion of constraints.
% @note Probably should have been written with recursion.
register_puzzle_classic(Puzzle, N, _K) :-
    flatten(Puzzle, FlatPuzzle),
    findall(R-C-N-D, (create_domain(N,D),between(1,N,R),between(1,N,C)), Cells),
    maplist(generate_domain_classic, Cells, FlatPuzzle),
    maplist(assign_value_classic, Cells, FlatPuzzle).
generate_domain_classic(R-C-N-Domain, V) :- var(V) -> var_classic(R,C,N,Domain) ; true.
assign_value_classic(R-C-_-_, V) :- nonvar(V) -> val_classic(R,C,V) ; true.

% Register the pre-filled cells in the given puzzle (dual model).
%
% @param Puzzle     The input puzzle as a list of lists.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
% @note No safety checks are done.
register_puzzle_dual(Puzzle, N, K) :-
    findall(B-V-N-D, (create_domain(N,D),between(1,N,B),between(1,N,V)), Vars),
    maplist(generate_domain, Vars),
    findall(B-P-B2-DelRow-DelCol,
            (between(1,N,B),between(1,N,P),row(K,B,P,R),column(K,B,P,C),between(1,N,B2),B\=B2,
            row_overlap(N,K,R,B2,DelRow), col_overlap(N,K,C,B2,DelCol)),
            Combs),
    maplist(overlap, Combs),
    flatten(Puzzle, FlatPuzzle),
    findall(B-P,
            (between(1,N,R),between(1,N,C),block(K,R,C,B), position(K,R,C,P)),
            Cells),
    maplist(associate_variable, Cells, FlatPuzzle),
    maplist(assign_value, Cells, FlatPuzzle).

row_overlap(N, K, R, B, Delete) :- findall(P, (between(1,N,P),row(K,B,P,R)), Delete).
col_overlap(N, K, C, B, Delete) :- findall(P, (between(1,N,P),column(K,B,P,C)), Delete).

generate_domain(B-V-N-D) :- var(B,V,N,D).
overlap(B1-P-B2-DelRow-DelCol) :-
    (DelRow \= [] -> rowpos(B1,P,B2,DelRow) ; true),
    (DelCol \= [] -> colpos(B1,P,B2,DelCol) ; true).
assign_value(B-P, V) :- (nonvar(V) -> val(B,V,P) ; true).
associate_variable(B-P, V) :- (var(V) -> cell(B,P,V) ; true).
