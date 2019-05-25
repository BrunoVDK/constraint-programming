%
% Channeling-constraints-only model for the Sudoku CHR solver.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- module(channeling_full, [solve/3]).
:- use_module(library(chr)).
:- use_module(library(lists)).

:- chr_constraint first_fail/2. % Variable heuristics
:- chr_constraint primal/5, varprimal/7. % Primal constraints
:- chr_constraint vardual1/5, vardual2/5, vardual3/5. % Dual constraints

% --------------------------
%       CHR rule base
% --------------------------

primal(A,C,_,_,Val) \ vardual1(A,C,Val,_,_) # passive <=> true.
primal(_,_,B,D,Val) \ vardual2(B,D,Val,_,_) # passive <=> true.
primal(A,_,B,_,Val) \ vardual3(A,B,Val,_,_) # passive <=> true.

% Channeling constraints

primal(A,C,_,_,Val) \ varprimal(A,C,B,D,V,L,Domain) # passive
    <=> select(Val,Domain,NewDomain)
    | NewL is L-1, NewL > 0, varprimal(A,C,B,D,V,NewL,NewDomain).
primal(_,_,B,D,Val) \ varprimal(A,C,B,D,V,L,Domain) # passive
    <=> select(Val,Domain,NewDomain)
    | NewL is L-1, NewL > 0, varprimal(A,C,B,D,V,NewL,NewDomain).
primal(A,_,B,_,Val) \ varprimal(A,C,B,D,V,L,Domain) # passive
    <=> select(Val,Domain,NewDomain)
    | NewL is L-1, NewL > 0, varprimal(A,C,B,D,V,NewL,NewDomain).

primal(A,C,B,D,Val) \ vardual1(X,Y,Val,L,Domain) # passive
    <=> filter(dual1_guard,A,C,B,D,X,Y,Domain,NewDomain,L,NewL)
    | NewL > 0, vardual1(X,Y,Val,NewL,NewDomain).
primal(A,C,B,D,_) \ vardual1(A,C,Val,L,Domain) # passive
    <=> select(B-D,Domain,NewDomain)
    | NewL is L-1, NewL > 0, vardual1(A,C,Val,NewL,NewDomain).

primal(A,C,B,D,Val) \ vardual2(X,Y,Val,L,Domain) # passive
    <=> filter(dual2_guard,A,C,B,D,X,Y,Domain,NewDomain,L,NewL)
    | NewL > 0, vardual2(X,Y,Val,NewL,NewDomain).
primal(A,C,B,D,_) \ vardual2(B,D,Val,L,Domain) # passive
    <=> select(A-C,Domain,NewDomain)
    | NewL is L-1, NewL > 0, vardual2(B,D,Val,NewL,NewDomain).

primal(A,C,B,D,Val) \ vardual3(X,Y,Val,L,Domain) # passive
    <=> filter(dual3_guard,A,C,B,D,X,Y,Domain,NewDomain,L,NewL)
    | NewL > 0, vardual3(X,Y,Val,NewL,NewDomain).
primal(A,C,B,D,_) \ vardual3(A,B,Val,L,Domain) # passive
    <=> select(C-D,Domain,NewDomain)
    | NewL is L-1, NewL > 0, vardual3(A,B,Val,NewL,NewDomain).

primal(A,C,B,D,Val) \ varprimal(A,C,B,D,Var,_,_) # passive
    <=> Var is Val.

filter(Guard, A, C, B, D, X, Y, Domain, NewDomain, OldLength, NewLength) :-
    filter(Guard, A, C, B, D, X, Y, Domain, [], NewDomain, 0, NewLength),
    NewLength \= OldLength.
filter(_,_, _, _, _, _, _, [], FilteredList, FilteredList, NewLength, NewLength).
filter(Guard, A, C, B, D, X, Y, [Z-W|Rest], Acc, FilteredList, AccLength, Length) :-
    (call(Guard, A-C-B-D, X-Y-Z-W) ->
        filter(Guard, A, C, B, D, X, Y, Rest, Acc, FilteredList, AccLength, Length)
    ;
        NewLength is AccLength + 1,
        filter(Guard, A, C, B, D, X, Y, Rest, [Z-W|Acc], FilteredList, NewLength, Length)
    ).

dual1_guard(A-C-B-D,X-Y-Z-W) :- (A == X, C == Y) ; (B == Z, D == W) ; (A == X, B == Z).
dual2_guard(A-C-B-D,X-Y-Z-W) :- (A == Z, C == W) ; (B == X, D == Y) ; (A == Z, B == X).
dual3_guard(A-C-B-D,X-Y-Z-W) :- (A == X, C == Z) ; (B == Y, D == W) ; (A == X, B == Y).

primal(_,_,_,_,_) <=> true.

% Only one value left in one of the viewpoints, assign the corresponding cell
first_fail(1,N), varprimal(A,C,B,D,Var,1,[Val]) # passive
    <=> primal(A,C,B,D,Val), Var = Val, first_fail(1,N).
first_fail(1,N), vardual1(A,C,V,1,[B-D]) # passive
    <=> primal(A,C,B,D,V), first_fail(1,N).
first_fail(1,N), vardual2(B,D,V,1,[A-C]) # passive
    <=> primal(A,C,B,D,V), first_fail(1,N).
first_fail(1,N), vardual3(A,B,V,1,[C-D]) # passive
    <=> primal(A,C,B,D,V), first_fail(1,N).

% First Fail heuristic
first_fail(Counter,Max), varprimal(A,C,B,D,Var,Counter,Dom) # passive
    <=> member(Val,Dom), primal(A,C,B,D,Val), Var = Val, first_fail(1,Max).
first_fail(Counter,Max)
    <=> Counter < Max | NewCounter is Counter + 1, first_fail(NewCounter,Max).
first_fail(_,_) \ vardual1(_,_,_,_,_) <=> true.
first_fail(_,_) \ vardual2(_,_,_,_,_) <=> true.
first_fail(_,_) \ vardual3(_,_,_,_,_) <=> true.
first_fail(_,_) <=> true.

% ---------------------------
% Register input puzzle
%   Not done in CHR because it would be more cluttered. In the other viewpoints it's done
%   in CHR.
% ---------------------------

% Solve the given puzzle (has to be a procedure, not a rule)
solve(P, N, K) :-
    register_puzzle(P, N, K),
    first_fail(1,N), !.

register_puzzle(Puzzle,N,K) :-
    flatten(Puzzle, FlatPuzzle),
    create_domain(N, Dom),
    reverse(Dom, D),
    create_domain_4coord(K, D4c),
    findall(X-Y-Z-W-N-D, (between(1,K,X),between(1,K,Y),between(1,K,Z),between(1,K,W)), Cells),
    maplist(generate_primal_domains, Cells, FlatPuzzle),
    findall(X-Y-V-N-D4c, (between(1,K,X),between(1,K,Y),between(1,N,V)), DualCells),
    maplist(generate_dual_domains, DualCells),
    maplist(assign_values, Cells, FlatPuzzle).

generate_primal_domains(A-B-C-D-N-Dom, Var) :-
    (var(Var) -> varprimal(A,B,C,D,Var,N,Dom) ; true).

generate_dual_domains(X-Y-V-N-D4c) :-
    vardual1(X,Y,V,N,D4c),
    vardual2(X,Y,V,N,D4c),
    vardual3(X,Y,V,N,D4c).

assign_values(A-B-C-D-_-_, Val) :-
    (nonvar(Val) -> primal(A,B,C,D,Val) ; true).
