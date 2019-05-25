%
% Classic model for the Sudoku CHR solver.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- module(classic, [solve/3]).
:- use_module(library(chr)).
:- use_module(library(lists)).

:- chr_constraint generate_domain/2, generate_domain/4, setup/3, setup_row/4, value/3, variable/5, first_fail/2, blocksize/1.
%:- chr_constraint in_same_block/4, same_block/2, same_block/3, same_block/4, same_block/5.
%:- chr_constraint xwing/5

% Constraint propagation (forward checking)
value(_,C,V) \ variable(R,C,Var,_,[V,LastV]) # passive <=> value(R,C,LastV), Var = LastV.
value(_,C,V) \ variable(R,C,Var,_,[LastV,V]) # passive <=> value(R,C,LastV), Var = LastV.
value(R,_,Val) \ variable(R,C,V,L,Domain) # passive <=>
    select(Val,Domain,NewDomain) | NewL is L-1, NewL > 0, variable(R,C,V,NewL,NewDomain).
value(_,C,Val) \ variable(R,C,V,L,Domain) # passive <=>
    select(Val,Domain,NewDomain) | NewL is L-1, NewL > 0, variable(R,C,V,NewL,NewDomain).
value(R1,C1,Val), blocksize(K) # passive \ variable(R2,C2,V,L,Domain) # passive <=>
    block(K,R1,C1,B), block(K,R2,C2,B), select(Val,Domain,NewDomain)
    | NewL is L-1, NewL > 0, variable(R2,C2,V,NewL,NewDomain).
value(_,_,_) <=> true.

% First Fail heuristic
first_fail(Counter,Max), variable(R,C,Var,Counter,D) # passive
    <=> member(Val,D), value(R,C,Val), Var = Val, first_fail(1,Max).
first_fail(Counter,Max) <=> Counter < Max | NewCounter is Counter + 1, first_fail(NewCounter,Max).
first_fail(_,_), blocksize(_), generate_domain(_,_) <=> true.

% Solve the given puzzle (has to be a procedure, not a rule)
solve(P,N,K) :- generate_domain(max,[],N,N), blocksize(K), setup(dom,1,P), setup(val,1,P), first_fail(1,N).

% Bad idea, just increases the size of the constraint store and
%  it has to go through the whole list to see if there's a match.
%  It's even better to just check at the last moment if two RxC combinations
%  belong to the same block.
%same_block(N,K) <=> same_block(N,K,1).
%same_block(N,_,I) <=> I is N + 1 | true.
%same_block(N,K,B) <=> same_block(N,K,B,1), NewB is B + 1, same_block(N,K,NewB).
%same_block(N,_,_,I) <=> I is N + 1 | true.
%same_block(N,K,B,P1) <=> same_block(N,K,B,P1,1), NewP1 is P1 + 1, same_block(N,K,B,NewP1).
%same_block(N,_,_,_,I) <=> I is N + 1 | true.
%same_block(N,K,B,P1,P2) <=> P1 \= P2 | row(K,B,P1,R1), column(K,B,P1,C1), row(K,B,P2,R2), column(K,B,P2,C2), in_same_block(R1,C1,R2,C2), NewP2 is P2 + 1, same_block(N,K,B,P1,NewP2).
%same_block(N,K,B,P1,P2) <=> NewP2 is P2 + 1, same_block(N,K,B,P1,NewP2).

% Make a list from 1 to N
generate_domain(min,L,1,N) <=> generate_domain(N,[1|L]).
generate_domain(max,L,1,N) <=> reverse([1|L],D), generate_domain(N,D).
generate_domain(Heur,L,X,N) <=> NewX is X - 1, generate_domain(Heur,[X|L],NewX,N).

% Generate the domains for the input puzzle
% Reversal of domain is for indomain_max
setup(Mode,R,[Row|Rows]) <=> setup_row(Mode,R,1,Row), NewR is R + 1, setup(Mode,NewR,Rows).
setup_row(val,R,C,[X|Xs])
    <=> nonvar(X) | value(R,C,X), NewC is C + 1, setup_row(val,R,NewC,Xs).
setup_row(val,R,C,[X|Xs])
    <=> var(X) | NewC is C + 1, setup_row(val,R,NewC,Xs).
generate_domain(L,D) # passive \ setup_row(dom,R,C,[X|Xs])
    <=> var(X) | variable(R,C,X,L,D), NewC is C + 1, setup_row(dom,R,NewC,Xs).
setup_row(dom,R,C,[X|Xs])
    <=> nonvar(X) | NewC is C + 1, setup_row(dom,R,NewC,Xs).
setup_row(_,_,_,_) <=> true.
setup(_,_,_) <=> true.
