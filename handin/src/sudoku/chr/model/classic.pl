%
% Classic model for the Sudoku CHR solver.
%   This one uses 4 coordinates instead of 3, making it easier to write code.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- module(classic, [solve/3]).
:- use_module(library(chr)).
:- use_module(library(lists)).

:- chr_constraint generate_domain/2, generate_domain/4, setup/4, setup_row/5, value/5, variable/7, first_fail/2.
%:- chr_constraint xwing/5

% Constraint propagation (forward checking)
value(_,B,_,D,V) \ variable(A,B,C,D,Var,_,[V,LastV]) # passive
    <=> value(A,B,C,D,LastV), Var = LastV.
value(_,B,_,D,V) \ variable(A,B,C,D,Var,_,[LastV,V]) # passive
    <=> value(A,B,C,D,LastV), Var = LastV.
value(A,_,C,_,Val) \ variable(A,B,C,D,V,L,Domain) # passive <=>
    select(Val,Domain,NewDomain)
    | NewL is L-1, NewL > 0, variable(A,B,C,D,V,NewL,NewDomain).
value(_,B,_,D,Val) \ variable(A,B,C,D,V,L,Domain) # passive <=>
    select(Val,Domain,NewDomain)
    | NewL is L-1, NewL > 0, variable(A,B,C,D,V,NewL,NewDomain).
value(A,B,_,_,Val) \ variable(A,B,C,D,V,L,Domain) # passive <=>
    select(Val,Domain,NewDomain)
    | NewL is L-1, NewL > 0, variable(A,B,C,D,V,NewL,NewDomain).
value(_,_,_,_,_) <=> true.

% First Fail heuristic
first_fail(Counter,Max), variable(A,B,C,D,Var,Counter,Dom) # passive
    <=> member(Val,Dom), value(A,B,C,D,Val), Var = Val, first_fail(1,Max).
first_fail(Counter,Max) <=> Counter < Max | NewCounter is Counter + 1, first_fail(NewCounter,Max).
first_fail(_,_), generate_domain(_,_) <=> true.

% Solve the given puzzle (has to be a procedure, not a rule)
solve(P,N,K) :- generate_domain(max,[],N,N), setup(dom,1,P,K), setup(val,1,P,K), first_fail(1,N).

% Make a list from 1 to N
generate_domain(min,L,1,N) <=> generate_domain(N,[1|L]).
generate_domain(max,L,1,N) <=> reverse([1|L],D), generate_domain(N,D).
generate_domain(Heur,L,X,N) <=> NewX is X - 1, generate_domain(Heur,[X|L],NewX,N).

% Generate the domains for the input puzzle
% Reversal of domain is for indomain_max
setup(Mode,R,[Row|Rows],K) <=> setup_row(Mode,R,1,Row,K), NewR is R + 1, setup(Mode,NewR,Rows,K).
setup_row(val,R,C,[X|Xs],K)
    <=> nonvar(X) | V is (R+2)//3, W is (C+2)//3, Y is ((R-1) mod 3)+1, Z is ((C-1) mod 3)+1, value(V,W,Y,Z,X), NewC is C + 1, setup_row(val,R,NewC,Xs,K).
setup_row(val,R,C,[X|Xs],K)
    <=> var(X) | NewC is C + 1, setup_row(val,R,NewC,Xs,K).
generate_domain(L,D) # passive \ setup_row(dom,R,C,[X|Xs],K)
    <=> var(X) | V is (R+2)//3, W is (C+2)//3, Y is ((R-1) mod 3)+1, Z is ((C-1) mod 3)+1, variable(V,W,Y,Z,X,L,D), NewC is C + 1, setup_row(dom,R,NewC,Xs,K).
setup_row(dom,R,C,[X|Xs],K)
    <=> nonvar(X) | NewC is C + 1, setup_row(dom,R,NewC,Xs,K).
setup_row(_,_,_,_,_) <=> true.
setup(_,_,_,_) <=> true.
