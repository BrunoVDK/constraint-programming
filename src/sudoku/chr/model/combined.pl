%
% Combined model for the Sudoku CHR solver.
%   This corresponds to classic + dual4 in the ECLiPSe versions.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- module(combined, [solve/3]).
:- use_module(library(chr)).
:- use_module(library(lists)).

:- chr_constraint init_domain_primal/2, init_domain_primal/4, setup/4, setup_row/5.
:- chr_constraint init_domain_dual/2, init_domain_dual/4, generate_domains/5, assign_values/6.
:- chr_constraint value/5, variable/7.
:- chr_constraint dual4/5, vardual4/5. % cell/5
:- chr_constraint first_fail/2.

%
%   PRIMAL propagation
%

value(A,B,C,D,Val) \ variable(A,B,C,D,Var,_,_) # passive <=> Var is Val.
dual4(A,B,_,_,Val) \ vardual4(A,B,Val,_,_) # passive <=> true.

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

dual4(A,B,_,_,Val) \ vardual4(A,B,Val,_,_) # passive <=> true.

%
%   DUAL propagation
%

% Constraint propagation (forward checking)
dual4(A,B,C,D,_) \ vardual4(A,B,Val,L,Domain) # passive <=> % alldiff blocks
    select(C-D,Domain,NewDomain)
    | NewL is L-1, NewL > 0, vardual4(A,B,Val,NewL,NewDomain).
dual4(A,_,C,_,Val) \ vardual4(A,B,Val,_,Domain) # passive <=>
    include(\=(C-_),Domain,NewDomain)
    | length(NewDomain,NewL), NewL > 0, vardual4(A,B,Val,NewL,NewDomain).
dual4(_,B,_,D,Val) \ vardual4(A,B,Val,_,Domain) # passive <=>
    include(\=(_-D),Domain,NewDomain)
    | length(NewDomain,NewL), NewL > 0, vardual4(A,B,Val,NewL,NewDomain).
dual4(_,_,_,_,_) <=> true.

% First Fail heuristic
first_fail(Counter,Max), variable(A,B,C,D,Var,Counter,Dom) # passive
    <=> member(Val,Dom), value(A,B,C,D,Val), dual4(A,B,C,D,Val), Var is Val, first_fail(1,Max).
first_fail(Counter,Max), vardual4(A,B,Val,Counter,Dom) # passive
    <=> member(C-D,Dom), dual4(A,B,C,D,Val), value(A,B,C,D,Val), first_fail(1,Max).
first_fail(Counter,Max) <=> Counter < Max | NewCounter is Counter + 1, first_fail(NewCounter,Max).
first_fail(_,_), init_domain_primal(_,_), init_domain_dual(_,_) <=> true.

% Solve the given puzzle (has to be a procedure, not a rule)
solve(P,N,K) :-
    flatten(P, Puzzle),
    % PRIMAL
    init_domain_primal(max,[],N,N), % Generate initial domain
    setup(dom,1,P,K), % Generate domains
    setup(val,1,P,K), % Register pre-filled cells
    % DUAL
    init_domain_dual(min,[],N,N), % Generate initial domain
    generate_domains(1,1,1,1,K), % Generate domains
    assign_values(1,1,1,1,K,Puzzle), % Register pre-filled cells
    first_fail(1,N).

% -----------
%   PRIMAL
% -----------

% Make a list from 1 to N
init_domain_primal(min,L,1,N) <=> init_domain_primal(N,[1|L]).
init_domain_primal(max,L,1,N) <=> reverse([1|L],D), init_domain_primal(N,D).
init_domain_primal(Heur,L,X,N) <=> NewX is X - 1, init_domain_primal(Heur,[X|L],NewX,N).

% Generate the domains for the input puzzle
% Reversal of domain is for indomain_max
setup(Mode,R,[Row|Rows],K) <=> setup_row(Mode,R,1,Row,K), NewR is R + 1, setup(Mode,NewR,Rows,K).
setup_row(val,R,C,[X|Xs],K)
    <=> nonvar(X) | V is (R+2)//3, W is (C+2)//3, Y is ((R-1) mod 3)+1, Z is ((C-1) mod 3)+1, value(V,W,Y,Z,X), NewC is C + 1, setup_row(val,R,NewC,Xs,K).
setup_row(val,R,C,[X|Xs],K)
    <=> var(X) | NewC is C + 1, setup_row(val,R,NewC,Xs,K).
init_domain_primal(L,D) # passive \ setup_row(dom,R,C,[X|Xs],K)
    <=> var(X) | V is (R+2)//3, W is (C+2)//3, Y is ((R-1) mod 3)+1, Z is ((C-1) mod 3)+1, variable(V,W,Y,Z,X,L,D), NewC is C + 1, setup_row(dom,R,NewC,Xs,K).
setup_row(dom,R,C,[X|Xs],K)
    <=> nonvar(X) | NewC is C + 1, setup_row(dom,R,NewC,Xs,K).
setup_row(_,_,_,_,_) <=> true.
setup(_,_,_,_) <=> true.

% --------
%   DUAL
% --------

% Make a list with positions
init_domain_dual(min,L,1,N) <=> init_domain_dual(N,[1-1|L]).
init_domain_dual(max,L,1,N) <=> reverse([1-1|L],D), init_domain_dual(N,D).
init_domain_dual(Heur,L,X,N)
    <=> NewX is X - 1,
    A is (X+2)//3, B is ((X-1) mod 3)+1,
    init_domain_dual(Heur,[A-B|L],NewX,N).

% Generate domains for all cells
generate_domains(0,_,_,_,_) <=> true.
init_domain_dual(L,Dom) # passive \ generate_domains(A,B,C,D,K) <=>
    Val is K*(C-1)+D,
    vardual4(A,B,Val,L,Dom),
    (D < K -> NewD is D + 1, NewC is C, NewB is B, NewA is A ; NewD is 1,
    (C < K -> NewC is C + 1, NewB is B, NewA is A ; NewC is 1,
    (B < K -> NewB is B + 1, NewA is A ; NewB is 1,
    (A < K -> NewA is A + 1 ; NewA is 0)))),
    generate_domains(NewA,NewB,NewC,NewD,K).

% Assign values (= register pre-filled cells)
assign_values(_,_,_,_,_,[]) <=> true.
assign_values(A,B,C,D,K,[P|Puzzle]) <=>
    (nonvar(P) -> dual4(A,C,B,D,P) ; true), % cell(A,C,B,D,P)
    (D < K -> NewD is D + 1, NewC is C, NewB is B, NewA is A ; NewD is 1,
    (C < K -> NewC is C + 1, NewB is B, NewA is A ; NewC is 1,
    (B < K -> NewB is B + 1, NewA is A ; NewB is 1,
    (A < K -> NewA is A + 1 ; NewA is 1)))),
    assign_values(NewA,NewB,NewC,NewD,K,Puzzle).
