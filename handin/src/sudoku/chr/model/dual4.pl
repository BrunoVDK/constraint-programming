%
% Dual4 viewpoint written with the use of 4 coordinates.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- module(dual4, [solve/3]).
:- use_module(library(chr)).
:- use_module(library(lists)).

:- chr_constraint init_domain/2, init_domain/4, generate_domains/5, assign_values/6.
:- chr_constraint first_fail/2.
:- chr_constraint dual4/5, vardual4/5, cell/5.

dual4(A,B,_,_,Val) \ vardual4(A,B,Val,_,_) # passive <=> true.

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
dual4(A,B,C,D,Val), cell(A,B,C,D,Var) # passive <=> Var is Val.
dual4(_,_,_,_,_) <=> true.

% First Fail heuristic
first_fail(Counter,Max), vardual4(A,B,Val,Counter,Dom) # passive
    <=> member(C-D,Dom), dual4(A,B,C,D,Val), first_fail(1,Max).
first_fail(Counter,Max) <=> Counter < Max | NewCounter is Counter + 1, first_fail(NewCounter,Max).
first_fail(_,_), init_domain(_,_) <=> true.

% Solve the given puzzle (has to be a procedure, not a rule)
solve(Puzzle, N, K) :-
    flatten(Puzzle, FlatPuzzle),
    init_domain(min,[],N,N),
    generate_domains(1,1,1,1,K),
    assign_values(1,1,1,1,K,FlatPuzzle),
    first_fail(1,N), !.

% Make a list with positions
init_domain(min,L,1,N) <=> init_domain(N,[1-1|L]).
init_domain(max,L,1,N) <=> reverse([1-1|L],D), init_domain(N,D).
init_domain(Heur,L,X,N)
    <=> NewX is X - 1,
        A is (X+2)//3, B is ((X-1) mod 3)+1,
        init_domain(Heur,[A-B|L],NewX,N).

% Generate domains for all cells
generate_domains(0,_,_,_,_) <=> true.
init_domain(L,Dom) # passive \ generate_domains(A,B,C,D,K) <=>
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
    (nonvar(P) -> dual4(A,C,B,D,P) ; cell(A,C,B,D,P)),
    (D < K -> NewD is D + 1, NewC is C, NewB is B, NewA is A ; NewD is 1,
    (C < K -> NewC is C + 1, NewB is B, NewA is A ; NewC is 1,
    (B < K -> NewB is B + 1, NewA is A ; NewB is 1,
    (A < K -> NewA is A + 1 ; NewA is 1)))),
    assign_values(NewA,NewB,NewC,NewD,K,Puzzle).
