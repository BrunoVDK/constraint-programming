%
% Hashiwokakero CHR solver.
%
% Imposed constraints
%   1. bridges run horizontally or vertically
%   2. bridges run in one straight line
%   3. bridges cannot cross other bridges or islands
%   4. at most two bridges connect a pair of islands
%   5. sum constraint
%   6. connectedness constraint
%
% They're all active. In the case of 1 and 2 it is done by
%   the sharing of variables between cells and islands.
%   (e.g. any cell's 'north' variable is the same as the 'south' variable
%       of the cell above it)
% Constraint 4 is enforced by defining the domains to be 0..2
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- use_module(library(chr)).
:- ['../benchmarks/hashi_benchmarks'].

% Domain declarations
:- op(700,xfx,'in').
:- op(700,xfx,'inflow').
:- op(600,xfx,'..').

% CHR constraints
:- chr_constraint solve_puzzle/3, read_puzzle/10, initialized/0.
:- chr_constraint print_board/1, print_cell/3, symbol/2.
:- chr_constraint search/0, assign/2, island/7, cell/4, sum/3.
:- chr_constraint flow_sum/3, assign_flow/2, bridge_flow/2.
:- chr_constraint in/2, inflow/2.

% Sum constraint (does forward checking)
sum(4,[A,B,C,D],8) <=> A in [2], B in [2], C in [2], D in [2].
sum(3,[A,B,C],6) <=> A in [2], B in [2], C in [2].
sum(2,[A,B],4) <=> A in [2], B in [2].
sum(1,[X],Val) <=> X in [Val].
assign(Val,X) \ sum(N,Vars,Sum) # passive <=>
    N > 1, select(X, Vars, NewVars)
    | NewN is N - 1, NewSum is Sum - Val, NewSum >= 0, sum(NewN,NewVars,NewSum).

% No bridges crossing constraint
assign(Val,X), cell(_,_,X,Y) # passive ==> Val > 0 | Y in [0].
assign(Val,X), cell(_,_,Y,X) # passive ==> Val > 0 | Y in [0].

% Domain updates
_ in [] <=> fail.
X in L1, X in L2 <=> intersection(L1,L2,L3) | X in L3.
X in L <=> nonvar(X) | member(X,L).

% Flow constraints
assign_flow(Val,FX), bridge_flow(X,FX), X in [0|Dom] ==> Val \= 0 | X in Dom.
assign(0,X) \ bridge_flow(X,FX) # passive <=> FX inflow 0..0.
assign(_,X) \ bridge_flow(X,_) # passive <=> true.
flow_sum(1,[S-FX],Sum) <=> (S == 1 -> Val is -Sum ; Val is Sum), FX inflow Val..Val.
assign_flow(Val,FX) \ flow_sum(N,Vars,Sum) # passive <=>
    N > 1, select(S-FX, Vars, NewVars)
    | NewN is N - 1,
    (S == 1 -> NewSum is Sum + Val ; NewSum is Sum - Val),
    flow_sum(NewN,NewVars,NewSum).

% Flow domain updates
(FX inflow L1..U1), (FX inflow L2..U2) <=>
    Min is max(L1,L2), Max is min(U1,U2), Min =< Max, (FX inflow Min..Max).
%print_board(_) \ bridge_flow(_,_) <=> true. % Clean up constraints
0 inflow _ <=> true.
assign_flow(Val,FX) <=> FX is Val.

% Search procedure
%   Implementing first-fail is easy because domains are very small.
%   We tried it, didn't improve speed.
assign(Val,X) <=> X is Val.
search, (X in [Val]) # passive <=> assign(Val,X), search.
% If you comment out the following line the first solution isn't connected
%  (it makes it clear that the constraints are active)
search, (FX inflow Val..Val) # passive <=> assign_flow(Val,FX), search.
search, (X in Dom) # passive <=> member(Val,Dom), assign(Val,X), search.

% Flow domain bound consistency (attempt, short on time)
% Puzzle 7 is interesting to test out with
(FA inflow MinA..MaxA), (FB inflow MinB..MaxB), flow_sum(2,[SA-FA,SB-FB],Sum) ==>
    E1B is Sum - MinA + (2*SA*MinA), E2B is Sum - MaxA + (2*SA*MaxA),
    E1A is Sum - MinB + (2*SB*MinB), E2A is Sum - MaxB + (2*SB*MaxB),
    (SA == 1 -> NewE1A is -E1A, NewE2A is -E2A ; NewE1A is E1A, NewE2A is E2A),
    (SB == 1 -> NewE1B is -E1B, NewE2B is -E2B ; NewE1B is E1B, NewE2B is E2B),
    NewMinA is min(NewE1A,NewE2A), NewMaxA is max(NewE1A,NewE2A),
    NewMinB is min(NewE1B,NewE2B), NewMaxB is max(NewE1B,NewE2B),
    LA is max(MinA,NewMinA), UA is min(MaxA,NewMaxA), LA =< UA,
    LB is max(MinB,NewMinB), UB is min(MaxB,NewMaxB), LB =< UB,
    ((LA \= MinA ; UA \= MaxA) -> FA inflow LA..UA ; true),
    ((LB \= MinB ; UB \= MaxB) -> FB inflow LB..UB ; true).
search, (FX inflow Min..Max) # passive <=>
    between(Min,Max,Val), assign_flow(Val,FX), search.
search <=> true.

% Print the solution
% Assumes fixed-width font (change in Settings > Font ...)
print_board(Size) <=>
    print_cell(1,1,Size).
print_cell(R,_,Size) <=> R > Size | true.
print_cell(R,C,_) \ island(R,C,_,_,_,_,Sum) # passive <=> write(Sum).
print_cell(R,C,_) \ cell(R,C,H,V) # passive <=> symbol(V,H).
print_cell(R,Size,Size) <=> nl, NewR is R + 1, print_cell(NewR,1,Size).
print_cell(R,C,Size) <=> write(' '), NewC is C + 1, print_cell(R,NewC,Size).

% Symbols to be used when printing the board
symbol(0,0) <=> write(' ').
symbol(1,1) <=> write('+').
symbol(0,1) <=> write('-').
symbol(0,2) <=> write('=').
symbol(1,0) <=> write('|').
symbol(2,0) <=> write('X').

% Clean up after initialization
initialized \ sum(_,Vars,Sum) # passive <=>
    member(0,Vars)
    | include(\==(0),Vars,NewVars), length(NewVars,L), sum(L,NewVars,Sum).
initialized \ flow_sum(_,Vars,Sum) # passive <=>
    member(_-0,Vars)
    | include(nontrivial,Vars,NewVars), length(NewVars,L), flow_sum(L,NewVars,Sum).
nontrivial(_-X) :- X \== 0.
initialized \ bridge_flow(0,0) <=> true.
initialized <=> true.

% Clean up pre-filled vars
0 in _ <=> true.

% Read the input puzzle
solve_puzzle(Puzzle,Size,Verbose) <=>
    findall(0, between(1,Size,_), PrecedingSouths),
    findall(0-0, between(1,Size,_), PrecedingFlowSouths),
    length(Puzzle, NbIslands),
    Max is NbIslands - 1,
    read_puzzle(Puzzle,Max,1,1,Size,PrecedingSouths,0,PrecedingFlowSouths,0-0,true),
    (Verbose -> print_board(Size) ; true).
read_puzzle(_,_,R,_,Size,_,_,_,_,_) <=> R > Size | initialized, search.
read_puzzle(P,Max,R,C,Size,[N|PrevSouths],W,[NegN-FN|PrevFlowSouths],NegW-FW,Sink) <=>
    NN is 1 - NegN, NW is 1 - NegW,
    (member((R,C,Sum), P) ->
        island(R,C,N,E,S,W,Sum),
        (Sink ->
            flow_sum(4,[NN-FN,0-FE,0-FS,NW-FW],Max)
        ;
            flow_sum(4,[NN-FN,0-FE,0-FS,NW-FW],-1)
        ),
        sum(4,[N,E,S,W],Sum),
        E in [0,1,2], S in [0,1,2],
        Min is -Max,
        FE inflow (Min..Max), FS inflow (Min..Max),
        bridge_flow(E,FE), bridge_flow(S,FS),
        NewSink = false
    ;
        cell(R,C,E,S),
        E = W, S = N,
        FE = FW, FS = FN,
        NewSink = Sink
    ),
    (C is Size -> E is 0, FE is 0, NewR is R + 1, NewC is 1 ; NewC is C + 1, NewR is R),
    (R is Size -> S is 0, FS is 0 ; true),
    append(PrevSouths, [S], NewPrevSouths),
    append(PrevFlowSouths, [0-FS], NewPrevFlowSouths),
    read_puzzle(P,Max,NewR,NewC,Size,NewPrevSouths,E,NewPrevFlowSouths,0-FE,NewSink).

%
% Entry point
%

% Run tests on the provided benchmarks.
%
% @param Verbose Flag denoting whether or not intermediate results should be displayed.
benchmark(Verbose) :-
    findall(Time, solve(_,Time,Verbose), Times),
    sumlist(Times, TotalTime),
    write('Total time : '), write(TotalTime), nl.

% Solve a hashiwokakero puzzle.
%
% @param Identifier The identifier of the puzzle.
% @param Time       The time it took to solve the puzzle.
% @param Verbose    Flag denoting whether or not intermediate results should be displayed.
solve(Identifier, Time, Verbose) :-
    puzzle(Identifier, N, Puzzle),
    (Verbose -> write('Puzzle name : '), write(Identifier), nl ; true),
    statistics(walltime, [_|[_]]),
    once(solve_puzzle(Puzzle,N,Verbose)),
    statistics(walltime, [_|[Time]]),
    (Verbose -> write('Time : '), write(Time), nl ; true).

