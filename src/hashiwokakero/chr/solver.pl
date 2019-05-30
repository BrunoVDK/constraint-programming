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
:- chr_constraint solve_puzzle/3, read_puzzle/6, initialized/0.
:- chr_constraint print_board/1, print_cell/3, symbol/2, in/2.
:- chr_constraint search/0, assign/2, island/7, cell/4, sum/3.
:- chr_constraint check_connectedness/0, no_sink/0, sink/2, connects/5, connected/4.

% Additional constraint : when two neighbouring islands have the number 1 or 2,
%  they cannot be by that number of bridges.
no_sink, island(_,C,Var,_,_,_,1) # passive, island(_,C,_,_,Var,_,1) # passive ==>
    var(Var) | Var in [0].
no_sink, island(R,_,_,Var,_,_,1) # passive, island(R,_,_,_,_,Var,1) # passive ==>
    var(Var) | Var in [0].
no_sink, island(_,C,Var,_,_,_,2) # passive, island(_,C,_,_,Var,_,2) # passive ==>
    var(Var) | Var in [0,1].
no_sink, island(R,_,_,Var,_,_,2) # passive, island(R,_,_,_,_,Var,2) # passive ==>
    var(Var) | Var in [0,1].

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

% Connectedness constraint (passive version)
island(R,C,_,_,_,_,_) # passive \ no_sink <=> sink(R,C).
assign(Val,X) \ connects(X,R1,C1,R2,C2) # passive <=> Val > 0 | connected(R1,C1,R2,C2).
assign(0,X) \ connects(X,_,_,_,_) # passive <=> true.
check_connectedness, sink(R,C) # passive,
    connected(R1,C1,R,C) \
    connected(R1,C1,R2,C2) # passive <=>
    (R1 \== R ; C1 \== C) | connected(R2,C2,R,C).
check_connectedness, sink(R,C) # passive,
    connected(R1,C1,R,C) \
    connected(R2,C2,R1,C1) # passive <=>
    (R1 \== R ; C1 \== C) | connected(R2,C2,R,C).
check_connectedness, sink(R,C) # passive \ connected(_,_,R,C) # passive <=> true.
check_connectedness, connected(_,_,_,_) # passive ==> fail.
check_connectedness \ connects(_,_,_,_,_) # passive <=> true.
check_connectedness, sink(_,_) # passive <=> true.

% Search procedure
%   Implementing first-fail is easy because domains are very small.
%   We tried it, didn't improve speed.
assign(Val,X) <=> X is Val.
%search, (X in [Val]) # passive <=> assign(Val,X), search.
%search, (X in [Val1,Val2]) # passive <=> member(Val,[Val1,Val2]), assign(Val,X), search.
search, (X in Dom) # passive <=> member(Val,NewDom), assign(Val,X), search.
search <=> check_connectedness.

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
initialized <=> true.

% Clean up pre-filled vars
0 in _ <=> true.

% Read the input puzzle
solve_puzzle(Puzzle,Size,Verbose) <=>
    findall(0-0-0, between(1,Size,_), PrecedingSouths),
    read_puzzle(Puzzle,1,1,Size,PrecedingSouths,0-0-0),
    (Verbose -> print_board(Size) ; true).
read_puzzle(_,R,_,Size,_,_) <=> R > Size | no_sink, initialized, search.
read_puzzle(P,R,C,Size,[NR-NC-N|PrevSouths],WR-WC-W) <=>
    (member((R,C,Sum), P) ->
        island(R,C,N,E,S,W,Sum),
        (var(N) -> connects(N,NR,NC,R,C) ; true),
        (var(W) -> connects(W,WR,WC,R,C) ; true),
        sum(4,[N,E,S,W],Sum),
        E in [0,1,2], S in [0,1,2],
        NewNR = R, NewNC = C, NewWR = R, NewWC = C
    ;
        cell(R,C,E,S),
        E = W, S = N,
        NewNR = NR, NewNC = NC, NewWR = WR, NewWC = WC
    ),
    (C is Size -> E is 0, NewR is R + 1, NewC is 1 ; NewC is C + 1, NewR is R),
    (R is Size -> S is 0 ; true),
    append(PrevSouths, [NewNR-NewNC-S], NewPrevSouths),
    read_puzzle(P,NewR,NewC,Size,NewPrevSouths,NewWR-NewWC-E).

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

