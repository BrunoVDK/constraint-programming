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
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- use_module(library(chr)).
:- ['../benchmarks/hashi_benchmarks'].

:- op(700,xfx,'in').
:- chr_constraint solve_puzzle/3, read_puzzle/6.
:- chr_constraint print_board/1, print_cell/3, symbol/2.
:- chr_constraint initialized/0, search/0, island/7, cell/4.
:- chr_constraint in/2, assign/2.
:- chr_constraint sum/3.

% Sum constraint
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

% Search procedure (first-fail)
assign(Val,X) <=> X is Val.
search, (X in Dom) # passive <=> member(Val,Dom), assign(Val,X), search.
search <=> true.

% Print the solution
% Assumes fixed-width font
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

% Clean up sums (remove zeroes) after initialization
initialized \ sum(_,Vars,Sum) # passive <=>
    member(0,Vars)
    | include(\==(0), Vars, NewVars), length(NewVars, L), sum(L,NewVars,Sum).
initialized <=> true.

% Clean up pre-filled vars
0 in _ <=> true.

% Read the input puzzle
solve_puzzle(P,Size,Verbose) <=>
    findall(0, between(1,Size,_), PrecedingSouths),
    read_puzzle(P,1,1,Size,PrecedingSouths,0),
    (Verbose -> print_board(Size) ; true).
read_puzzle(_,R,_,Size,_,_) <=> R > Size | initialized, search.
read_puzzle(Puzzle,R,C,Size,[N|PreviousSouths],W) <=>
    (member((R,C,Sum), Puzzle) ->
        island(R,C,N,E,S,W,Sum),
        sum(4,[N,E,S,W],Sum),
        E in [0,1,2],
        S in [0,1,2]
    ;
        cell(R,C,E,S),
        E = W,
        S = N
    ),
    (C is Size -> E is 0, NewR is R + 1, NewC is 1 ; NewC is C + 1, NewR is R),
    (R is Size -> S is 0 ; true),
    append(PreviousSouths, [S], NewPreviousSouths),
    read_puzzle(Puzzle,NewR,NewC,Size,NewPreviousSouths,E).

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
    solve_puzzle(Puzzle,N,Verbose), !,
    statistics(walltime, [_|[Time]]),
    (Verbose -> write('Time : '), write(Time), nl ; true),
    (Verbose -> print_solution : true).
