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

% 1 en 2 :

size(N) ==>

north(I,J,2) ==> south(I,J,2).
south(I,J,2) ==> north(I,J,2).

% variable heuristic :
%  choose smallest domain first is best
%  what is domain? per island keep sum and N/E/S/W and number of these unassigned ones is domain i think
% bvb.
% guess, 

% on finish : print(1,1)
% print(I,J) \ island(I,J,Sum), east(I,J,E) <=> write(Sum), write(E), NewI is I + 1, print(NewI,J).

%
% Entry point
%

solve(Name, Time) :-
    % Read puzzle


%
% Puzzles for testing purposes
%

board(simple,
     [[0,1], [0,1]]).
