:- use_module(library(chr)).
:- chr_constraint size/2, cell/2, sum/3, north/3, east/3, south/3, west/3.

% size/2:
% The size of the playing field. Contains the maximum row and column.

% cell/2:
% A cell, represented by its row and coumn.

% sum/3:
% The sum on a specific cell given by its coordinates, the sum will be greater than zero if that cell is an island.

% north/3, east/3, south/3, west/3 :
% The amount of bridges that that go in the given direction, for a specific cell, specified by its coordinates.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Basic constraints								%
% 1. bridges run horizontally or vertically			%
% 2. bridges run in one straight line				%
% 3. bridges cannot cross other bridges or islands	%
% 4. at most two bridges connect a pair of islands	%
% 5. sum constraint									%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Constraint 1 and 2
cell(I,J) ==> I == 1 | north(I,J,0).
cell(I,J) ==> size(I,_) | south(I,J,0).
cell(I,J) ==> J == 1 | west(I,J,0).
cell(I,J) ==> size(_,J) | east(I,J,0).
cell(I,J), north(I,J,N) ==> I > 1 | N >= 0, N <= 2.
cell(I,J), south(I,J,S), size(Imax,_) ==> I < Imax | S >= 0, S <= 2.
cell(I,J), east(I,J,E), size(_,Jmax) ==> J < Jmax | E >= 0, E <= 2.
cell(I,J), west(I,J,W) ==> J > 1 | W >= 0, W <= 2. 

% Constraint 5
sum(I,J, Sum), north(I,J,N), east(I,J,E), south(I,J,S), west(I,J,W) ==> Sum > 0 | sum(I,J, N + E + S + W).

% Constraint 4
sum(I,J,Sum), north(I,J,N) ==> Sum > 0 | N >= 0, N <= 2.
sum(I,J,Sum), east(I,J,E) ==> Sum > 0 | E >= 0, E <= 2.
sum(I,J,Sum), south(I,J,S) ==> Sum > 0 | S >= 0, S <= 2.
sum(I,J,Sum), west(I,J,W) ==> Sum > 0 | W >= 0, W <= 2.

% Constraint 3
sum(I,J,Sum), north(I,J,N), south(I,J,S) ==> Sum == 0 | N == S.
sum(I,J,Sum), east(I,J,E), west(I,J,W) ==> Sum == 0 | E == W. 