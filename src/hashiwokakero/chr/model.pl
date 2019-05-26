:- use_module(library(chr)).
:- chr_constraint size/2, cell/2, sum/3, north/3, east/3, south/3, west/3, fnorth/3, feast/3, fsouth/3, fwest/3, sink/2, total/1.

% size/2:
% The size of the playing field. Contains the maximum row and column.

% cell/2:
% A cell, represented by its row and coumn.

% sum/3:
% The sum on a specific cell given by its coordinates, the sum will be greater than zero if that cell is an island.

% north/3, east/3, south/3, west/3 :
% The amount of bridges that that go in the given direction, for a specific cell, specified by its coordinates.

% fnorth/3, feast/3, fsouth/3, fwest/3 :
% The amount of flow that that go in the given direction, for a specific cell, specified by its coordinates.

% sink/2 :
% The sink, represented by its row and column.

% total/1 :
% The total amount of islands in the game.

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
cell(I,J), north(I,J,N) ==> I > 1 | N >= 0, N =< 2.
cell(I,J), south(I,J,S), size(Imax,_) ==> I < Imax | S >= 0, S =< 2.
cell(I,J), east(I,J,E), size(_,Jmax) ==> J < Jmax | E >= 0, E =< 2.
cell(I,J), west(I,J,W) ==> J > 1 | W >= 0, W =< 2. 

% Constraint 5
sum(I,J, Sum), north(I,J,N), east(I,J,E), south(I,J,S), west(I,J,W) ==> Sum > 0 | sum(I,J, N + E + S + W).

% Constraint 4
sum(I,J,Sum), north(I,J,N) ==> Sum > 0 | N >= 0, N =< 2.
sum(I,J,Sum), east(I,J,E) ==> Sum > 0 | E >= 0, E =< 2.
sum(I,J,Sum), south(I,J,S) ==> Sum > 0 | S >= 0, S =< 2.
sum(I,J,Sum), west(I,J,W) ==> Sum > 0 | W >= 0, W =< 2.

% Constraint 3
sum(I,J,Sum), north(I,J,N), south(I,J,S) ==> Sum == 0 | N == S.
sum(I,J,Sum), east(I,J,E), west(I,J,W) ==> Sum == 0 | E == W. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Connectedness constraints																						%
% 1. A cell that isn't an island, neither a bridge, has no flow.													%
% 2. The net flow in a bridge is 0.																					%
% 3. If a cell has a flow n in the direction of a neighbor, that neighbor has a flow -n in the opposite direction.	%
% 4. Every non-sink island has a net flow is +1.																	%
% 5. The net flow arriving at the sink island is equal to the amount of islands in the puzzle, minus 1.				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Constraint 1
north(I,J,0) ==> fnorth(I,J,0).
east(I,J,0) ==> feast(I,J,0).
south(I,J,0) ==> fsouth(I,J,0).
west(I,J,0) ==> fwest(I,J,0).

% Constraint 2
sum(I,J,0), fnorth(I,J,N), fsouth(I,J,S) ==> N == S.
sum(I,J,0), feast(I,J,E), fwest(I,J,W) ==> E == W.

% Constraint 3
cell(I,J) ==> I == 1 | fnorth(I,J,0).
cell(I,J) ==> size(I,_) | fsouth(I,J,0).
cell(I,J) ==> J == 1 | fwest(I,J,0).
cell(I,J) ==> size(_,J) | feast(I,J,0).
cell(I,J), fnorth(I,J,N) ==> I > 1 | fsouth(I-1,J,N).
cell(I,J), fsouth(I,J,S), size(Imax,_) ==> I < Imax | fnorth(I+1,J,S).
cell(I,J), feast(I,J,E), size(_,Jmax) ==> J < Jmax | fwest(I,J+1,E).
cell(I,J), fwest(I,J,W) ==> J > 1 | feast(I-1,J,W). 

% Constraint 4

cell(I,J), sum(I,J,Sum), fnorth(I,J,N), feast(I,J,E), fsouth(I,J,S), fwest(I,J,W) ==> Sum > 0, not(sink(I,J)) | N + E + S + W == 1.

% Constraint 5

cell(I,J), sum(I,J,Sum), fnorth(I,J,N), feast(I,J,E), fsouth(I,J,S), fwest(I,J,W), total(T) ==> Sum > 0, sink(I,J) | N + E + S + W == T - 1.
 

