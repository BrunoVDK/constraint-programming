:- use_module(library(chr)).
:- ['../benchmarks/hashi_benchmarks'].
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


%%%%%%%%%%%%%%%%%%%%%%%%%
%	Solving a puzzle	%
%%%%%%%%%%%%%%%%%%%%%%%%%

solve(Name) :-
	(
		board(Name, Board)
	;
		makeFromPuzzle(Name, Board)
	),
	length(Board,Imax), % get board dimensions
	Board = [H|_],
	length(H,Jmax),
	size(Imax,Jmax),
	getIslands(Board, Imax, Jmax, Islands),
    Islands = [[A,B]|_],  % pick a sink, always picks the first from the list, this is the island in the lowest row, in the furthest column
	sink(A,B),
    length(Islands,Total),
	total(Total),
	solve_board(Board,Imax,Jmax),
	length(NESW,Imax), % generate NESW
	NESW = [NESWRow|_],
	length(NESWRow,Jmax),
	NESWRow = [NESWCol|_],
	length(NESWCol,4),
	form_solution(NESW,Imax,Jmax),
	nl,
	print_board(Board, NESW).

solve_board(Board,Row,Col) :-
	MaxRow is Row + 1,
	MaxCol is Col + 1,
	solve_row(Board,1,MaxRow,MaxCol).

solve_row(_,MaxRow,MaxRow,_).
solve_row([H|T],Acc,MaxRow,MaxCol) :-
	solve_column(H,Acc,1,MaxCol),
	NewAcc is Acc + 1,
	solve_row(T,NewAcc,MaxRow,MaxCol).
	
solve_column(_,_,MaxCol,MaxCol).
solve_column([H|T],Row,Acc,MaxCol) :-
	solve_cell(Row,Acc,H),
	NewAcc is Acc + 1,
	solve_column(T,Row,NewAcc,MaxCol).
	
solve_cell(I,J,Sum) :-
	cell(I,J),
	sum(I,J,Sum).
	
form_solution(NESW,Row,Col) :-
	MaxRow is Row + 1,
	MaxCol is Col + 1,
	form_row(NESW,1,MaxRow,MaxCol).

form_row(_,MaxRow,MaxRow,_).
form_row([H|T],Acc,MaxRow,MaxCol) :-
	form_column(H,Acc,1,MaxCol),
	NewAcc is Acc + 1,
	form_row(T,NewAcc,MaxRow,MaxCol).
	
form_column(_,_,MaxCol,MaxCol).
form_column([H|T],Row,Acc,MaxCol) :-
	H = [N,E,S,W],
	north(Row,Acc,N),
	east(Row,Acc,E),
	south(Row,Acc,S),
	west(Row,Acc,W),
	NewAcc is Acc + 1,
	solve_column(T,Row,NewAcc,MaxCol).
	
% For a puzzle, given as list, returns a list containing all the islands. 
% An island is represented as [X,Y], the coordinates in the board.
getIslands(Name, Islands) :-
    board(Name, Board),
	length(Board,I),
	Board = [H|_T],
	length(H,J),
	Imax is I + 1,
	Jmax is J + 1,
    getIslands(Board, Imax, Jmax, Islands).
	
getIslands(Board, Imax, Jmax, Islands) :-
    getIslands_row(Board, 1, Imax, Jmax, [], Islands).
	
getIslands_row(_,Imax,Imax,_,Islands,Islands).
getIslands_row([H|T],Acc,Imax,Jmax,L,Islands) :-
	getIslands_column(H,Acc,1,Jmax,L,Islands),
	NewAcc is Acc + 1,
	getIslands_row(T,NewAcc,Imax,Jmax,L,Islands).
	
getIslands_column(_,_,_,Jmax,Jmax,Islands,Islands).
getIslands_column([H|T],Row,Col,Jmax,L,Islands) :-
	( H > 0 ->
		append(L,[H],Lnew)
	;
		Lnew =  L
	),
	NewCol is Col + 1,
	getIslands_column(T,Row,NewCol,Jmax,Lnew,Islands).
	
	
% Get an element from a matrix
getElement(M,I,J,El) :-
	getRow(M,1,I,Row),
	getColumn(Row,1,J,El).
getRow([H|_],I,I,H).
getRow([_|T],Acc,I,Row) :-
	NewAcc is Acc + 1,
	getRow(T,NewAcc,I,Row).
getColumn([H|_],J,J,H).
getColumn([_|T],Acc,J,El) :-
	NewAcc is Acc + 1,
	getColumn(T,NewAcc,J,El).
	
% Turns a puzzle given in the format of the benchmark puzzles into an array.
makeFromPuzzle(Id, Board) :-
	puzzle(Id, Imax, P),
	length(Board,Imax),
	Board = [H|_],
	length(H,Imax),
	puzzleToMtx(P,1,Imax,1,Imax,Board).
	
puzzleToMtx(_,Imax,Imax,Imax,Imax,_).
puzzleToMtx([(X,Y,V)|T],RowAcc,Imax,ColAcc,Imax,Board) :-
	( X == RowAcc, Y == ColAcc ->
		getElement(Board,X,Y,V),
		Lnew = T
	;
		getElement(Board,X,Y,0),
		Lnew = [(X,Y,V)|T]
	),
	TempNewColAcc is ColAcc + 1,
	( NewColAcc > Imax ->
		NewColAcc = 1,
		NewRowAcc is RowAcc + 1
	;
		NewColAcc = TempNewColAcc,
		NewRowAcc = RowAcc
	),
	puzzleToMtx(Lnew,NewRowAcc,Imax,NewColAcc,Imax,Board).

% Print the solution usinf the board and the found NESW	
print_board([],_) :-
		nl.
print_board([H|T], [V|W]) :-
	print_row(H, V),
	nl,
	print_board(T,W).
	
print_row([],_).
print_row([H|T], [V|W]) :-
	( H > 0 ->
		write(H)
	;
		V = [N,E,_,_],
		( N > 0 ->
			( N > 1 ->
				write('X')
			;
				write('|')
			)
		;
			( E > 0 ->
				( E > 1 ->
					write('=')
				;
					write('-')
				)
			;
				true
			)
		)
	),
	write(' '),
	print_row(T,W).
	

%%%%%%%%%%%%%%%%%	
%	Examples	%
%%%%%%%%%%%%%%%%%

board(simple,
     []([](0,1),
        [](0,1))
    ).

board(simple2,
     []([](0,1),
        [](0,0),
        [](0,1))
    ).

board(simple3,
     []([](1,0,2),
        [](0,0,0),
        [](0,0,1))
    ).

board(simple4,
     []([](0,1),
        [](0,0),
        [](0,0),
        [](0,1))
    ).

board(simple5,
     []([](1,0,1),
        [](0,0,0))
    ).

board(simple6,
     []([](1,1),
        [](0,0))
    ).
	
board(simple7,
     []([](1,1))
    ).
	
board(simple8,
     []([](1,0,0,1))
    ).
	
board(simple9,
     []([](0,0,0),
        [](1,0,1))
    ).
	
board(simple10,
     []([](0,0,1,0,0),
		[](0,0,0,0,0),
        [](1,0,3,0,1))
    ).

board(simple11,
     []([](0,1,0),
        [](0,0,0),
        [](0,1,0))
    ).
	
board(simple12,
     []([](2,0,2),
        [](0,0,0),
        [](1,0,1))
    ).

board(simple13,
     []([](1,2),
        [](0,1))
    ).

board(stackoverflow,
     []([](4, 0, 6, 0, 0, 0, 6, 0, 3),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0),
        [](0, 1, 0, 0, 0, 0, 0, 0, 0),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0),
        [](3, 0, 0, 0, 0, 1, 0, 0, 0),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0),
        [](1, 0, 3, 0, 0, 2, 0, 0, 0),
        [](0, 3, 0, 0, 0, 0, 4, 0, 1))
    ).
board(wikipedia,
     []([](2, 0, 4, 0, 3, 0, 1, 0, 2, 0, 0, 1, 0),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 1),
        [](0, 0, 0, 0, 2, 0, 3, 0, 2, 0, 0, 0, 0),
        [](2, 0, 3, 0, 0, 2, 0, 0, 0, 3, 0, 1, 0),
        [](0, 0, 0, 0, 2, 0, 5, 0, 3, 0, 4, 0, 0),
        [](1, 0, 5, 0, 0, 2, 0, 1, 0, 0, 0, 2, 0),
        [](0, 0, 0, 0, 0, 0, 2, 0, 2, 0, 4, 0, 2),
        [](0, 0, 4, 0, 4, 0, 0, 3, 0, 0, 0, 3, 0),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        [](2, 0, 2, 0, 3, 0, 0, 0, 3, 0, 2, 0, 3),
        [](0, 0, 0, 0, 0, 2, 0, 4, 0, 4, 0, 3, 0),
        [](0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0),
        [](3, 0, 0, 0, 0, 3, 0, 1, 0, 2, 0, 0, 2))
    ).
 

