%
% Hashiwokakero CHR solver.
%   Based on partial solution by Stack Overflow user "jschimpf"
%   https://stackoverflow.com/questions/20337029/
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

:- lib(ic).  % uses the integer constraint library
:- compile('../benchmarks/hashi_benchmarks.pl').

test(name) :-
	board(name, Board),
	hashi(Board, _T, _B, _NESW).

% Solve the given hashiwokakero puzzle.
%
% @param Board      The board to solve.
% @param Time       The time it took to solve the puzzle.
% @param Backtracks The number of backtracks done when solving the puzzle.
% @param NESW       The representation of the solution (to be used for printing purposes).
hashi(Board, Time, Backtracks, NESW) :-
    % --- Define variables
    dim(Board, [Imax,Jmax]),
    dim(NESW, [Imax,Jmax,4]),   % 4 variables N,E,S,W for each field
    dim(FlowNESW, [Imax,Jmax,4]), % 4 variables FN,FE,FS,FW for each field
    % --- Get all islands, define sink ([A,B] is the sink, it's the top left island)
    findall([X,Y], (between(1,Imax,1,X),between(1,Jmax,1,Y),0<Board[X,Y]), Islands),
    Islands = [[A,B]|_],
    length(Islands, NumberOfIslands),
    % --- Generate constraints
    (foreachindex([I,J], Board),
     param(Board, NESW, FlowNESW, Imax, Jmax, A, B, NumberOfIslands) do
        % --- First 5 constraints
        Sum is Board[I,J],
        N is NESW[I,J,1], E is NESW[I,J,2], S is NESW[I,J,3], W is NESW[I,J,4],
        FN is FlowNESW[I,J,1], FE is FlowNESW[I,J,2], FS is FlowNESW[I,J,3], FW is FlowNESW[I,J,4],
        % Constraints 1 and 2:
        %   The combination of N=S etc on tiles without island
        %   and f.i. north on current tile must equal south on
        %   other northern tile, express that bridges can only run
        %   in a straight line, horizontally or vertically.
        %   They also express another constraint that was not stated
        %   explicitly: that bridges must be on both sides connected
        %   to islands.
        (I == 1 -> N = 0 ; true),
        (J == 1 -> W = 0 ; true),
        (I < Imax -> S #= NESW[I+1,J,1] ; S = 0 ),
        (J < Jmax -> E #= NESW[I,J+1,4] ; E = 0 ),
        %(I > 1    -> N #= NESW[I-1,J,3] ; N = 0 ),
        %(I < Imax -> S #= NESW[I+1,J,1] ; S = 0 ),
        %(J < Jmax -> E #= NESW[I,J+1,4] ; E = 0 ),
        %(J > 1    -> W #= NESW[I,J-1,2] ; W = 0 ),
        (Sum > 0 -> % Cell is an island
          [N,E,S,W] #:: 0..2, % Constraint 4
          N+E+S+W #= Sum % Constraint 5
        ; % Bridge or empty cell
          N = S, E = W,
          (N #= 0) or (E #= 0) % Constraint 3
        ),
        % --- Flow constraints
        % Define the domain of the variables
        [FN,FE,FS,FW] #:: -(NumberOfIslands-1)..(NumberOfIslands-1),
        % Flow constraint 1:
        %  If a cell does not have a bridge in a certain direction,
        %  it does not have flow in that direction.
        N #= 0 => FN #= 0,
        E #= 0 => FE #= 0,
        S #= 0 => FS #= 0,
        W #= 0 => FW #= 0,
        % Flow constraint 3:
        %  If a cell has a flow n in the direction of neighbor,
        %  that neighbor has a flow -n in the opposite direction.
        (I > 1      -> FN #= -(FlowNESW[I-1,J,3]) ; FN = 0),
        (I < Imax   -> FS #= -(FlowNESW[I+1,J,1]) ; FS = 0),
        (J > 1	    -> FW #= -(FlowNESW[I,J-1,2]) ; FW = 0),
        (J < Jmax   -> FE #= -(FlowNESW[I,J+1,4]) ; FE = 0),
        (Sum > 0 -> % Island
            ([I,J] = [A,B] ->
                % Flow constraint 5:
                %  The net flow arriving at the sink island is equal to
                %  the amount of islands in the puzzle, minus 1.
                FN + FE + FS + FW #= -(NumberOfIslands-1)
            ;
                % Flow constraint 4:
                %  Every non-sink island has a net flow is +1.
                FN + FE + FS + FW #= 1
				% Implied Constraint
				%	A non-sink island with only 1 bridge, has a flow of only 1
                %    in the direction of that bridge.
				%(Sum == 1 ->
				%	N #= 1 => FN #= 1, E #= 1 => FE #= 1, S #= 1 => FS #= 1, W #= 1 => FW #= 1
				%;
				%	true
				%),
				% Implied Constraint
				%	A non-sink island with only 2 bridges (in the same direction),
                %   will only have flow 1 in that direction.
				%(Sum == 2 ->
				%	N #= 2 => FN #= 1,
				%	E #= 2 => FE #= 1,
				%	S #= 2 => FS #= 1,
				%	W #= 2 => FW #= 1
				%;
				%	true
				%)
            )
        ; % Bridge or empty cell
            % Flow constraint 2:
            %  The net flow in a bridge is 0.
            %  (this constraint is also valid for empty cells)
            N + E + S + W #\= 0 => FN #= -(FS) and FE #= -(FW) and FN + FE + FS + FW #= 0
        )
		
    ),
    % --- Search procedure
    statistics(hr_time, Start),
    collection_to_list(NESW, Vars1),
    collection_to_list(FlowNESW, Vars2),
    append(Vars1, Vars2, Vars),
    search(Vars, 0, input_order, indomain_min, complete, [backtrack(Backtracks)]),
    statistics(hr_time, End),
    Time is End - Start.

% Print the solution for the given hashiwokakero board.
%
% @param Board  The hashiwokakero board.
% @param NESW   The N,E,S,W variables for each cell in the board.
print_board(Board, NESW) :-
        dim(Board, [_,N]),
        (foreachindex([I,J], Board), param(Board, NESW, N) do
            Sum is Board[I,J],
            (Sum > 0 ->
                write(Sum)
            ; 
                NS is NESW[I,J,1],
                EW is NESW[I,J,2],
                symbol(NS, EW, Char),
                write(Char)
            ),
            (J == N -> nl ; write(' '))
        ).

% Symbols to be used when printing the board.
symbol(0, 0, ' ').
symbol(0, 1, '-').
symbol(0, 2, '=').
symbol(1, 0, '|').
symbol(2, 0, 'X').

% Run tests on the provided benchmarks.
%
% @param Verbose Flag denoting whether or not intermediate results should be displayed.
benchmark(Verbose) :-
    findall(Name-Board, (board(Name,Board) ; puzzle(Name,_,_),read_puzzle(Name,Board)), Puzzles),
    (foreach(Name-Board, Puzzles),
     fromto(0, InTime, OutTime, TotalTime),
     fromto(0, InBacktracks, OutBacktracks, TotalBacktracks),
     param(Verbose) do
        (Verbose -> write('Puzzle name : '), write(Name), nl ; true),
        hashi(Board, Time, Backtracks, NESW),
        (Verbose -> write('Backtracks : '), write(Backtracks), nl ; true),
        (Verbose -> write('Time : '), write(Time), nl ; true),
        (Verbose -> print_board(Board, NESW) ; true),
        OutTime is InTime + Time,
        OutBacktracks is InBacktracks + Backtracks
    ),
    write('Total time : '), write(TotalTime), nl,
    write('Total backtracks : '), write(TotalBacktracks), nl.
	
% Read in a puzzle with given identifier, turn it into a board.
%
% @param Id     The identifier of the puzzle.
% @param Board  The resulting board.
read_puzzle(Id, Board) :-
	puzzle(Id, Imax, P),
	dim(Board, [Imax,Imax]),
	(foreachindex([I,J],Board), param(Board,P) do
		(member((I,J,A), P) -> A is Board[I,J] ; 0 is Board[I,J])
	).

%
% Puzzles for testing purposes
%

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
	
board(simple14,
     []([](2,4,2))
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

