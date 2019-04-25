% Partial solution by Stack Overflow user "jschimpf"
% https://stackoverflow.com/questions/20337029/hashi-puzzle-representation-to-solve-all-solutions-with-prolog-restrictions

:- lib(ic).  % uses the integer constraint library
:- compile('benchmarks/hashi_benchmarks.pl').
	

% 1. bridges run horizontally or vertically
% 2. bridges run in one straight line
% 3. bridges cannot cross other bridges or islands
% 4. at most two bridges connect a pair of islands
% 5. sum constraint
% 6. connectedness

hashi(Name, Time, Backtracks) :-
		(
			board(Name, Board)
		;
			makeFromPuzzle(Name, Board)
		),
        dim(Board, [Imax,Jmax]),
        dim(NESW, [Imax,Jmax,4]),   % 4 variables N,E,S,W for each field
        dim(FlowNESW, [Imax,Jmax,4]), % 4 variables FN,FE,FS,FW for each field
        getIslands(Board, Imax, Jmax, Islands),
        Islands = [[A,B]|_],  % pick a sink, always picks the first from the list, this is the island in the lowest row, in the furthest column
        Total is length(Islands),
        ( foreachindex([I,J],Board), param(Board,NESW,Imax,Jmax) do
            Sum is Board[I,J],
            N is NESW[I,J,1],
            E is NESW[I,J,2],
            S is NESW[I,J,3],
            W is NESW[I,J,4],

            % Constraints 1 and 2:
            % The combination of N=S etc on tiles without island
            % and f.i. north on current tile must equal south on
            % other northern tile, express that bridges can only run
            % in a straight line, horizontally or vertically.
            % They also express another constraint that was not stated
            % explicitly: that bridges must be on both sides connected
            % to islands.
            ( I > 1    -> N #= NESW[I-1,J,3] ; N = 0 ),
            ( I < Imax -> S #= NESW[I+1,J,1] ; S = 0 ),
            ( J > 1    -> W #= NESW[I,J-1,2] ; W = 0 ),
            ( J < Jmax -> E #= NESW[I,J+1,4] ; E = 0 ),
            ( Sum > 0 ->
              % Constraint 4
              [N,E,S,W] #:: 0..2,

              % Constraint 5
              N+E+S+W #= Sum
            ;
            N = S, E = W,
            
            % Constraint 3
            (N #= 0) or (E #= 0)
            )
		),

        % find a solution
		statistics(hr_time, Start1),
        %labeling(NESW),
		search(NESW, 0, input_order, indomain, complete, [backtrack(Backtracks1)]),
		statistics(hr_time, End1),
		Time1 is End1 - Start1,
		
		( foreachindex([I,J],Board), param(Board,NESW,FlowNESW,Imax,Jmax,A,B,Total) do
            Sum is Board[I,J],
            N is NESW[I,J,1],
            E is NESW[I,J,2],
            S is NESW[I,J,3],
            W is NESW[I,J,4],
            FN is FlowNESW[I,J,1],
            FE is FlowNESW[I,J,2],
            FS is FlowNESW[I,J,3],
            FW is FlowNESW[I,J,4],
			
		% Connectedness constraint:
        % Consists of 5 flow constraints.
            
			% Define the domain of the variables 
			% Implied constraint: if there's no bridge in a direction, there is no flow in that direction. 
			% Remark: the domain definition is not an implied constraint, it is necessary.
			(N = 0 -> FN #= 0 ; FN #:: -(Total-1)..(Total-1)),
			(E = 0 -> FE #= 0 ; FE #:: -(Total-1)..(Total-1)),
			(S = 0 -> FS #= 0 ; FS #:: -(Total-1)..(Total-1)),
			(W = 0 -> FW #= 0 ; FW #:: -(Total-1)..(Total-1)),
			% Flow constraint 3:
			% If a cell has a flow n in the direction of neighbor, that neighbor has a flow -n in the opposite
			% direction.
			( I > 1 ->
				FN #= -(FlowNESW[I-1,J,3])
				;
				FN = 0
			),
			( I < Imax  -> 
				FS #= -(FlowNESW[I+1,J,1]) 
				;
				FS = 0 
			),
			( J > 1	-> 
				FW #= -(FlowNESW[I,J-1,2]) 
				; 
				FW = 0 
				),
			( J < Jmax -> 
				FE #= -(FlowNESW[I,J+1,4]) 
				; 
				FE = 0 
			),
			( Sum > 0 ->
				( [I,J] = [A,B] ->
					% Flow constraint 5:
					% The net flow arriving at the sink island is equal to the amount of islands in the puzzle,
					% minus 1.
					FN+FE+FS+FW #= -(Total-1)
					;
					% Flow constraint 4:
					% Every non-sink island has a net flow is +1.
					FN+FE+FS+FW #= 1
				)
				;
				( N+E+S+W #= 0 ->
					% Flow constraint 1:
					% A cell that isn't an island, neither a bridge, has no flow.
					[FN,FE,FS,FW] #:: 0
					;
					% Flow constraint 2:
					% The net flow in a bridge cell is 0.
					FN #= -(FS),
					FE #= -(FW),
					FN+FE+FS+FW #= 0
				)
			)
        ),
		statistics(hr_time, Start2),
		%labeling(FlowNESW),
		search(FlowNESW, 0, input_order, indomain, complete, [backtrack(Backtracks2)]),
		statistics(hr_time, End2),
		Time2 is End2 - Start2,
		Time is Time1 + Time2,
		Backtracks is Backtracks1 + Backtracks2,
        print_board(Board, NESW).


print_board(Board, NESW) :-
        ( foreachindex([I,J],Board), param(Board,NESW) do
            ( J > 1 -> true ; nl ),
            Sum is Board[I,J],
            ( Sum > 0 ->
                write(Sum)
            ; 
                NS is NESW[I,J,1],
                EW is NESW[I,J,2],
                symbol(NS, EW, Char),
                write(Char)
            ),
            write(' ')
        ),
        nl.

symbol(0, 0, ' ').
symbol(0, 1, '-').
symbol(0, 2, '=').
symbol(1, 0, '|').
symbol(2, 0, 'X').

benchmark_hashi(Name) :-
	(write('Puzzle name : '), write(Name), nl ),
	hashi(Name, Time, Backtracks),
	(write('Backtracks : '), write(Backtracks), nl ),
    (write('Time : '), write(Time), nl ).

% For a puzzle, given as an array, returns a list containing all the islands. 
% An island is represented as [X,Y], the coordinates in the board.
getIslands(Name, Islands) :-
    board(Name, Board),
    dim(Board, [Imax,Jmax]),
    getIslands(Board, Imax, Jmax, Islands).
getIslands(Board, Imax, Jmax, Islands) :-
    getIslands(Board, Imax, Jmax, 1, 1, [], Islands).
getIslands(Board, Imax, Jmax, A, B, L, Islands) :-
    ( Board[A,B] > 0 ->
        Lnew = [[A,B]|L]
        ;
        Lnew = L
    ),
    Btemp is B+1,
    (Btemp > Jmax ->
        Bnew is 1,
        Anew is A+1
        ;
        Bnew is Btemp,
        Anew is A
    ),
    (Anew > Imax ->
        Islands = Lnew
        ;
        getIslands(Board, Imax, Jmax, Anew, Bnew, Lnew, Islands)
    ). 
	
% Turns a puzzle given in the format of the benchmark puzzles into an array.
makeFromPuzzle(Id, Board) :-
	puzzle(Id, Imax, P),
	dim(Board, [Imax,Imax]),
	( foreachindex([I,J],Board), param(Board,P) do
		( member((I,J,A), P) ->
			Board[I,J] #= A
		;
			Board[I,J] #= 0 
		)
	),
	labeling(Board).

% Examples

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
