% Partial solution by Stack Overflow user "jschimpf"
% https://stackoverflow.com/questions/20337029/hashi-puzzle-representation-to-solve-all-solutions-with-prolog-restrictions

:- lib(ic).  % uses the integer constraint library

% 1. bridges run horizontally or vertically
% 2. bridges run in one straight line
% 3. bridges cannot cross other bridges or islands
% 4. at most two bridges connect a pair of islands
% 5. sum constraint
% 6. connectedness

hashi(Name) :-
        board(Name, Board),
        dim(Board, [Imax,Jmax]),
        dim(NESW, [Imax,Jmax,4]),   % 4 variables N,E,S,W for each field
        dim(FlowNESW, [Imax,Jmax,4]), % 4 variables FN,FE,FS,FW for each field
        getIslands(Board, Imax, Jmax, Islands),
        Islands = [[A,B]|_],  % pick a sink
        Total is length(Islands),
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
            ),

            % Constraint 6:
            % Consists of 5 flow constraints.
			%(N #= 0 -> FN = 0),
			%(E #= 0 -> FE = 0),
			%(S #= 0 -> FS = 0),
			%(W #= 0 -> FW = 0),
            % Flow constraint 1
            %( ((N+E+S+W #= 0), (Sum = 0)) ->  [FN,FE,FS,FW] #:: 0 ; true),
            % Flow constraint 2
            %( ((Sum = 0), (N+E+S+W #\= 0)) -> (FN #= -(FS), FE #= -(FW), FN+FE+FS+FW #= 0) ; true),
            % Flow constraint 3
            %( ((I > 1), (Sum = 0))     -> FN #= -(FlowNESW[I-1,J,3]) ; FN = 0 ),
            %( ((I < Imax ), (Sum = 0)) -> FS #= -(FlowNESW[I+1,J,1]) ; FS = 0 ),
            %( ((J > 1), (Sum = 0))     -> FW #= -(FlowNESW[I,J-1,2]) ; FW = 0 ),
            %( ((J < Jmax), (Sum = 0))  -> FE #= -(FlowNESW[I,J+1,4]) ; FE = 0 ),
            % Flow constraint 4
            %( (([I,J] \= [A,B]), (Sum > 0))  ->  FN+FE+FS+FW #= 1 ; true),
            % Flow constraint 5
            %( [I,J] = [A,B] -> FN+FE+FS+FW #= -(Total-1) ; true)
			%(N #= 0 -> FN = 0 ; true),
			%(E #= 0 -> FE = 0 ; true),
			%(S #= 0 -> FS = 0 ; true),
			%(W #= 0 -> FW = 0 ; true),
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
				[FN,FE,FS,FW] #:: -(Total-1)..(Total-1),
				( [I,J] = [A,B] ->
					FN+FE+FS+FW #= -(Total-1)
					;
					FN+FE+FS+FW #= 1
				)
				;
				FN #= -(FS), %
				FE #= -(FW), %
				(FN #= 0) or (FE #= 0), %
				FN+FE+FS+FW #= 0 %
				%( N+E+S+W #= 0 ->
				%	[FN,FE,FS,FW] :: 0
				%	;
				%	FN #= -(FS),
				%	FE #= -(FW),
				%	(FN #= 0) or (FE #= 0),
				%	FN+FE+FS+FW #= 0
				%)
			)
        ),

        % find a solution
        labeling(NESW),
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
