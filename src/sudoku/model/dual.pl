%
% Dual model for the Sudoku CLP solver (referred to in slide 30 of 'Other.pdf').
%   We included several of the variants :
%   - Row x Value implies Column (dual1)
%   - Column x Value implies Row (dual2)
%   - Block x Position implies Value (dual3)
%   - Block x Value implies Position (dual4)
%   Only the dual3 variant doesn't make use of channeling. In the other viewpoints
%       it is hard to express certain constraints (on rows, columns or blocks).
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

variant(dual2) :- true. % Choose the variant here (dual1, dual2, dual3 or dual4)

% Set up the model for the given puzzle.
%
% @param Puzzle     The input puzzle (a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
setup_model(dual, Puzzle, N, K, Vars) :-
    dim(Variables, [N,N]),
    declare_domains_dual(Variables, N, K),
    generate_constraints_dual(Variables, Puzzle, N, K),
    Vars is collection_to_list(Variables).

% Declare the domains for the given puzzle.
%
% @param Variables  The variable array to declare the domains for.
% @param N          The dimension of the puzzle.
declare_domains_dual(Variables, N, _K) :-
    Variables :: 1..N. % Every dual model has the same variable domains

% Convert from the original puzzle to the variable array (and vice versa).
%
% @param Variables  The variable array.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
% @param Row        The row of the element in the original puzzle.
% @param Column     The column of the element in the original puzzle.
% @param Value      The value of the call at given row/column.
convert(Variables, _N, K, Row, Column, Value) :-
    (variant(dual1) -> Column is Variables[Row,Value] ; true),
    (variant(dual2) -> Row is Variables[Column,Value] ; true),
    block(K, Row, Column, Block), % Convert Row x Column to Block
    position(K, Row, Column, Position), % Convert Row x Column to Position
    (variant(dual3) -> Value is Variables[Block,Position] ; true),
    (variant(dual4) -> Position is Variables[Block,Value] ; true).

% Set up all constraints on rows for the variable array.
%
% @param Variables  The variable array.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
row_constraints(Variables, _N, _K) :-
    (foreacharg(Row, Variables) do alldifferent(Row)).

% Set up all constraints on columns for the variable array.
%
% @param Variables  The variable array.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
column_constraints(Variables, N, _K) :-
    (for(I, 1, N), param(Variables, N) do
        alldifferent(Variables[1..N,I])
    ).

% Set up all block constraints for the variable array.
%
% @param Variables  The variable array.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
block_constraints(Variables, N, K) :-
    % This is channeling
    dim(BlockArray, [N,N]),
    BlockArray :: 1..N,
    (for(Value, 1, N), param(BlockArray, Variables, N, K) do
        alldifferent(BlockArray[Value,1..N]), % All blocks have different values
        (multifor([Row,Column], 1, N), param(BlockArray, Variables, Value, K) do
            % The following is equivalent to an <=> constraint
            block(K, Row, Column, Block),
            position(K, Row, Column, Position),
            #=(BlockArray[Block,Value], Position, Bool),
            (variant(dual1) -> #=(Variables[Row,Value], Column, Bool) ; true),
            (variant(dual2) -> #=(Variables[Column,Value], Row, Bool) ; true)
        )
    ).

% Set up all remaining constraints for the dual3 variant
%
% @param Variables  The variable array.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
dual3_constraints(Variables, N, K) :-
    (for(R, 1, N), param(N, K, Variables) do % X is row or column
        SI is K * ((R-1) // K) + 1, EI is SI + K - 1,
        SJ is K * ((R-1) mod K) + 1, EJ is SJ + K - 1,
        length(RowValues, N),
        (   multifor([I,J], [SI,SJ], [EI,EJ]),
            foreach(RowValue, RowValues),
            param(Variables) do
            RowValue is Variables[I,J]
        ),
        alldifferent(RowValues)
    ),
    (for(C, 1, N), param(N, K, Variables) do % X is row or column
        SI is ((C-1) // K) + 1, SJ is ((C-1) mod K) + 1,
        length(ColumnValues, N),
        (   multifor([I,J], [SI,SJ], N, K),
            foreach(ColumnValue, ColumnValues),
            param(Variables) do
            ColumnValue is Variables[I,J]
        ),
        alldifferent(ColumnValues)
    ).

% Set up all remaining constraints for the dual4 variant
%
% @param Variables  The variable array.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
dual4_constraints(Variables, N, K) :-
    % This is channeling
    dim(MappedArray, [N,N]),
    MappedArray :: 1..N, % ... this is channelling, really
    (for(Row, 1, N), param(MappedArray, Variables, N, K) do
        alldifferent(MappedArray[Row,1..N]), % All rows have different values
        alldifferent(MappedArray[1..N,Row]), % All columns have different values
        (multifor([Column,Value], 1, N), param(MappedArray, Row, Variables, K) do
            block(K, Row, Column, Block), % Get Block for Row x Column
            position(K, Row, Column, Position), % Get Position for Row x Column
            % The following is equivalent to an <=> constraint
            #=(MappedArray[Row,Column], Value, Bool),
            #=(Variables[Block,Value], Position, Bool)
        )
    ).

% Generate the constraints for the given puzzle.
%
% @param Variables  The variables to generate constraints for.
% @param Puzzle     The input puzzle (a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
generate_constraints_dual(Variables, Puzzle, N, K) :-
    % Register pre-filled cells
    (foreach(Row, Puzzle), for(R, 1, N), param(Variables, N, K) do
        (foreach(X, Row), for(C, 1, N), param(N, K, R, Variables) do
            (nonvar(X) -> convert(Variables, N, K, R, C, X) ; true)
        )
    ),
    % Constraints
    row_constraints(Variables, N, K),
    ((variant(dual1) ; variant(dual2)) ->
        column_constraints(Variables, N, K),
        block_constraints(Variables, N, K)
    ;
        (variant(dual3) -> dual3_constraints(Variables, N, K) ; true),
        (variant(dual4) -> dual4_constraints(Variables, N, K) ; true)
    ).

% Transform the assignments to the decision variables to a solved puzzle.
%
% @param Variables  The decision variables (should be assigned).
% @param Puzzle     The input puzzle (this is a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
% @param Solution   The puzzle's solution corresponding to the assignments
%                       to the variables.
read_solution(dual, Variables, _, N, K, Solution) :-
    dim(SolutionArray, [N,N]),
    (multifor([R,C], 1, N), foreach(X, Variables), param(N, K, SolutionArray) do
        (nonvar(X) ->
            row(K, R, C, R1), column(K, R, C, C1),
            row(K, R, X, R2), column(K, R, X, C2),
            (variant(dual1) -> convert(SolutionArray, N, K, R, C, X) ; true),
            (variant(dual2) -> convert(SolutionArray, N, K, C, X, R) ; true),
            (variant(dual3) -> X is SolutionArray[R1,C1] ; true),
            (variant(dual4) -> C is SolutionArray[R2,C2] ; true)
        ;
            true
        )
    ),
    list_2d_to_array(Solution, SolutionArray).
