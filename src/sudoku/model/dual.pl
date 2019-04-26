%
% Dual model for the Sudoku CLP solver.
%   This includes several of the variants :
%   - Row x Value implies Column (dual1)
%   - Column x Value implies Row (dual2)
%   - Block x Position implies Value (dual3)
%   - Block x Value implies Position (dual4)
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

variant(dual1) :- true. % Choose the variant here (dual1, dual2, dual3 or dual4)

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
% @param I          The row of the element in the original puzzle.
% @param J          The column of the element in the original puzzle.
% @param V          The value of the call at given row/column.
convert(Variables, Row, Column, Value) :-
    (variant(dual1) -> Column is Variables[Row,Value] ; true),
    (variant(dual2) -> Row is Variables[Column,Value] ; true).

% Set up all row constraints for the variable array.
%
% @param Variables  The variable array.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
% @param Rows       A list of rows for the given variable array.
row_constraints(Variables, _N, _K) :-
    (foreacharg(Row, Variables) do
        alldifferent(Row)
    ).

% Set up all column constraints for the variable array.
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
    dim(BlockArray, [N,N]),
    BlockArray :: 1..9,
    (for(Block, 1, N), param(BlockArray, Variables, N, K) do
        alldifferent(BlockArray[Block,1..N]), % All blocks have different values
        (multifor([Cell,Value], 1, N), param(BlockArray, Variables, Block, K) do
            % The following is equivalent to an <=> constraint
            row(K, Block, Cell, Row), % Given Block x Cell => give Row
            column(K, Block, Cell, Column), % Given Block x Cell => give Column
            #=(BlockArray[Block,Cell], Value, Bool),
            #=(Variables[Row,Value], Column, Bool)
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
    (foreach(Row, Puzzle), for(R, 1, N), param(Variables, N) do
        (foreach(X, Row), for(C, 1, N), param(R, Variables) do
            (\+ var(X) -> convert(Variables, R, C, X) ; true)
        )
    ),
    % Constraints
    row_constraints(Variables, N, K),
    column_constraints(Variables, N, K),
    block_constraints(Variables, N, K).

% Transform the assignments to the decision variables to a solved puzzle.
%
% @param Variables  The decision variables (should be assigned).
% @param Puzzle     The input puzzle (this is a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
% @param Solution   The puzzle's solution corresponding to the assignments to the variables.
read_solution(dual, Variables, _, N, _K, Solution) :-
    dim(SolutionArray, [N,N]),
    (multifor([R,C], 1, N), foreach(X, Variables), param(SolutionArray) do
        (\+ var(X) -> convert(SolutionArray, R, C, X) ; true)
    ),
    list_2d_to_array(Solution, SolutionArray).
