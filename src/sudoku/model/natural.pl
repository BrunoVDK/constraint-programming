%
% Natural combined model for the Sudoku CLP solver.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

% Set up the model for the given puzzle.
%
% @param Puzzle     The input puzzle (a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
setup_model(natural, Puzzle, N, K, Vars) :-
    % Set up decision variables (record pre-filled cells)
    dim(Variables, [N,N,N]),
    %   ...
    % (multifor([R,C], 1, N), fromto(Puzzle, [[X|Xs]|Rows], Out, []), param(Variables) do
    %    (var(X) -> true ; 1 is Variables[R,C,X]),
    %    (Xs == [] -> Out = Rows ; Out = [Xs|Rows])
    % ),
    (for(R, 1, N), foreach(Row, Puzzle), param(N, Variables) do
        (for(C, 1, N), foreach(X, Row), param(R, Variables) do
            (var(X) -> true ; 1 is Variables[R,C,X])
        )
    ),
    % Declare domains and generate constraints
    declare_domains_natural(Variables),
    generate_constraints_natural(Variables, N, K),
    Vars is collection_to_list(Variables).

% Declare the domains.
%
% @param Variables The decision variables.
declare_domains_natural(Variables) :-
    Variables :: 0..1. % All booleans

% Generate the constraints for the given decision variables.
%
% @param Variables  The decision variables.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
generate_constraints_natural(Variables, N, K) :-
    % Primal/Row/Column variables
    (multifor([I,J], 1, N), param(N, Variables) do
        ic_global:occurrences(1, Variables[I,J,1..N], 1),
        ic_global:occurrences(1, Variables[1..N,I,J], 1),
        ic_global:occurrences(1, Variables[I,1..N,J], 1)
        %ic_global:atmost(1, Variables[I,J,1..N], 1),
        %ic_global:atmost(1, Variables[1..N,I,J], 1),
        %ic_global:atmost(1, Variables[I,1..N,J], 1),
        %ic_global:atleast(1, Variables[I,J,1..N], 1),
        %ic_global:atleast(1, Variables[1..N,I,J], 1),
        %ic_global:atleast(1, Variables[I,1..N,J], 1)
    ),
    % Block variables (I = Row, J = Column, V = Value)
    (multifor([I,J,V], 1, N, [K,K,1]), param(K, Variables) do
        ic_global:occurrences(1, flatten(Variables[I..I+K-1,J..J+K-1,V]), 1)
        %ic_global:atmost(1, flatten(Variables[I..I+K-1,J..J+K-1,V]), 1),
        %ic_global:atleast(1, flatten(Variables[I..I+K-1,J..J+K-1,V]), 1)
    ).

% Transform the assignments to the decision variables to a solved puzzle.
%
% @param Variables  The decision variables (should be assigned).
% @param Puzzle     The input puzzle (this is a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
% @param Solution   The puzzle's solution corresponding to the assignments to the variables.
read_solution(natural, Variables, _, N, _, Solution) :-
    dim(SolutionArray, [N,N]),
    (multifor([R,C,V], 1, N), foreach(X, Variables), param(SolutionArray) do
        (1 is X -> V is SolutionArray[R,C] ; true)
        % Note, ic_global:bool_channeling/3 can't be used as Variables is matrix
    ),
    list_2d_to_array(Solution, SolutionArray).
