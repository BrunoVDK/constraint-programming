%
% Boolean model for the Sudoku CLP solver.
%   Every Sudoku is represented as an NxNxN matrix. For each cell
%   there is an array of N values, each of these values is zero except
%   for the one corresponding to the value that lies in the cell.
% The constraints are that every primal, row, column and block value have just one
%   1 value.
% Initially we based this on the Natural Combined Model as proposed by Pay & Cox
%   "Encodings, Consistency Algorithms and Dynamic Variable -
%       Value Ordering Heuristics for Multiple Permutation Problems" (2017)
% Since it uses disequality between slices (arrays), we wrote one implementation of
%   enforcing inequality in unequal_list/2. It wasn't a fruitful attempt.
% N.B. : the Natural Combined Model was part of a study to demonstrate a new
%   algorithm. Implementing it was more of an experiment than anything else.
%   The channeling model was a more appropriate (and fruitful) experiment.
% N.B. : the model we implement here can be seen as a linear programming model.
% N.B. : as F. Rossi points out in her book, this is generally not a good idea, it's
%           usually preferable to transform a boolean model into one with integers.
%
% @author   Michaël Dooreman & Bruno Vandekerkhove
% @version  1.0

:- import occurrences/3 from ic_global.

variant(original) :- fail.

% Set up the model for the given puzzle.
%
% @param Puzzle     The input puzzle (a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
setup_model(Puzzle, N, K, Vars) :-
    % Set up decision variables (record pre-filled cells)
    dim(Variables, [N,N,N]),
    %   ... (less readable)
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
    declare_domains_boolean(Variables),
    generate_constraints_boolean(Variables, N, K),
    Vars is collection_to_list(Variables).

% Declare the domains.
%
% @param Variables The decision variables.
declare_domains_boolean(Variables) :-
    Variables::0..1. % All booleans

% Generate the constraints for the given decision variables.
%
% @param Variables  The decision variables.
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
generate_constraints_boolean(Variables, N, K) :-
    (variant(original) -> % The original model
        (for(I, 1, N), param(Variables, N, K) do
            (for(J, I+1, N), param(Variables, N, K, I) do
                (for(V, 1, N), param(Variables, I, J, N, K) do
                    % Two columns, different value
                    unequal_list(Variables[V,I,1..N], Variables[V,J,1..N]),
                    % Two rows, different value
                    unequal_list(Variables[I,V,1..N], Variables[J,V,1..N]),
                    % Two positions in block, different value
                    row(K, V, I, RI), column(K, V, I, CI),
                    row(K, V, J, RJ), column(K, V, J, CJ),
                    unequal_list(Variables[RI,CI,1..N], Variables[RJ,CJ,1..N]),
                    % Value not twice in the same column
                    unequal_list(Variables[I,1..N,V], Variables[J,1..N,V]),
                    % Value not twice in the same row
                    unequal_list(Variables[1..N,I,V], Variables[1..N,J,V]),
                    % Value not twice in same block
                    row(K, V, 1, RV), column(K, V, 1, CV),
                    unequal_list(   Variables[RV..RV+K-1,CV..CV+K-1,I],
                                    Variables[RV..RV+K-1,CV..CV+K-1,J])
                )
            )
        )
    ; % Other (our own) approach
        % Primal/Row/Column variables
        (multifor([I,J], 1, N), param(N, Variables) do
            ic_global:occurrences(1, Variables[I,J,1..N], 1),
            ic_global:occurrences(1, Variables[1..N,I,J], 1),
            ic_global:occurrences(1, Variables[I,1..N,J], 1)
        ),
        % Block variables (I = Row, J = Column, V = Value)
        (multifor([I,J,V], 1, N, [K,K,1]), param(K, Variables) do
            ic_global:occurrences(1, flatten(Variables[I..I+K-1,J..J+K-1,V]), 1)
        )
    ).

% Given 2 lists, enforce the constraint that the two lists are different.
%
% @param List1  The first list.
% @param List2  The second list.
% @note There are a few ways to make this possible, one of which is the use of ~= which
%       does not propagate (it doesn't know about domains), and our own code which is
%       just as slow as it only propagates when only one variable is left.
% @note The code here is a bunch of experiments, mostly done out of curiosity.
unequal_list(List1, List2) :-
    collection_to_list(flatten(List1), Iterator1),
    collection_to_list(flatten(List2), Iterator2),
    % Experiment 1
    %Iterator1 ~= Iterator2.
    % Experiment 2 (didn't make much sense)
    %bool_channeling(X, Iterator1, 1), bool_channeling(Y, Iterator2, 1), X #\= Y.
    % Experiment 3 (very slow unless you explicitely say there's one 1 in each list)
    (foreach(X, Iterator1), foreach(Y, Iterator2), foreach((X #\= Y), Constraints) do true),
    1 #< sum(Constraints),
    1 #= sum(Iterator1),
    1 #= sum(Iterator2).
    % See slides Active.pdf, 27 and onwards
    %L1 is List1, L2 is List2,
    %(foreach(X, L1), foreach(Y, L2), fromto(Sum, S1, S2, 0) do
        %$\=(X, Y, B), % Reification
        %S1 = B + S2
    %),
    %eval(Sum) $>= 1. % See http://eclipseclp.org/doc/bips/lib/ic/index.html

% Transform the assignments to the decision variables to a solved puzzle.
%
% @param Variables  The decision variables (should be assigned).
% @param Puzzle     The input puzzle (this is a list).
% @param N          The dimension of the puzzle.
% @param K          The dimension of blocks.
% @param Solution   The puzzle's solution corresponding to the assignments to the variables.
read_solution(Variables, _Puzzle, N, _K, Solution) :-
    dim(SolutionArray, [N,N]),
    (multifor([R,C,V], 1, N), foreach(X, Variables), param(SolutionArray) do
        (nonvar(X), 1 is X -> V is SolutionArray[R,C] ; true)
        % Note, ic_global:bool_channeling/3 can't be used as Variables is 1D list
    ),
    list_2d_to_array(Solution, SolutionArray), !.
