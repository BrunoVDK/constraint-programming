% solve(X)  :- 
%     the query X succeeds for the pure Prolog
%     program accessible by clause/2. 

solve(true) :- !. 
solve((A,B)) :-  !, solve(A), solve(B).
solve(A) :-  clause(A, B), solve(B).
