% solve(X, N)  :- 
%     the query X succeeds for the pure Prolog 
%     program accessible by clause/2 in N steps.

solve(true, 0) :- !. 
solve((A, B), N) :-  !, solve(A, N1), solve(B, N2), 
                     N is N1+N2.
solve(A, N) :-  clause(A, B), solve(B, M), 
                N is M+1.
