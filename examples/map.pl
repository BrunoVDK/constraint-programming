% map(P, Xs, Ys) :- 
%     the list Ys is the result of applying P 
%     elementwise to the list Xs.

map(_, [], []).
map(P, [X | Xs] , [Y | Ys]) :- 
    apply(P, [X, Y]), 
    map(P, Xs, Ys).

% apply(P, [X1, ..., Xn]) :- 
%     execute the atomic query P(X1, ..., Xn).

apply(P, Xs) :- Query =.. [P|Xs], Query.
