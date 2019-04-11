% list(X) :- X is a list.

list(X) :- X == [], !.
list([_ | Ts]) :- nonvar(Ts), list(Ts).
