% ordered(Xs) :- Xs is an =<-ordered list of numbers.

ordered([]).
ordered([H | Ts]) :- ordered(H, Ts).

% ordered(H, Ts) :- [H|Ts] is an =<-ordered list of numbers.

ordered(_, []).
ordered(H, [Y | Ts]) :- H =< Y, ordered(Y, Ts).
