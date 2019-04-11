% ordered(Xs) :- Xs is an =<-ordered list of numbers.

ordered(List) :-
    ( fromto(List,[El|Rest],Rest,[])
    do
      ordered(El, Rest)
    ).

ordered(_, []).
ordered(X, [Y|_]) :- X =< Y.
