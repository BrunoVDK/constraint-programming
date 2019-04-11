% ordered(Xs) :- Xs is an =<-ordered list of numbers.

ordered([]).
ordered([H | Ts]) :- 
    ( number(H) ->
      ordered(H, Ts)
    ;
      write("wrong input":H)
    ).

% ordered(H, Ts) :- [H|Ts] is an =<-ordered list of numbers.

ordered(_, []).
ordered(H, [Y | Ts]) :- 
    ( number(Y) ->
      H =< Y, ordered(Y, Ts)
    ;
      write("wrong input":Y)
    ).
