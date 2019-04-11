add_const(Increment, Decrement, InList, OutList) :-
    ( foreach(In,InList),
      foreach(Out,OutList),
      param(Increment,Decrement)
    do
      ( In >= 0 -> 
        Out is In+Increment
      ;
        Out is In-Decrement
      )
    ).
