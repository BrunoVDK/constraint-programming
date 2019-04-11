% rev(List, Reverse) :- 
%     Reverse is a reverse of the list List or
%     List is a reverse of the list Reverse.

rev(List, Reverse) :-
    ( fromto([],Tail,[Head|Tail],List), 
      foreach(El,Reverse) do El = Head ).
