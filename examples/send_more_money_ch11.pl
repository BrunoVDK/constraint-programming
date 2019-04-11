:- lib(ic).

send(List):-                                 
    List = [S,E,N,D,M,O,R,Y],                 
    List :: 0..9,                             
    diff_list(List),                     
                 1000*S + 100*E + 10*N + D    
               + 1000*M + 100*O + 10*R + E    
    $= 10000*M + 1000*O + 100*N + 10*E + Y,   
    S $\= 0,                                  
    M $\= 0,                                 
    shallow_backtrack(List).

shallow_backtrack(List) :- 
    ( foreach(Var,List) do once(indomain(Var)) ).

% diff_list(List) :- 
%     List is a list of different variables.

diff_list(List) :-
    ( fromto(List,[X|Tail],Tail,[]) 
    do
      ( foreach(Y, Tail), 
        param(X) 
      do
        X $\= Y
      )
    ).

