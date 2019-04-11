% mem(Element, List) :- Element is an element of 
%                       the list List. 

mem(X, [X | _]).
mem(X, [_ | Xs]) :- mem(X, Xs).
