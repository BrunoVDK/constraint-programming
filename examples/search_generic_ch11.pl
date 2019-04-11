:- lib(ic).

% search(List) :- Assign values from their domains
%                 to all the variables in List.

search_with_dom(List) :-
        ( fromto(List, Vars, Rest, [])
        do
          choose_var(Vars, Var, Rest),
          indomain(Var).
        ).

choose_var(List, Var, Rest) :- List = [Var | Rest].
