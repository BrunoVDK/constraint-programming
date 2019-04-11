search(List, Credit) :-
    ( fromto(List, Vars, Rest, []),
      fromto(Credit, TCredit, NCredit, _)
    do
      choose_var(Vars, Var-Domain, Rest),
      choose_val(Domain, Val, TCredit, NCredit),
      Var = Val
     ).

choose_var(List, Var, Rest) :- List = [Var | Rest].

choose_val(Domain, Val, TCredit, NCredit) :-
    share_credit(Domain, TCredit, DomCredList),
    member(Val-NCredit, DomCredList).

% share_credit(DomList,InCredit,DomCredList) :- 
%     Allocate credit by discrepancy.

share_credit(Domain, N, DomCredList) :-
   ( fromto(N, CurrCredit, NewCredit, -1),
     fromto(Domain, [Val|Tail], Tail, _),
     foreach(Val-CurrCredit, DomCredList)
   do
        ( Tail = [] ->
              NewCredit is -1
         ;
              NewCredit is CurrCredit -1
         )
    ).
