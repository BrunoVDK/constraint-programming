% This program contains three search_credit/3 predicates.
% The second and third one is in the comments section.


% search(List, Credit) :- 
%     Search for solutions with a given Credit. 

search(List, Credit) :-
    ( fromto(List, Vars, Rest, []),
      fromto(Credit, CurCredit, NewCredit, _)
    do
      choose_var(Vars, Var-Domain, Rest),
      choose_val(Domain, Val, CurCredit, NewCredit),
      Var = Val
    ).


% choose_var(List, Var, Rest) :- 
%     Var is a member of List and
%     Rest contains all the other members.

choose_var(List, Var, Rest) :- List = [Var | Rest].

choose_val(Domain, Val, CurCredit, NewCredit) :-
    share_credit(Domain, CurCredit, DomCredList),
    member(Val-NewCredit, DomCredList).

% share_credit(Domain, N, DomCredList) :- 
%     Admit only the first N values.

share_credit(Domain, N, DomCredList) :-
    ( fromto(N, CurCredit, NewCredit, 0),
      fromto(Domain, [Val|Tail], Tail, _),
      foreach(Val-N, DomCredList),
      param(N)
    do
      ( Tail = [] -> 
        NewCredit is 0 
      ; 
        NewCredit is CurCredit - 1 
      )
    ).

/*

% share_credit(Domain, N, DomCredList) :- 
%     Allocate credit N by binary chop.

share_credit(Domain, N, DomCredList) :-
    ( fromto(N, CurCredit, NewCredit, 0),
      fromto(Domain, [Val|Tail], Tail, _),
      foreach(Val-Credit, DomCredList)
    do
      ( Tail = [] -> 
        Credit is CurCredit 
      ; 
        Credit is fix(ceiling(CurCredit/2)) 
      ),
      NewCredit is CurCredit - Credit
    ).

% share_credit(Domain, N, DomCredList) :- 
%     Allocate credit N by discrepancy.

share_credit(Domain, N, DomCredList) :-
    ( fromto(N, CurCredit, NewCredit, 0),
      fromto(Domain, [Val|Tail], Tail, _),
      foreach(Val-CurCredit, DomCredList)
    do
      ( Tail = [] -> 
        NewCredit is 0 
      ; 
        NewCredit is CurCredit - 1 
      )
    ). 


/*