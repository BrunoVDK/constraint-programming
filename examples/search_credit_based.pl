:- lib(ic).

% search(our_credit_search(Var,_), List) :- 
%     Find a labelling of List using our own credit based 
%     search procedure with an initial credit of Var.

search(our_credit_search(Var,_), List) :-
     search(List, 0, first_fail,
            our_credit_search(Var,_), complete, []).

% our_credit_search(Var, TCredit, NCredit) :-
%     Select a value for Var reducing the remaining 
%     credit from TCredit to NCredit.

our_credit_search(Var, TCredit, NCredit) :-
    get_domain_as_list(Var,Domain),
    share_credit(Domain, TCredit, DomCredList),
    member(Val-Credit, DomCredList),
    Var = Val, 
    NCredit = Credit.

% share_credit(DomList, InCredit, DomCredList) :- 
%     Share the credit in InCredit amongst the
%     values in DomList producing DomCredList.

share_credit(DomList, InCredit, DomCredList) :-
    ( fromto(InCredit, TCredit, NCredit, 0),
      fromto(DomList, [Val|Tail], Tail, _),
      foreach(Val-Credit, DomCredList)
    do
      Credit is fix(ceiling(TCredit/2)),
      NCredit is TCredit - Credit
    ). 
