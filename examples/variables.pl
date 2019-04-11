% vars(Term, List) :- 
%     List is the list of variables of the term Term.    

vars(Term, []) :- atomic(Term), !.
vars(Term, [Term]) :- var(Term), !.
vars(Term, List) :-
    compound(Term),
    functor(Term,_,K),
    args(K, Term, List).

% args(K, Term, List) :- 
%     List is the list of variables occurring 
%     in the first K arguments of the term Term.

args(0, _, []) :- !.
args(K, Term, List) :-
    K > 0,
    K1 is K-1,
    args(K1, Term, L1s),
    arg(K,Term,A),
    vars(A, L2s),
    app(L1s, L2s, List).

% app(Xs, Ys, Zs) :- Zs is the result of 
%     concatenating the lists Xs and Ys. 

app([], Ys, Ys).
app([X | Xs], Ys, [X | Zs]) :- app(Xs, Ys, Zs).
