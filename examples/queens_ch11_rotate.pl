:- lib(ic).
:- lib(lists).

queens(rotate, Queens, Number) :-
    init(QueenStruct, Number),
    constraints(QueenStruct, Number),
    struct_to_list(QueenStruct, QList),
    search(rotate, QList),
    rotate(QueenStruct, Queens).

search(rotate, QList) :-
    middle_out_dom(QList, MOutDom),
    ( foreach(Val,MOutDom), param(QList) 
    do
      member(Val, QList) 
    ).

middle_out_dom([Q | _], MOutDom) :-
    get_domain_as_list(Q,OrigDom),
    middle_out(OrigDom, MOutDom).

rotate(QueenStruct, Queens) :-
    functor(QueenStruct, _, Number),
    init(RotatedQueens, Number),
    ( count(I,1,_),
      foreacharg(Q, QueenStruct),
      param(RotatedQueens)
    do
      RotatedQueens[Q] $= I
    ),
    struct_to_list(RotatedQueens, Queens).

init(QueenStruct, Number) :- 
    dim(QueenStruct,[Number]).

constraints(QueenStruct, Number) :-
    ( for(I,1,Number), param(QueenStruct,Number)
    do
      QueenStruct[I] :: 1..Number, 
      (for(J,1,I-1), param(I,QueenStruct)
      do
        QueenStruct[I] $\= QueenStruct[J],
        QueenStruct[I] - QueenStruct[J] $\= I-J, 
        QueenStruct[I]-QueenStruct[J] $\= J-I
      )
    ).

struct_to_list(Struct, List) :-
    ( foreacharg(Arg,Struct),
      foreach(Var,List)
    do
      Var = Arg
    ).

middle_out(List, MOutList) :-
    halve(List, FirstHalf, LastHalf),
    reverse(FirstHalf, RevFirstHalf),
    splice(LastHalf, RevFirstHalf, MOutList).



