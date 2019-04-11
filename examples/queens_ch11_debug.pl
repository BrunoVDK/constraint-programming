:- lib(ic).
:- lib(lists).

queens(debug, Queens, VarChoice, Method, Number) :-
    init(QueenStruct, Number),
    constraints(QueenStruct, Number),
    struct_to_list(QueenStruct, Queens),
    struct_to_queen_list(QueenStruct, QList),
    search(QList, 2, VarChoice, show_choice, Method, []).

struct_to_queen_list(Struct, QList) :-
    ( foreacharg(Var,Struct),
      count(Col,1,_),
      foreach(Term,QList)
    do
      Term = q(Col, Var)
    ).

show_choice(q(I, Var)) :-
    indomain(Var),
    writeln(col(I):square(Var)).

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



