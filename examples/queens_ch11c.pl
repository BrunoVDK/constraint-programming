:- lib(ic).

% queens(Heur, Queens, Number) :- 
%            Solve the Number-queens problem using
%            heuristic Heur, generating answer Queens.

queens(Heur, Queens, Number) :-
    init(QueenStruct, Number),
    constraints(QueenStruct, Number),
    struct_to_list(QueenStruct, Queens),
    search(Heur, Queens).

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

