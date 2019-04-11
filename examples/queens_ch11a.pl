:- lib(ic).

queens(QueenStruct, Number) :-
    init(QueenStruct, Number),
    constraints(QueenStruct, Number),
    backtrack_free(QueenStruct).

init(QueenStruct, Number) :- 
    dim(QueenStruct, [Number]).

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

backtrack_free(QueenStruct):-
    ( foreacharg(Col,QueenStruct) do get_min(Col,Col) ).

