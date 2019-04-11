% transpose(Matrix, Transpose) :-
%     Transpose is a transpose of the matrix Matrix.

transpose(Matrix, Transpose) :-
    dim(Matrix,[R,C]),
    dim(Transpose,[C,R]),
    ( foreachelem(El,Matrix,[I,J]),
      param(Transpose)
    do
      subscript(Transpose,[J,I],El)
    ).
