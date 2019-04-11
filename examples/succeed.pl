% succeed(Q, N) :- 
%     N is the number of times the query Q succeeds.

succeed(Q, N) :- 
    ( setval(count,0),
      Q,
      incval(count),
      fail
    ;
      true
    ),
    getval(count,N).
