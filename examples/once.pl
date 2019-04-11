% one(Q) :- 
%     The query Q succeeds exactly once.

one(Q) :- 
    not(run(Q)),   % The query Q succeeds at most once.
    getval(x,1).   % The query Q succeeds at least once.

% run(Q) :- 
%     The query Q succeeds at least twice.

run(Q) :-
    setval(x,0),
    Q,
    incval(x),
    getval(x,2).
