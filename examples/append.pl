% app(Xs, Ys, Zs) :- Zs is the result of 
%     concatenating the lists Xs and Ys. 

app([], Ys, Ys).
app([X | Xs], Ys, [X | Zs]) :- app(Xs, Ys, Zs).