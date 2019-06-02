% puzzle 1, easy
% http://en.wikipedia.org/wiki/File:Val42-Bridge1n.png
% solution: http://en.wikipedia.org/wiki/File:Val42-Bridge1.png
puzzle(1,7,P):-
	P=[	(1,1,2),(1,2,3),(1,4,4),(1,6,2),
		(2,7,2),
		(3,1,1),(3,2,1),(3,5,1),(3,6,3),(3,7,3),
		(4,1,2),(4,4,8),(4,6,5),(4,7,2),
		(5,1,3),(5,3,3),(5,7,1),
		(6,3,2),(6,6,3),(6,7,4),
		(7,1,3),(7,4,3),(7,5,1),(7,7,2)].

% puzzle 2, moderate
% http://en.wikipedia.org/wiki/File:Bridges-example.png
% solution: http://upload.wikimedia.org/wikipedia/en/1/10/Bridges-answer.PNG

puzzle(2,13,P):-
	P=[	(1,1,2),(1,3,4),(1,5,3),(1,7,1),(1,9,2),(1,12,1),
		(2,10,3),(2,13,1),
		(3,5,2),(3,7,3),(3,9,2),
		(4,1,2),(4,3,3),(4,6,2),(4,10,3),(4,12,1),
		(5,5,2),(5,7,5),(5,9,3),(5,11,4),
		(6,1,1),(6,3,5),(6,6,2),(6,8,1),(6,12,2),
		(7,7,2),(7,9,2),(7,11,4),(7,13,2),
		(8,3,4),(8,5,4),(8,8,3),(8,12,3),
		(10,1,2),(10,3,2),(10,5,3),(10,9,3),(10,11,2),(10,13,3),
		(11,6,2),(11,8,4),(11,10,4),(11,12,3),
		(12,3,1),(12,5,2),
		(13,1,3),(13,6,3),(13,8,1),(13,10,2),(13,13,2)].

% puzzle 3
% http://www.conceptispuzzles.com/index.aspx?uri=puzzle/hashi/techniques
puzzle(3,6,P) :- 
    P = [ (1,1,1),(1,3,4),(1,5,2),
			    (2,4,2),(2,6,3),
			    (3,1,4),(3,3,7),(3,5,1),
			    (4,4,2),(4,6,5),
			    (5,3,3),(5,5,1),
			    (6,1,3),(6,4,3),(6,6,3)].

% puzzle 4
% http://www.conceptispuzzles.com/index.aspx?uri=puzzle/euid/010000008973f050f28ceb4b11c74e73d34e1c47d885e0d8449ab61297e5da2ec85ea0804f0c5a024fbf51b5a0bd8f573565bc1b/play
puzzle(4,8,P) :- 
	  P = [ (1,1,2),(1,3,2),(1,5,5),(1,7,2),
			    (2,6,1),(2,8,3),
			    (3,1,6),(3,3,3),
			    (4,2,2),(4,5,6),(4,7,1),
			    (5,1,3),(5,3,1),(5,6,2),(5,8,6),
			    (6,2,2),
			    (7,1,1),(7,3,3),(7,5,5),(7,8,3),
			    (8,2,2),(8,4,3),(8,7,2)].

% Generated from https://www.kakuro-online.com/hashi/ (hard)
puzzle(5,10,P) :-
    P = [(1,2,4),(1,4,5),(1,6,3),(1,8,3),(1,10,2),(2,1,1),(3,5,2),(3,7,4),(3,10,3),(4,2,3),(4,4,2),(4,6,1),(4,9,1),(5,1,3),(5,5,1),(5,7,3),(5,10,2),(6,2,4),(6,4,4),(6,6,3),(6,9,2),(7,1,3),(7,10,2),(8,2,3),(8,5,3),(8,7,4),(8,9,2),(9,8,2),(9,10,3),(10,1,3),(10,3,3),(10,5,5),(10,7,4),(10,9,1)].

% Generated from https://www.kakuro-online.com/hashi/ (medium)
puzzle(6,20,P) :-
    P = [(1,2,3),(1,4,3),(1,6,4),(1,8,5),(1,11,4),(1,14,4),(1,17,3),(1,20,3),(2,1,2),(2,3,2),(2,5,5),(2,7,2),(3,2,4),(3,4,1),(3,8,3),(3,10,3),(3,12,3),(3,14,5),(3,16,3),(3,19,2),(4,1,4),(4,3,3),(4,6,3),(4,9,2),(4,11,3),(4,13,3),(4,15,2),(4,17,1),(4,20,3),(5,2,4),(5,4,3),(5,7,1),(5,10,5),(5,12,4),(5,14,3),(5,16,4),(5,18,4),(6,3,3),(6,5,3),(6,13,2),(6,15,5),(6,17,3),(6,19,2),(7,1,4),(7,4,4),(7,6,5),(7,9,2),(7,12,4),(7,14,2),(7,16,2),(7,18,4),(7,20,3),(8,2,4),(8,5,1),(8,10,2),(8,13,1),(8,15,3),(8,17,2),(8,19,2),(9,1,4),(9,4,5),(9,6,5),(9,9,6),(9,12,6),(9,14,2),(9,16,4),(10,2,4),(10,5,4),(10,8,2),(10,13,2),(10,15,3),(10,18,3),(10,20,2),(11,1,3),(11,4,4),(11,7,1),(11,9,5),(11,11,2),(11,16,3),(11,19,3),(12,3,3),(12,5,5),(12,8,3),(12,10,3),(12,12,6),(12,15,3),(12,18,3),(13,1,2),(13,4,4),(13,6,2),(13,13,2),(13,16,2),(13,20,3),(14,2,3),(14,5,1),(14,7,3),(14,9,4),(14,11,1),(14,15,4),(14,18,4),(15,1,2),(15,4,3),(15,6,3),(15,13,1),(15,17,1),(15,19,3),(16,3,4),(16,5,4),(16,7,6),(16,9,6),(16,12,7),(16,15,6),(16,18,2),(16,20,4),(17,2,3),(17,4,4),(17,10,2),(17,13,1),(17,16,3),(17,19,2),(18,1,4),(18,3,4),(18,6,2),(18,8,3),(18,11,2),(18,18,2),(18,20,4),(19,13,3),(19,15,4),(19,19,1),(20,1,4),(20,3,5),(20,5,2),(20,8,3),(20,10,3),(20,12,4),(20,14,3),(20,16,5),(20,18,6),(20,20,4)].

% Puzzle(s) for testing connectedness constraint
puzzle(7,3,P) :-
    P = [(1,1,2),(1,3,2),(3,1,2),(3,3,2)].
puzzle(8,5,P) :-
    P = [(1,1,2),(1,3,2),(3,1,2),(3,3,2),(5,1,2),(5,3,2),(3,5,2),(5,5,2)].
puzzle(9,3,P) :-
    P = [(1,3,1),(3,1,1),(3,3,2)].