book(harry_potter, rowlings).
book(anna_karenina, tolstoy).
book(elements, euclid).
book(histories, herodotus).
book(constraint_logic_programming, apt).
book(constraint_logic_programming, wallace).

genre(harry_potter, fiction).
genre(anna_karenina, fiction).
genre(elements, science).
genre(histories, history).
genre(constraint_logic_programming, science).

author(Author, Genre) :- book(X, Author), genre(X, Genre).
