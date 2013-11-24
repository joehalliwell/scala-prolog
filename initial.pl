member(X, [X|T]).
rule(member(X, [H|T]), member(X, T)).

length([], 0).
rule(length([H|T], X), length(T, Y), X is Y + 1).