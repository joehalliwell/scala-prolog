member(X, [X|T]).
member(X, [H|T]) :- member(X, T).

length([], 0).
length([H|T], X) :- length(T, Y), X is Y + 1.

append([], X, X).
append([H|T], X, [H|T2]) :- append(T, X, T2).


fib(0, 0, 1).
fib(1, 1, 1).
fib(N, R1, R) :- N1 is N-1, fib(N1, R2, R1), R is R1 + R2.
fibe(N, R) :- fib(N, R1, R).


rev([], X, X).
rev([H|T], T2, R) :- rev(T, [H|T2], R).
rev(In, Out) :- rev(In, [], Out).

baz.
foo :- bar; baz.
