member(X, [X|T]).
rule(member(X, [H|T]), member(X, T)).

length([], 0).
rule(length([H|T], X), length(T, Y), X is Y + 1).

%fib(0, 0).
%fib(1, 1).
%rule(fib(N, X), N1 is N - 1, N2 is N - 2, fib(N1, X1), fib(N2, X2), X is X1 + X2).

fib(0, 0, 0).
fib(1, 0, 1).
rule(fib(N, R1, R), N1 is N-1, fib(N1, R2, R1), R is R1 + R2).
rule(fib(N, R), fib(N, R1, R)).