male(james1).
male(charles1).
male(charles2).
male(james2).
male(george1).

female(catherine).
female(elizabeth).
female(sophia).

child(charles1, james1).
child(elizabeth, james1).
child(charles2, charles1).
child(catherine, charles1).
child(james2, charles1).
child(sophia, elizabeth).
child(george1, sophia).

rule(parent(X, Y), child(Y, X)).

rule(daughter(X, Y), female(X), child(X, Y)).
