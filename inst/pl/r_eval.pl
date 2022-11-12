r_seed(S) :-
    r_eval('set.seed'(S)).

r_norm(N, L) :-
    r_eval(rnorm(N), L).
