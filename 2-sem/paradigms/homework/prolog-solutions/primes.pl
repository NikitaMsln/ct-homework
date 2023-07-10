set_composite(M, A, L) :-
    A > L,
    !.
set_composite(M, A, L) :-
    assert(composite(A)),
    A1 is M + A,
    set_composite(M, A1, L).

find_composite(N, L) :-
    N1 is N * N,
    N1 > L,
    !.
find_composite(N, L) :-
    composite(N),
    N1 is N + 1,
    find_composite(N1, L),
    !.
find_composite(N, L) :-
    N2 is N * N,
    set_composite(N, N2, L),
    N1 is N + 1,
    find_composite(N1, L).

init(M) :- find_composite(2, M).

prime(N) :-
    N > 1,
    \+ composite(N).

prime_divisors(1, P, []) :- !.
prime_divisors(N, P, [N]) :- prime(N), !.
prime_divisors(N, P, [P | Divisors]) :-
    prime(P),
    0 is mod(N, P),
    N1 is N / P,
    prime_divisors(N1, P, Divisors),
    !.
prime_divisors(N, P, Divisors) :-
    P2 is P * P,
    P2 =< N,
    P1 is P + 1,
    prime_divisors(N, P1, Divisors),
    !.

multiply(1, []).
multiply(R, [M | Tail]) :- multiply(R2, Tail), R is R2 * M.

sorted_primes_array(R, []).
sorted_primes_array(R, [P | Tail]) :-
    prime(P),
    R =< P,
    sorted_primes_array(P, Tail).

prime_divisors(N, Divisors) :-
    number(N),
    !,
    prime_divisors(N, 2, Divisors).
prime_divisors(N, Divisors) :-
    list(Divisors),
    sorted_primes_array(1, Divisors),
    multiply(N, Divisors).

divisor_divisors(N, P, []) :-
    P1 is P * P,
    N < P1,
    !.
divisor_divisors(N, P, [Divisors]) :-
    P1 is P * P,
    N = P1,
    prime_divisors(P, Divisors),
    !.
divisor_divisors(N, P, [Divisors1, Divisors2 | Tail]) :-
    0 is mod(N, P),
    prime_divisors(P, Divisors1),
    P1 is N / P,
    prime_divisors(P1, Divisors2),
    P2 is P + 1,
    divisor_divisors(N, P2, Tail),
    !.
divisor_divisors(N, P, Tail) :-
    P1 is P + 1,
    divisor_divisors(N, P1, Tail),
    !.

divisors_divisors(N, Divisors) :- divisor_divisors(N, 1, Divisors).