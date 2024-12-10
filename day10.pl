:- use_module(util).
:- use_module(library(aggregate)).
:- dynamic g/2.

at(X-Y, Ch) :- N is Ch - 48, asserta(g(X-Y, N)).
input :-
    retractall(g(_,_)),
    load_grid('day10.txt', at, _).

neighbor(X-Y, X1-Y) :- X1 is X - 1.
neighbor(X-Y, X1-Y) :- X1 is X + 1.
neighbor(X-Y, X-Y1) :- Y1 is Y - 1.
neighbor(X-Y, X-Y1) :- Y1 is Y + 1.

next_step(X1-Y1, X2-Y2) :-
    g(X1-Y1, N),
    succ(N, N1),
    neighbor(X1-Y1, X2-Y2),
    g(X2-Y2, N1).

path(From, To) :- g(From, 8), neighbor(From, To), g(To, 9).
path(From, To) :-
    g(From, N), N < 8,
    next_step(From, Intermediate),
    path(Intermediate, To).

trailhead(X-Y) :- g(X-Y, 0).

score(Pos, Score) :-
    aggregate_all(count, To, path(Pos, To), Score).

score2(Pos, Score) :-
    aggregate_all(count, path(Pos,_), Score).

solve(P1,P2) :-
    input,
    aggregate_all(sum(S), (trailhead(Pos), score(Pos, S)), P1),
    aggregate_all(sum(S), (trailhead(Pos), score2(Pos, S)), P2).
