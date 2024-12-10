:- use_module(library(aggregate)).
:- use_module(util).
:- dynamic antenna/2. % antenna(Freq, X-Y).
:- dynamic size/2. % size(W,H)

at(_, 46) :- !.
at(X-Y, Freq) :- asserta(antenna(Freq,X-Y)).

input :-
    retractall(antenna(_,_)),
    retractall(size(_,_)),
    load_grid('day8.txt', at, W-H),
    asserta(size(W,H)).

antinode(X-Y) :- antenna(Freq, X1-Y1), antenna(Freq, X2-Y2), X1-Y1 \== X2-Y2,
                 Dx is X2 - X1, Dy is Y2 - Y1,
                 size(W,H),
                 ((X is X1 - Dx, Y is Y1 - Dy); (X is X2 + Dx, Y is Y2 + Dy)),
                 X >= 0, Y >= 0, X < W, Y < H.

slope(X1-Y1, X2-Y2, S) :- Dx is X2 - X1, Dy is Y2 - Y1, Dx \== 0, S is Dy / Dx.
antinode2(X-Y) :- size(W,H), MaxX is W - 1, MaxY is H - 1,
                  between(0, MaxX, X), between(0, MaxY, Y),
                  antenna(Freq, A1), antenna(Freq, A2),
                  A1 \== A2,
                  slope(X-Y, A1, S), slope(X-Y, A2, S).
antinode2(X-Y) :- antenna(_, X-Y).

solve(P1, P2) :-
    input,
    aggregate_all(count, A, antinode(A), P1),
    aggregate_all(count, A, antinode2(A), P2).
