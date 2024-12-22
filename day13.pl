:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).

button(X-Y) --> "Button ", [_], ": X+", integer(X), ", Y+", integer(Y), "\n".
config(config(A,B,PrizeX-PrizeY)) -->
    button(A), button(B),
    "Prize: X=", integer(PrizeX), ", Y=", integer(PrizeY), "\n".

input(I) :-
    phrase_from_file(sequence(config, "\n", I), "day13.txt").

winnable(InMoves, config(Ax-Ay,Bx-By,Px-Py), Tokens) :-
    writeln(is_winnable(config(Ax-Ay,Bx-By,Px-Py),inmoves(InMoves))),
    [Am,Bm] ins 0..InMoves,
    Px #= Ax*Am + Bx*Bm,
    Py #= Ay*Am + By*Bm,
    Tokens #= 3*Am + Bm.

part(MaxPresses, AllTokens) :-
    input(Configs),
    convlist(winnable(MaxPresses), Configs, Tokens),
    sum_list(Tokens, AllTokens).

part1(A) :- part(100, A).

fix(config(A,B,Px-Py), config(A,B,Px1-Py1)) :-
    Px1 is Px + 10000000000000,
    Py1 is Py + 10000000000000.

part2(A) :-
    input(Configs0),
    maplist(fix, Configs0, Configs),
    convlist(winnable(sup), Configs, Tokens),
    sum_list(Tokens, A).
