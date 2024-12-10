:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(util).

equation(Res-Ns) --> integer(Res), `: `, sequence(integer, ` `, Ns).

op1(L,R,Ans) :- Ans is L * R.
op1(L,R,Ans) :- Ans is L + R.
op2(L,R,Ans) :- op1(L,R,Ans).
op2(L,R,Ans) :- atom_concat(L,R,A), atom_number(A, Ans).

calc(_, Res-[Res]) :- !.
calc(_, Res-[V|_]) :- V > Res, !, fail.
calc(Op, Res-[L,R|Rest]) :- call(Op,L,R,Ans), calc(Op, Res-[Ans|Rest]).

line(Line, P1In-P2In, P1Out-P2Out) :-
    phrase(equation(Res-Ns), Line),
    (calc(op1, Res-Ns) -> P1Out is P1In + Res; P1Out = P1In),
    (calc(op2, Res-Ns) -> P2Out is P2In + Res; P2Out = P2In).

solve(P1, P2) :- util:fold_line('day7.txt', line, 0-0, P1-P2).
