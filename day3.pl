input(I) :- read_file_to_string('day3.txt', I, []).

mult(re_match{0:_, 1:L, 2:R}, V0, Res) :-
    number_string(N1, L),
    number_string(N2, R),
    Res is V0 + (N1 * N2).

p1(Answer) :- input(I), re_foldl(mult, "mul\\((\\d+),(\\d+)\\)", I, 0, Answer, []).

toggled_mults(Acc, _, [], Acc). % done
toggled_mults(Acc, _, ["do()"|Items], Result) :- toggled_mults(Acc, true, Items, Result), !.
toggled_mults(Acc, _, ["don't()"|Items], Result) :- toggled_mults(Acc, false, Items, Result), !.
toggled_mults(Acc, false, [_|Items], Result) :- toggled_mults(Acc, false, Items, Result), !.
toggled_mults(Acc, true, [Mults|Items], Result) :-
    re_foldl(mult, "mul\\((\\d+),(\\d+)\\)", Mults, Acc, Acc1, []),
    toggled_mults(Acc1, true, Items, Result).

p2(Answer) :- input(I), re_split("do(n't)?\\(\\)", I, Splits),
              toggled_mults(0, true, Splits, Answer).
