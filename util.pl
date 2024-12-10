:- module(util, [fold_line/4, load_grid/3]).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

fold_line_(In, Goal, Current, Result) :- read_line_to_codes(In, Codes),
                                         (Codes = end_of_file
                                         -> Result = Current
                                         ; (call(Goal, Codes, Current, Next),
                                            fold_line_(In, Goal, Next, Result))).

fold_line(File, Goal, Initial, Result) :-
    setup_call_cleanup(
        open(File, read, In),
        fold_line_(In, Goal, Initial, Result),
        close(In)).

load_grid_(_, _, _, []) :- !.
load_grid_(At, _, Y, [10|Rest]) :- succ(Y, Y1), load_grid_(At, 0, Y1, Rest), !.
load_grid_(At, X, Y, [A|Rest]) :- call(At, X-Y, A), succ(X,X1), load_grid_(At, X1, Y, Rest).

load_grid(File, At, Width-Height) :-
    read_file_to_codes(File, Codes, []),
    once(append([FirstLine, [10], _], Codes)),
    length(FirstLine, Width),
    length(Codes, TotalLen),
    Height is TotalLen / (Width+1),
    load_grid_(At,0,0,Codes).
