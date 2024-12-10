:- use_module(library(rbtrees)).
:- use_module(library(yall)).

digit(Ch,D) :- Ch >= 48, D is Ch - 48.
input(Fm) :- read_file_to_codes('day9.txt', Cs, []),
             convlist(digit, Cs, I),
             to_file_map(file-0, I, Fm).

to_file_map(_, [], []).
to_file_map(file-ID, [Size|Rest], Out) :-
    length(File, Size),
    maplist(=(ID), File),
    succ(ID, ID1),
    to_file_map(free-ID1, Rest, Fm0),
    append(File, Fm0, Out).
to_file_map(free-ID, [Size|Rest], Out) :-
    length(Free, Size),
    maplist(=(f), Free),
    to_file_map(file-ID, Rest, Fm0),
    append(Free, Fm0, Out).

defrag(FileMap, Defragged) :-
    reverse(FileMap, Reverse),
    partition(=(f), Reverse, Free, ToMove0), % blocks to move
    length(Free, Space),
    length(ToMove, Space),
    append(ToMove, _, ToMove0),
    length(ToMove0, FileBlocks),
    %writeln(to_move(ToMove0, ToMove)),
    defrag_(FileBlocks, FileMap, ToMove, Defragged).

defrag_(0, _, _, []) :- !. % all fileblocks processed

defrag_(B, [f|Fm], [M|Move], [M|Rest]) :- B1 is B - 1, defrag_(B1, Fm, Move, Rest), !.
defrag_(B, [ID|Fm], Move, [ID|Rest]) :- B1 is B - 1, defrag_(B1, Fm, Move, Rest).

checksum(_, [], 0) :- !.
checksum(Pos, [f|Rest], CS) :- succ(Pos, Pos1), checksum(Pos1, Rest, CS), !.
checksum(Pos, [ID|Rest], CS) :-
    succ(Pos, Pos1),
    checksum(Pos1, Rest, CS0),
    CS is Pos*ID + CS0.
checksum(FileBlocks, CS) :- checksum(0, FileBlocks, CS).

part1(A) :- input(I), defrag(I, D), checksum(D, A).

% contiguous file blocks
take_all(Val, [Val|Rest], [Val|Before], After) :-
    take_all(Val, Rest, Before, After), !.
take_all(_, [], [], []) :- !.
take_all(Val, [X|Rest], [], [X|Rest]) :- Val \== X.

contiguous([], []) :- !.
contiguous([X|Rest0], [X-Len|Rest1]) :-
    take_all(X, Rest0, Xs, Rest),
    length([X|Xs], Len),
    contiguous(Rest, Rest1), !.

part2(Ans) :-
    input(I),
    contiguous(I, F),
    % make a tree of Pos-Type-Len
    rb_new(Empty),
    foldl([Type-Len,P0-T0,P1-T1]>>(rb_insert(T0, Type-Len-P0, 1, T1),
                                   P1 is P0 + Len),
          F, 0-Empty, _-Files),
    % get all files to try moving
    rb_fold([T-L-P-1,S0,S1]>>(T=f -> S1 = S0; S1=[T-L-P|S0]), Files, [], ToMove),
    foldl(try_move, ToMove, Files, Moved),
    checksum2(Moved, Ans).

try_move(Type-Len-FPos, FilesIn, FilesOut) :-
    % Try to find a space in files, that has length at least Len,
    (aggregate_all(min(Po, Sp),
                   (rb_in(f-Sp-Po, 1, FilesIn), Sp >= Len, Po < FPos),
                   min(Pos, Space))
    -> (rb_delete(FilesIn, Type-Len-FPos, T0), % delete file from orig pos
        rb_delete(T0, f-Space-Pos, T1), % delete empty space
        rb_insert_new(T1, Type-Len-Pos, 1, T2), % insert file at new pos
        (Space > Len
        -> (Rem is Space - Len, Pos1 is Pos + Len,
            rb_insert_new(T2, f-Rem-Pos1, 1, FilesOut))
        ; FilesOut = T2))
    ; FilesOut = FilesIn).

checksum2(Files, CS) :-
    % Sum all files' positions*id
    rb_fold([T-L-P-1, S0, S1]>>
            ( T=f
            -> S0 = S1
            ; ( P1 is P + L - 1,
                aggregate_all(sum(V), (between(P,P1,N), V is N * T), CS),
                S1 is S0 + CS )),
            Files, 0, CS).
