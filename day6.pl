:- use_module(library(yall)).
:- use_module(library(readutil)).

:- dynamic grid/2.
:- dynamic visited/1.

load_grid(_, _, []) :- !.
load_grid(_, Y, [10|Rest]) :- succ(Y, Y1), load_grid(0, Y1, Rest), !.
load_grid(X, Y, [What|Rest]) :- asserta(grid(X-Y, What)), succ(X,X1), load_grid(X1, Y, Rest).

input :-
    read_file_to_codes("day6.txt", Codes, []),
    retractall(grid(_,_)),
    load_grid(0,0, Codes).

fwd(X-Y0, up, X-Y1) :- Y1 is Y0 - 1.
fwd(X-Y0, down, X-Y1) :- Y1 is Y0 + 1.
fwd(X0-Y, right, X1-Y) :- X1 is X0 + 1.
fwd(X0-Y, left, X1-Y) :- X1 is X0 - 1.

turn(up, right).
turn(right, down).
turn(down, left).
turn(left, up).

type(35, blocked). % obstruction #
type(94, open). % start ^
type(46, open). % empty .

blocked(Pos) :- grid(Pos, Ch), type(Ch, blocked).
open(Pos) :- grid(Pos, Ch), type(Ch, open).
outside(Pos) :- \+ grid(Pos, _).

start(X-Y) :- grid(X-Y, 94). % ^ character

% done, this would loop... end it here and record loop
walk(Pos-Dir, Pos-Dir) :- fwd(Pos, Dir, Pos1), visited(Pos1-Dir), throw(loop).
walk(Pos-Dir, Pos-Dir) :- fwd(Pos, Dir, Pos1), outside(Pos1), !. % done, this is the final state before going outside
walk(PosIn-DirIn, Final) :- fwd(PosIn, DirIn, NextPos), blocked(NextPos),
                            turn(DirIn, Dir1), walk(PosIn-Dir1, Final).
walk(PosIn-DirIn, Final) :- fwd(PosIn, DirIn, NextPos), open(NextPos),
                            asserta(visited(NextPos-DirIn)), walk(NextPos-DirIn, Final).

part1(A) :-
    input,
    retractall(visited(_)),
    start(Pos),
    asserta(visited(Pos-up)),
    walk(Pos-up, _),
    aggregate_all(count, N, visited(N-_), A).

walk_with_obstruction(Start, Obstruction, Loop) :-
    open(Obstruction),
    retractall(visited(_)), retractall(grid(Obstruction, 46)),
    %writeln(checking(Obstruction)),
    asserta(grid(Obstruction, 35)),
    catch((walk(Start-up, _), Loop=0),
          loop,
          Loop=1),
    % retract the obstruction
    retractall(grid(Obstruction, 35)),
    asserta(grid(Obstruction, 46)).

part2(A) :-
    part1(_), % run part1 to get all visited positions
    aggregate_all(set(N), N, visited(N-_), Candidates),
    start(Pos),
    retractall(loop(_)),
    % For each visited, check all opens for candidates
    maplist({Pos}/[Obs,Loop]>>walk_with_obstruction(Pos,Obs,Loop), Candidates, Loops),
    sum_list(Loops, A).
