%%%-------------------------------------------------------------------
%%% @author cedricseger
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Feb 2017 23:25
%%%-------------------------------------------------------------------
-module(life2).
-author("cedricseger").

%% API
-export([bench/2]).

% -----------------------
% Benchmarking and state initializations
% -----------------------

state(M) ->
  Rows = next_rows(M,0,no),
  list_to_tuple(Rows).


bench(N, M) ->
  State = state(M),
  Start = erlang:system_time(micro_seconds),
  Final = generations(N, M, State),
  Stop = erlang:system_time(micro_seconds),
  Time = Stop - Start,
  io:format("~w generations computed in ~w us~n", [N,Time]),
  io:format("final state: ~w~n",[Final]),
  Time.

generations(0,_M, State) ->
  State;
generations(N, M, State) ->
  generations(N-1, M, next_state(M, State)).


%% Selects element from a 2-dimensional tuple
element(Row, Col, Grid) ->
  element(Col+1, element(Row+1,Grid)).

%% Generates a new state/grid of size M
next_state(M,Grid) ->
  Rows = next_rows(M,0,Grid),
  list_to_tuple(Rows).

next_rows(M,M, _Grid) ->
  [];
next_rows(M,R,Grid) ->
  [next_row(M,R,Grid) | next_rows(M,R+1,Grid)].

% Creates new row of size M
next_row(M,R,Grid) ->
  Cells = next_cells(M,R,0,Grid),
  list_to_tuple(Cells).

next_cells(M, _R, M, _Grid) ->
  [];
next_cells(M,R,C,Grid) ->
  [next_cell(M,R,C,Grid) | next_cells(M,R,C+1,Grid)].

next_cell(_M,_R,_C,no) ->
  flip();
next_cell(M,R,C,Grid) ->
  S = s(M,R,C,Grid),
  SW = sw(M,R,C,Grid),
  SE = se(M,R,C,Grid),
  N = n(M,R,C,Grid),
  NE = ne(M,R,C,Grid),
  NW = nw(M,R,C,Grid),
  E = e(M,R,C,Grid),
  W = w(M,R,C,Grid),
  This = this(R,C,Grid),
  rule([S,SW,SE,N,NE,NW,E,W], This).

% Generates random new state for a cell
flip() ->
  Flip = rand:uniform(4),
  if
    Flip == 1 -> alive ;
    Flip =/= 1 -> dead
  end.

rule(Neighbours, State) ->
  Alive = alive(Neighbours),
  if
    Alive < 2 -> dead;
    Alive == 2 -> State;
    Alive == 3 -> alive;
    Alive > 3 -> dead
  end.

alive(Neighbours) -> lists:foldl(fun(dead, Sum) -> Sum; (alive,Sum) -> 1+Sum end, 0, Neighbours).

% -----------------------
% Methods for accessing neighbours of a given cell
% eg: Finds south neighbour of a cell in row R, and col C
% -----------------------
s(M,R,C,Grid) ->
  element((R+1) rem M, C, Grid).

sw(M,R,C,Grid) ->
  element((R+1) rem M, (C+(M-1)) rem M, Grid).

se(M,R,C,Grid) ->
  element((R+1) rem M, (C+1) rem M, Grid).

n(M,R,C,Grid) ->
  element((R+(M-1)) rem M, C, Grid).

ne(M,R,C,Grid) ->
  element((R+(M-1)) rem M, (C+1) rem M, Grid).

nw(M,R,C,Grid) ->
  element((R+(M-1)) rem M, (C+(M-1)) rem M, Grid).

e(M,R,C,Grid) ->
  element(R, (C+1) rem M, Grid).

w(M,R,C,Grid) ->
  element(R, (C+(M-1)) rem M, Grid).

% Returns state of cell itself.
this(R,C,Grid) ->
  element(R,C,Grid).