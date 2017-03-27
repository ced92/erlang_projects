%%%-------------------------------------------------------------------
%%% @author cedricseger
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Feb 2017 19:58
%%%-------------------------------------------------------------------
-module(life1).
-author("cedricseger").

%% API
-export([bench/1]).

% -----------------------
% Benchmarking and state initializations
% -----------------------

state() ->
  { {dead, dead, dead, alive},
    {alive, alive, dead, dead},
    {dead, alive, alive, dead},
    {dead, alive, dead, alive}
  }.

bench(N) ->
  State = state(),
  Start = erlang:system_time(micro_seconds),
  Final = generations(N, State),
  Stop = erlang:system_time(micro_seconds),
  Time = Stop - Start,
  io:format("~w generations computed in ~w us~n", [N,Time]),
  io:format("final state: ~w~n",[Final]),
  Time.

generations(0,State) ->
  State;
generations(N, State) ->
  generations(N-1, next_grid(State)).

%% Selects element from a 2-dimensional tuple
element(Row, Col, Grid) ->
  element(Col+1, element(Row+1,Grid)).

% -----------------------
% Methods for accessing neighbours of a given cell
% eg: Finds south neighbour of a cell in row R, and col C
% -----------------------
s(R,C,Grid) ->
  element((R+1) rem 4, C, Grid).

sw(R,C,Grid) ->
  element((R+1) rem 4, (C+3) rem 4, Grid).

se(R,C,Grid) ->
  element((R+1) rem 4, (C+1) rem 4, Grid).

n(R,C,Grid) ->
  element((R+3) rem 4, C, Grid).

ne(R,C,Grid) ->
  element((R+3) rem 4, (C+1) rem 4, Grid).

nw(R,C,Grid) ->
  element((R+3) rem 4, (C+3) rem 4, Grid).

e(R,C,Grid) ->
  element(R, (C+1) rem 4, Grid).

w(R,C,Grid) ->
  element(R, (C+3) rem 4, Grid).

% Returns state of cell itself.
this(R,C,Grid) ->
  element(R,C,Grid).


% -----------------------
% Next-state functions
% Given a state, generates next states
% -----------------------

next_grid(Grid) ->
  R1 = next_row(0, Grid),
  R2 = next_row(1, Grid),
  R3 = next_row(2, Grid),
  R4 = next_row(3, Grid),
  {R1,R2,R3,R4}.

next_row(R, Grid) ->
  C1 = next_cell(R,0,Grid),
  C2 = next_cell(R,1,Grid),
  C3 = next_cell(R,2,Grid),
  C4 = next_cell(R,3,Grid),
  {C1,C2,C3,C4}.

next_cell(R,C,Grid) ->
  S = s(R,C,Grid),
  SW = sw(R,C,Grid),
  SE = se(R,C,Grid),
  N = n(R,C,Grid),
  NE = ne(R,C,Grid),
  NW = nw(R,C,Grid),
  E = e(R,C,Grid),
  W = w(R,C,Grid),
  This = this(R,C,Grid),
  rule([S,SW,SE,N,NE,NW,E,W], This).

rule(Neighbours, State) ->
  Alive = alive(Neighbours),
  if
    Alive < 2 -> dead;
    Alive == 2 -> State;
    Alive == 3 -> alive;
    Alive > 3 -> dead
  end.

alive(Neighbours) -> lists:foldl(fun(dead, Sum) -> Sum; (alive,Sum) -> 1+Sum end, 0, Neighbours).