%%%-------------------------------------------------------------------
%%% @author cedricseger
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Feb 2017 23:50
%%%-------------------------------------------------------------------
-module(life3).
-author("cedricseger").

%% API
-export([bench/2]).

% -----------------------
% Benchmarking and state initializations
% -----------------------

state(M) ->
  First = row(M),
  {Rows, Last} = state(M,2,First),
  [Last, First | Rows].

state(M,M,First) ->
  Last = row(M),
  {[Last,First], Last};
state(M,R,First) ->
  Row = row(M),
  {Rows, Last} = state(M,R+1, First),
  {[Row|Rows], Last}.

% Generates a new row of size M
row(M) ->
  First = flip(),
  {Cells, Last} = row(M, 2, First),
  [Last, First | Cells].

row(M,M,First) ->
  Last = flip(),
  {[Last, First], Last};
row(M,R,First) ->
  Cell = flip(),
  {Cells, Last} = row(M,R+1,First),
  {[Cell|Cells], Last}.


% Generates random new state for a cell
flip() ->
  Flip = rand:uniform(4),
  if
    Flip == 1 -> alive ;
    Flip =/= 1 -> dead
  end.

bench(N, M) ->
  State = state(M),
  Start = erlang:system_time(micro_seconds),
  Final = generations(N, State),
  Stop = erlang:system_time(micro_seconds),
  Time = Stop - Start,
  io:format("~w generations computed in ~w us~n", [N,Time]),
  io:format("final state: ~w~n",[Final]),
  Time.

generations(0, State) ->
  State;
generations(N, State) ->
  generations(N-1, next_state(State)).


next_state([Rm,R1,R2 | Rest]) ->
  First = next_row(Rm,R1,R2),
  {Rows, Last} = next_rows([R1,R2 | Rest], First),
  [Last,First | Rows].

% -----------------------
% Representing state as list of rows -> [Rm,R1,R2,....,Rm,R1]
% M is the size of list
% When finding next_row we only need to consider R-1,R,R+1
% Hence the special representation
% More efficient implementation than searching lists for item we need (linear time)
% -----------------------
next_rows([Rl,Rm,R1], First) ->
  Last = next_row(Rl,Rm,R1),
  {[Last,First], Last};
next_rows([R1,R2,R3 | Rest], First) ->
  Next = next_row(R1,R2,R3),
  {Rows, Last} = next_rows([R2,R3 | Rest], First),
  {[Next|Rows], Last}.

next_row([NE,N,NW | Nr], [E,This,W |Tn], [SE,S,SW|Sr]) ->
  First = rule([S,SW,SE,N,NE,NW,E,W], This),
  {Columns,Last} = next_column([N,NW|Nr], [This,W | Tn], [S,SW|Sr],First),
  [Last,First|Columns].

% -----------------------
% Follows same pattern as next_rows()
% This functions is applied to each cell in a row
% -----------------------
next_column([NE,N,NW], [E,This,W], [SE,S,SW], First) ->
  Last = rule([S,SW,SE,N,NE,NW,E,W], This),
  {[Last,First], Last};
next_column([NE,N,NW | Nr], [E,This,W | Tn], [SE,S,SW | Sr], First) ->
  Next = rule([S,SW,SE,N,NE,NW,E,W], This),
  {Columns, Last} = next_column([N,NW|Nr], [This,W | Tn], [S,SW|Sr],First),
  {[Next|Columns],Last}.

rule(Neighbours, State) ->
  Alive = alive(Neighbours),
  if
    Alive < 2 -> dead;
    Alive == 2 -> State;
    Alive == 3 -> alive;
    Alive > 3 -> dead
  end.

alive(Neighbours) -> lists:foldl(fun(dead, Sum) -> Sum; (alive,Sum) -> 1+Sum end, 0, Neighbours).