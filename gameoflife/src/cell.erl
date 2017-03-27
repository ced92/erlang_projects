%%%-------------------------------------------------------------------
%%% @author cedricseger
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Feb 2017 20:38
%%%-------------------------------------------------------------------
-module(cell).
-author("cedricseger").

%% API
-export([bench/2]).

% -----------------------
% Benchmarking and state initializations
% -----------------------

bench(N,M) ->
  All = all(M),
  Start = erlang:system_time(micro_seconds),
  init(N,self(), All),
  collect_responses(All),
  Stop = erlang:system_time(micro_seconds),
  Time = Stop - Start,
  io:format("~w generations of size ~w computed in ~w us~n", [N,M,Time]).

% Collects the done messages from the on-going processes/cells
collect_responses(Neighbours) ->
  lists:map(fun(Pid) ->
    receive
      {done, Pid} -> Pid
    end
            end, Neighbours).

init(N, Ctrl, All) ->
  lists:foreach(fun(Pid) -> Pid ! {go, N, Ctrl} end, All).


% Generates grid, connects all cells
% Returns list of all cells
all(M) ->
  {Grid, All} = state(M),
  connect(Grid),
  All.

% Connects all rows
connect([Rl,Rm,R1]) ->
  connect(Rl,Rm,R1);
connect([R1,R2,R3 | Rest]) ->
  connect(R1,R2,R3),
  connect([R2,R3 | Rest]).

% Connects all the cells
connect([NE,N,NW], [E,This,W], [SE,S,SW]) ->
  This ! {init, [S,SW,SE,N,NE,NW,E,W]};
connect([NE,N,NW|Nr],[E,This,W|Tr],[SE,S,SW|Sr]) ->
  This ! {init, [S,SW,SE,N,NE,NW,E,W]},
  connect([N,NW | Nr], [This, W | Tr], [S,SW|Sr]).

% -----------------------
% Grid definitions
% -----------------------
state(M) ->
  {First, S0} = row(M,[]),
  {Rows, Last, S1} = state(M,2,First,S0),
  {[Last,First | Rows], S1}.

state(M,M,First, List) ->
  {Last, NewList} = row(M, List),
  {[Last,First], Last, NewList};
state(M,R,First, List) ->
  {Row, NewList} = row(M, List),
  {Rows, Last, FinalList} = state(M,R+1, First, NewList),
  {[Row|Rows], Last, FinalList}.

% Generates a new row of size M
% Also returns list of all generated cells so far
row(M,Cell_List) ->
  First = start(flip()),
  List = [First|Cell_List],
  {Cells, Last, Final_List} = row(M, 2, First,List),
  {[Last, First | Cells], Final_List}.

row(M,M,First,List) ->
  Last = start(flip()),
  NewList = [Last|List],
  {[Last, First], Last, NewList};
row(M,R,First,List) ->
  Cell = start(flip()),
  NewList = [Cell | List],
  {Cells, Last, Final_List} = row(M,R+1,First, NewList),
  {[Cell|Cells], Last, Final_List}.

% Generates random new state for a cell
flip() ->
  Flip = rand:uniform(4),
  if
    Flip == 1 -> alive ;
    Flip =/= 1 -> dead
  end.

% -----------------------
% Cell definitions
% -----------------------
start(State) ->
  spawn_link(fun() -> init(State) end).

% Process for creating new cell
% (1) Waits for list of neighbours
% (2) Waits for 'go' command to activate
init(State) ->
  receive
    {init, Neighbours} ->
      receive
        {go, N, Ctrl} ->
          cell(N,Ctrl,State,Neighbours)
      end
  end.

cell(0,Ctrl,_State,_Neighbours) ->
  Ctrl ! {done, self()};
cell(N, Ctrl, State, Neighbours) ->
  multicast(State, Neighbours),
  Alive_neighbours = collect2(Neighbours),
  NextState = rule(Alive_neighbours, State),
  cell(N-1,Ctrl,NextState,Neighbours).

% Sends a message to each neighbour, notifying them of its state
multicast(State, Neighbours) ->
  Self = self(),
  lists:foreach(fun(Pid) -> Pid ! {state, Self, State} end, Neighbours).

% Collects all messages sent by a cell's neighbours
collect(Neighbours) ->
  lists:map(fun(Pid) ->
    receive
      {state, Pid, State} -> State
    end
            end, Neighbours).

%Better collection function, returns number of alive neighbour cells
collect2(Neighbours) ->
  lists:foldl(fun(Pid,Sum) ->
    receive
      {state,Pid,alive} -> Sum +1;
      {state, Pid, dead} -> Sum
    end
              end, 0,Neighbours).

% Game rules
rule(Neighbours, State) ->
  if
    Neighbours < 2 -> dead;
    Neighbours == 2 -> State;
    Neighbours == 3 -> alive;
    Neighbours > 3 -> dead
  end.