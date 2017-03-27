%%%-------------------------------------------------------------------
%%% @author cedricseger
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2017 02:52
%%%-------------------------------------------------------------------
-module(mandel).
-author("cedricseger").

%% API
-export([mandelbrot_parallel/6, mandelbrot/6]).


% Parallel implementation
mandelbrot_parallel(Width, Height, X, Y, K, MaxDepth) ->
  %Trans returns complex number for a pixel at W,H position
  Trans = fun(W,H) -> cmplx_optimized:new(X + K*(W-1),Y-K*(H-1)) end,
  start_jobs(Width,Height,Trans,MaxDepth),
  collect_responses(Height).

% Starts as many processes as rows in image for execution in parallel
start_jobs(_,-1,_,_) ->
  done;
start_jobs(Width,Height,Trans,MaxDepth) ->
  spawn_worker(Width,Height,Trans,MaxDepth),
  start_jobs(Width,Height-1,Trans,MaxDepth).

spawn_worker(Width,Height,Trans,MaxDepth) ->
  spawn(fun() -> compute(self(),Width,Height,Trans,MaxDepth) end).

compute_row(0,_Height,_Trans,_MaxDepth,Acc) ->
  Acc;
compute_row(Width,Height,Trans,MaxDepth,Acc) ->
  ComplexNumber = Trans(Width,Height),
  Depth = brot:mandelbrot(ComplexNumber,MaxDepth),
  Color = color:convert(Depth,MaxDepth),
  compute_row(Width-1,Height,Trans,MaxDepth,[Color|Acc]).

compute(Pid, Width, Height,Trans,MaxDepth) ->
  Row = compute_row(Width,Height,Trans,MaxDepth,[]),
  Pid ! {done, Height, Row}.

collect_responses(Height) when Height >= 0 ->
  receive
    {done,Height,Row_Data} ->
      [Row_Data | collect_responses(Height-1)]
  end;
collect_responses(_Height) ->
  [].



% Sequential implementation
mandelbrot(Width, Height, X, Y, K, MaxDepth) ->
  Trans = fun(W,H) -> cmplx_optimized:new(X + K*(W-1),Y-K*(H-1)) end,
  rows(Width,Height,Trans,MaxDepth,[]).

rows(_Width,Height,_Trans,_Depth,Acc) when Height < 0 ->
  Acc;
rows(Width,Height,Trans,MaxDepth,Acc) ->
  Row = compute_row(Width,Height,Trans,MaxDepth,[]),
  rows(Width,Height-1,Trans,MaxDepth,[Row|Acc]).
