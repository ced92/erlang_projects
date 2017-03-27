%%%-------------------------------------------------------------------
%%% @author cedricseger
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2017 01:20
%%%-------------------------------------------------------------------
-module(test).
-author("cedricseger").

%% API
-export([demo/0]).

demo() ->
  small_image(-0.5,1,0.5),
  large_image(-0.01,0.8,0.64). %Window size -> Used for zooming and viewing different parts of the mandelbrot

% C optimization, sequential logic
small_image(X,Y,X1) ->
  Width = 800,
  Height = 600,
  K = (X1 - X)/Width,
  Depth = 200,
  T0 = now(),
  Image = mandel:mandelbrot(Width, Height ,X, Y, K, Depth),
  T = timer:now_diff(now(), T0),
  io:format("picture generated in ~w ms~n", [T div 1000]),
  ppm:write("small_op.ppm", Image).

% C optimization, parallel logic
large_image(X,Y,X1) ->
  Width = 2900,
  Height = 1500,
  K = (X1 - X)/Width,
  Depth = 1000,
  T0 = now(),
  Image = mandel:mandelbrot_parallel(Width, Height ,X, Y, K, Depth),
  T = timer:now_diff(now(), T0),
  io:format("picture generated in ~w ms~n", [T div 1000]),
  ppm:write("large.ppm", Image).