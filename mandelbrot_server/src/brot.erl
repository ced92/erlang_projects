%%%-------------------------------------------------------------------
%%% @author cedricseger
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Feb 2017 23:52
%%%-------------------------------------------------------------------
-module(brot).
-author("cedricseger").

%% API
-export([mandelbrot/2]).

% Finds depth of image given a point in complex plane and maxDepth
mandelbrot(ComplexNumber, MaxIter) ->
  Z0 = cmplx_optimized:new(0,0),
  I = 0,
  test_depth(I,Z0,ComplexNumber,MaxIter).

test_depth(Iteration,_Z,_ComplexNumber,MaxIter) when Iteration > MaxIter ->
  0;
test_depth(Iteration,Z,ComplexNumber,MaxIter) ->
  Abs = cmplx_optimized:abs1(Z),
  if
    Abs >= 2 -> Iteration;
    Abs < 2 ->
      ZNew = cmplx_optimized:add(cmplx_optimized:sqr(Z),ComplexNumber),
      test_depth(Iteration+1,ZNew,ComplexNumber,MaxIter)
  end.




