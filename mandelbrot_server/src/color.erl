%%%-------------------------------------------------------------------
%%% @author cedricseger
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2017 00:27
%%%-------------------------------------------------------------------
-module(color).
-author("cedricseger").

%% API
-export([convert/2]).

%Converts depth into RGB color
convert(Depth,MaxDepth) ->
  Frac = Depth / MaxDepth,
  Float = 10 * Frac, %A
  X = trunc(Float), %X
  Y = trunc(255*(Float-X)),
  case X of
    0 -> {Y,50,255};
    1 -> {80,255-Y,47};
    2 -> {255-Y,0,229};
    3 -> {10,89,255-Y};
    4 -> {215,30,255-Y};
    5 -> {255,142,255-Y};
    6 -> {Y,120,255-Y};
    7 -> {255-Y,215,255-Y};
    8 -> {255-Y,60,Y};
    9 -> {60,10,255-Y};
    10 -> {21,25,110}
  end.
