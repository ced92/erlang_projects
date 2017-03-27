%%%-------------------------------------------------------------------
%%% @author cedricseger
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2017 10:30
%%%-------------------------------------------------------------------
-module(cmplx_optimized).
-author("cedricseger").

%% API
-export([init/0,new/2, add/2, sqr/1, abs1/1]).
-on_load(init/0).

init() ->
  ok = erlang:load_nif("./cmplx_optimized",0).

%Returns new complex number
new(X,Y) ->
  {X,Y}.

%Adds two complex number
add({Re,Im},{Re2,Im2}) ->
  {Re+Re2, Im+Im2}.

%Returns square of a complex number (optimized in C)
sqr({_Re,_Im}) ->
  exit(nif_library_not_loaded).


%Returns abs of a complex number (optimized in C)
abs1({_Re,_Im}) ->
  exit(nif_library_not_loaded).
