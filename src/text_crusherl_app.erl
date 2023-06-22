%%%-------------------------------------------------------------------
%% @doc text_crusherl public API
%% @end
%%%-------------------------------------------------------------------

-module(text_crusherl_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  text_crusherl_sup:start_link().

stop(_State) ->
  ok.

%% internal functions
