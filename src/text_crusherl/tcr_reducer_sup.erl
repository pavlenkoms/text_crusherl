-module(tcr_reducer_sup).

-behaviour(supervisor).

-export([start_link/0, start_reducer/1]).
-export([init/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_reducer(FileList :: [string()]) -> supervisor:startchild_ret().
start_reducer(FileList) ->
  supervisor:start_child(?MODULE, [self(), FileList]).

init(_Args) ->
  SupFlags =
    #{strategy => simple_one_for_one,
      intensity => 0,
      period => 1},
  ChildSpecs =
    [#{id => call,
       start => {tcr_reducer, start_link, []},
       shutdown => brutal_kill,
       restart => temporary}],
  {ok, {SupFlags, ChildSpecs}}.
