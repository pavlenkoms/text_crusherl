-module(tcr_runner).

-export([crush/1, reply/2]).

-define(TIMEOUT, 60000).

-export_type([reply/0]).

-type reply() ::
  {ok, Words :: #{bitstring() => non_neg_integer()}, Errors :: [{error, term()}]} |
  {error, term()}.

-spec crush(FileList :: [string()]) -> reply().
crush(FileList) ->
  {ok, Pid} = tcr_reducer_sup:start_reducer(FileList),
  Ref = monitor(process, Pid),
  receive
    {'DOWN', Ref, _, _, Info} ->
      {error, Info};
    {?MODULE, {ok, _TotalWords, _Errors} = Resp} ->
      demonitor(Ref, [flush]),
      Resp;
    {?MODULE, {error, _} = Error} ->
      demonitor(Ref, [flush]),
      Error
  after ?TIMEOUT ->
    {error, timeout}
  end.

-spec reply(From :: pid(), Resp :: reply()) -> {?MODULE, reply()}.
reply(From, Resp) ->
  From ! {?MODULE, Resp}.
