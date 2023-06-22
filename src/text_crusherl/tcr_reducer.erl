-module(tcr_reducer).

-behaviour(gen_server).

-record(state,
        {process_map = #{} :: #{pid() => string()},
         results = #{} :: #{string() => #{binary() => non_neg_integer()}},
         from :: pid()}).

-export([start_link/2, reply/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2,
         terminate/2]).

-type reply() :: {ok, Words :: #{bitstring() => non_neg_integer()}} | {error, term()}.

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link(From :: pid(), FileList :: [string()]) -> gen_server:start_ret().
start_link(From, FileList) ->
  gen_server:start_link(?MODULE, [From, FileList], []).

-spec reply(Reducer :: pid(), File :: string(), Result :: reply()) -> ok.
reply(Reducer, File, Result) ->
  gen_server:cast(Reducer, {result, File, Result}).

%% ===================================================================
%% GenServer callbacks
%% ===================================================================

init([From, FileList]) ->
  process_flag(trap_exit, true),
  {ok, #state{from = From}, {continue, FileList}}.

handle_continue(FileList, State) ->
  ProcessMap =
    lists:foldl(fun(File, Acc) ->
                   Pid = tcr_mapper:start_link(File),
                   Acc#{Pid => File}
                end,
                #{},
                FileList),

  {noreply, State#state{process_map = ProcessMap, results = #{}}}.

handle_call(_, _From, State) ->
  {reply, undefined, State}.

handle_cast({result, File, Result}, State) ->
  process_result(File, Result, State).

handle_info({'EXIT', _Pid, normal}, State) ->
  {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
  #{Pid := File} = State#state.process_map,
  process_result(File, {error, Reason}, State).

terminate(_, _State) ->
  ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

process_result(File, Result, State) ->
  Results = maps:put(File, Result, State#state.results),
  State1 = State#state{results = Results},
  calculate(State1).

calculate(#state{results = Results, process_map = ProcessMap} = State)
  when map_size(Results) == map_size(ProcessMap) ->
  {TotalWords, Errors} =
    maps:fold(fun (_, {ok, Words}, {TotalWords, Errors}) ->
                    {maps:merge_with(fun map_merge/3, TotalWords, Words), Errors};
                  (File, Error, {TotalWords, Errors}) ->
                    {TotalWords, [{File, Error} | Errors]}
              end,
              {#{}, []},
              Results),
  tcr_runner:reply(State#state.from, {ok, TotalWords, Errors}),
  {stop, normal, State};
calculate(State) ->
  {noreply, State}.

map_merge(_Key, Value1, Value2) ->
  Value1 + Value2.
