-module(tcr_reducer_tests).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
  #{}.

stop(_) ->
  meck:unload().

create_test_() ->
  [{"sucess", ?setup(fun success/1)},
   {"sucess one error", ?setup(fun success_one_error/1)},
   {"sucess one exit", ?setup(fun success_one_exit/1)}].

success(_) ->
  meck:new(file, [unstick]),
  meck:expect(tcr_mapper,
              start_link,
              fun ("file1" = File) ->
                    Self = self(),
                    spawn_link(fun() -> tcr_reducer:reply(Self, File, {ok, #{<<"a">> => 1}}) end);
                  ("file2" = File) ->
                    Self = self(),
                    spawn_link(fun() ->
                                  tcr_reducer:reply(Self, File, {ok, #{<<"a">> => 2, <<"b">> => 1}})
                               end)
              end),
  meck:expect(tcr_runner, reply, fun(From, Result) -> From ! {test_result, Result} end),

  tcr_reducer:start_link(self(), ["file1", "file2"]),

  Result =
    receive
      R ->
        R
    after 10000 ->
      {error, test_timeout}
    end,

  Expected = {ok, #{<<"a">> => 3, <<"b">> => 1}, []},
  ExpectedResp = {test_result, Expected},
  Self = self(),
  [?_assertEqual(Result, ExpectedResp),
   ?_assertEqual(meck:num_calls(tcr_runner, reply, [Self, Expected]), 1)].

success_one_error(_) ->
  meck:new(file, [unstick]),
  meck:expect(tcr_mapper,
              start_link,
              fun ("file1" = File) ->
                    Self = self(),
                    spawn_link(fun() -> tcr_reducer:reply(Self, File, {error, some_error}) end);
                  ("file2" = File) ->
                    Self = self(),
                    spawn_link(fun() ->
                                  tcr_reducer:reply(Self, File, {ok, #{<<"a">> => 2, <<"b">> => 1}})
                               end)
              end),
  meck:expect(tcr_runner, reply, fun(From, Result) -> From ! {test_result, Result} end),

  tcr_reducer:start_link(self(), ["file1", "file2"]),

  Result =
    receive
      R ->
        R
    after 10000 ->
      {error, test_timeout}
    end,

  Expected = {ok, #{<<"a">> => 2, <<"b">> => 1}, [{"file1", {error, some_error}}]},
  ExpectedResp = {test_result, Expected},
  Self = self(),
  [?_assertEqual(Result, ExpectedResp),
   ?_assertEqual(meck:num_calls(tcr_runner, reply, [Self, Expected]), 1)].

success_one_exit(_) ->
  meck:new(file, [unstick]),
  meck:expect(tcr_mapper,
              start_link,
              fun ("file1" = _File) ->
                    spawn_link(fun() -> exit(very_bad_reason) end);
                  ("file2" = File) ->
                    Self = self(),
                    spawn_link(fun() ->
                                  tcr_reducer:reply(Self, File, {ok, #{<<"a">> => 2, <<"b">> => 1}})
                               end)
              end),
  meck:expect(tcr_runner, reply, fun(From, Result) -> From ! {test_result, Result} end),

  tcr_reducer:start_link(self(), ["file1", "file2"]),

  Result =
    receive
      R ->
        R
    after 10000 ->
      {error, test_timeout}
    end,

  Expected = {ok, #{<<"a">> => 2, <<"b">> => 1}, [{"file1", {error, very_bad_reason}}]},
  ExpectedResp = {test_result, Expected},
  Self = self(),
  [?_assertEqual(Result, ExpectedResp),
   ?_assertEqual(meck:num_calls(tcr_runner, reply, [Self, Expected]), 1)].
