-module(tcr_mapper_tests).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
  #{}.

stop(_) ->
  meck:unload().

create_test_() ->
  [{"sucess", ?setup(fun success/1)},
   {"error while reading", ?setup(fun error_while_reading/1)},
   {"error while opening", ?setup(fun error_while_opening/1)}].

success(_) ->
  meck:new(file, [unstick]),
  meck:expect(file, open, fun(_, _) -> {ok, fd} end),
  meck:sequence(file, read, 2, [{ok, <<"a b\r\nc">>}, {ok, <<"d\tcd\na ">>}, eof]),
  meck:expect(tcr_reducer, reply, fun(_, _, _) -> ok end),

  Parent = self(),
  Filename = "the filename",
  Expected =
    {ok,
     #{<<"a">> => 2,
       <<"b">> => 1,
       <<"cd">> => 2}},
  tcr_mapper:run_mapper(Parent, Filename),

  [?_assertEqual(meck:num_calls(tcr_reducer, reply, [Parent, Filename, Expected]), 1)].

error_while_reading(_) ->
  meck:new(file, [unstick]),
  meck:expect(file, open, fun(_, _) -> {ok, fd} end),
  meck:sequence(file,
                read,
                2,
                [{ok, <<"a b\r\nc">>}, {ok, <<"d\tcd\na ">>}, {error, some_posix_error}]),
  meck:expect(tcr_reducer, reply, fun(_, _, _) -> ok end),

  Parent = self(),
  Filename = "the filename",
  Expected = {error, some_posix_error},
  tcr_mapper:run_mapper(Parent, Filename),

  [?_assertEqual(meck:num_calls(tcr_reducer, reply, [Parent, Filename, Expected]), 1),
   ?_assertEqual(meck:num_calls(tcr_reducer, reply, ['_', '_', '_']), 1)].

error_while_opening(_) ->
  meck:new(file, [unstick]),
  meck:expect(file, open, fun(_, _) -> {error, some_posix_error} end),
  meck:expect(tcr_reducer, reply, fun(_, _, _) -> ok end),

  Parent = self(),
  Filename = "the filename",
  Expected = {error, some_posix_error},
  tcr_mapper:run_mapper(Parent, Filename),

  [?_assertEqual(meck:num_calls(tcr_reducer, reply, [Parent, Filename, Expected]), 1),
   ?_assertEqual(meck:num_calls(tcr_reducer, reply, ['_', '_', '_']), 1)].
