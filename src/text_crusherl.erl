-module(text_crusherl).

-export([crush/1]).

-spec crush(FileList :: [string()]) ->
             {ok, Words :: #{bitstring() => non_neg_integer()}, Errors :: [{string(), term()}]} |
             {error, term()}.
crush([_ | _] = FileList) ->
  tcr_runner:crush(FileList);
crush(_FileList) ->
  {error, bagarg}.
