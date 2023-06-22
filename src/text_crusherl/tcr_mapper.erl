-module(tcr_mapper).

-export([start_link/1, run_mapper/2]).

-define(CHUNK_SIZE, 1024).
-define(SPACE_SYMBOLS, [<<"\r\n">>, <<"\r">>, <<"\n">>, <<"\t">>, <<" ">>]).

-spec start_link(File :: string()) -> pid().
start_link(File) ->
  spawn_link(?MODULE, run_mapper, [self(), File]).

-spec run_mapper(Parent :: pid(), File :: string()) -> ok.
run_mapper(Parent, File) ->
  case file:open(File, [read, raw, binary]) of
    {ok, FileDevice} ->
      loop(FileDevice, File, Parent, <<>>, #{});
    Error ->
      tcr_reducer:reply(Parent, File, Error)
  end.

loop(FileDevice, File, Parent, Buffer, Words) ->
  case file:read(FileDevice, ?CHUNK_SIZE) of
    eof when Buffer == <<>> ->
      tcr_reducer:reply(Parent, File, {ok, Words});
    eof ->
      Words1 = add_word(Buffer, Words),
      tcr_reducer:reply(Parent, File, {ok, Words1});
    {error, _} = Error ->
      tcr_reducer:reply(Parent, File, Error);
    {ok, Data} ->
      {Buffer1, Words1} = count_words(<<Buffer/binary, Data/binary>>, Words),
      Buffer2 = binary:copy(Buffer1),
      loop(FileDevice, File, Parent, Buffer2, Words1)
  end.

count_words(<<>>, Words) ->
  {<<>>, Words};
count_words(Buffer, Words) ->
  case binary:split(Buffer, ?SPACE_SYMBOLS) of
    [<<>>] ->
      {<<>>, Words};
    [Word] ->
      {Word, Words};
    [Word, <<>>] ->
      {<<>>, add_word(Word, Words)};
    [Word, Tail] ->
      count_words(Tail, add_word(Word, Words))
  end.

add_word(Word, Words) ->
  Word1 = binary:copy(Word),
  Count = maps:get(Word1, Words, 0),
  Words#{Word1 => Count + 1}.
