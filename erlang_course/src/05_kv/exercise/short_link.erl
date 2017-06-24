-module(short_link).

-export([init/0, create_short/2, get_long/2, rand_str/1]).

%%% module API

init() ->
    %% init randomizer
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    random:seed({A,B,C}),
    State = { maps:new(), maps:new() },
    State.


create_short(LongLink, State) ->
  {ToMap, FromMap} = State,
  case maps:find(LongLink, ToMap) of
    {ok, Val} -> {Val, State};
    error ->
      ShortLink = rand_str(10),
      {ShortLink, {maps:put(LongLink, ShortLink, ToMap), maps:put(ShortLink, LongLink, FromMap)}}
  end.

get_long(ShortLink, State) ->
  {_, FromMap} = State,
    case maps:find(ShortLink, FromMap) of
      {ok, Val} -> {ok, Val};
      error -> {error, not_found}
    end.


%% generates random string of chars [a-zA-Z0-9]
rand_str(Length) ->
    lists:map(fun(Char) when Char > 83 -> Char + 13;
                 (Char) when Char > 57 -> Char + 7;
                 (Char) -> Char
              end,
              [crypto:rand_uniform(48, 110) || _ <- lists:seq(1, Length)]).
