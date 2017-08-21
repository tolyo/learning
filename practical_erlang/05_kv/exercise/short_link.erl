-module(short_link).

-export([init/0, create_short/2, get_long/2, rand_str/1]).

%%% module API

init() ->
    %% init randomizer
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    random:seed({A,B,C}),
    State = #{},
    State.


create_short(LongLink, State) ->
    Exists = get_short(LongLink, State),
    case Exists of
        {ok, ShortLink} -> {ShortLink, State};
        _ ->
            ShortLink = rand_str(10),
            State1 = maps:put(ShortLink, LongLink, State),
            State2 = maps:put(LongLink, ShortLink , State1),
            {ShortLink, State2}
    end.


get_long(ShortLink, State) ->
    Result = maps:find(ShortLink, State),
    case Result of
        {ok, Val} -> {ok, Val};
        error -> {error, not_found}
    end.


get_short(LongLink, State) -> get_long(LongLink, State).


%% generates random string of chars [a-zA-Z0-9]
rand_str(Length) ->
    lists:map(fun(Char) when Char > 83 -> Char + 13;
                 (Char) when Char > 57 -> Char + 7;
                 (Char) -> Char
              end,
              [crypto:rand_uniform(48, 110) || _ <- lists:seq(1, Length)]).
