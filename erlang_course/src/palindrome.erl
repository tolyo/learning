%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
-module(palindrome).

%% API
-export([server/1]).


rem_punct(String) ->
  lists:filter(fun (Ch) -> not(lists:member(Ch,"\"\'\t\n ")) end, String).

to_small(String) -> lists:map(fun(Ch) ->
  case ($A =< Ch andalso Ch =< $Z) of
    true -> Ch+32;
    false -> Ch
  end
                              end,
  String).

palindrome_check(String) ->
  Normalise = to_small(rem_punct(String)),
  lists:reverse(Normalise) == Normalise.

server(Pid) ->
  receive
    stop -> io:format("stopping server ~n");
    Msg ->
      io:format("Incoming message ~s~n", [Msg]),
      Pid ! {result, palindrome_check(Msg)},
      server(Pid)
  end.
