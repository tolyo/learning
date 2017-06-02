%%%-------------------------------------------------------------------
%%% @author aostrovsky
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% Нужно реализовать функцию champ_stat:get_stat/1, которая на вход принимает структуру данных, о
%%% писывающую чемпионат и на выходе отдает кортеж:
%%    {NumTeams, NumPlayers, AvgAge, AvgRating}
%%% @end
%%% Created : 02. Jun 2017 14:28
%%%-------------------------------------------------------------------
-module(champ_stat).
-import(lists, [foldl/3, flatmap/2]).
-export([get_stat/1]).
-include_lib("eunit/include/eunit.hrl").


get_stat(Champ) ->
  NumTeams = length(Champ),
  NumPlayers = foldl(fun({_, _, Players}, Sum) -> length(Players) + Sum end, 0, Champ),
  AllPlayers = flatmap(fun({_, _, Players}) -> Players end, Champ),
  Ages = lists:map(fun(X) -> get_age(X) end, AllPlayers),
  AvgAge =  lists:sum(Ages) / NumPlayers,
  AvgRating = foldl(fun(X, Sum) -> get_rating(X) + Sum end, 0, AllPlayers) / NumPlayers,
  {NumTeams, NumPlayers, AvgAge, AvgRating}.

%% {player, Name, Age, Rating, Health}
get_age({_, _, Age, _, _}) -> Age.
get_rating({_, _, _, Rating, _}) -> Rating.

get_stat_test() ->
  ?assertEqual({5,40,24.85,242.8}, get_stat(champ:sample_champ())),
  ok.

