%%-------------------------------------------------------------------
%% @author anatoly
%% @copyright (C) 2017, <COMPANY>
%% @doc Geometric functions
%%
%% @end
%% Created : 05. Aug 2017 08:35
%%-------------------------------------------------------------------
-module(geom).
-author("anatoly").

%% API
-export([area/2]).


%% @doc Calculate the area of a square
area(X, Y) ->
  A = X * Y,
  A.