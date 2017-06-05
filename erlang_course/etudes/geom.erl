%%%-------------------------------------------------------------------
%%% @doc
%%% Functions for calculating areas of geometric shapes.
%%% @end
%%% Created : 05. Jun 2017 14:51
%%%-------------------------------------------------------------------
-module(geom).

%% API
-export([area/2]).

%% @doc Calculates the area of a rectangle.
%% Given height and length, returns a products of those values.
-spec(area(number(), number()) -> number()).

area(H, L) -> H * L.

