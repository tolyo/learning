-module(tic_tac_toe).

-export([new_game/0, win/1, move/3]).

new_game() -> {{f, f, f}, {f, f, f}, {f, f, f}}.

win(GameState) ->
  case GameState of
    %% horizontal checks
    {{x, x, x}, _, _} -> {win, x};
    {_, {x, x, x}, _} -> {win, x};
    {_, _, {x, x, x}} -> {win, x};
    {{o, o, o}, _, _} -> {win, o};
    {_, {o, o, o}, _} -> {win, o};
    {_, _, {o, o, o}} -> {win, o};

    %% vertical checks
    {{x, _, _}, {x, _, _}, {x, _, _}} -> {win, x};
    {{_, x, _}, {_, x, _}, {_, x, _}} -> {win, x};
    {{_, _, x}, {_, _, x}, {_, _, x}} -> {win, x};
    {{o, _, _}, {o, _, _}, {o, _, _}} -> {win, o};
    {{_, o, _}, {_, o, _}, {_, o, _}} -> {win, o};
    {{_, _, o}, {_, _, o}, {_, _, o}} -> {win, o};

    %% diagonal checks
    {{x, _, _}, {_, x, _}, {_, _, x}} -> {win, x};
    {{_, _, x}, {_, x, _}, {x, _, _}} -> {win, x};
    {{o, _, _}, {_, o, _}, {_, _, o}} -> {win, o};
    {{_, _, o}, {_, o, _}, {o, _, _}} -> {win, o};

    % default
    _ -> no_win
  end.

move(Cell, Player, GameState) ->
  {{F1, F2, F3}, {F4, F5, F6}, {F7, F8, F9}} = GameState,
  case GameState of
    {{f, _, _}, _, _} when Cell =:= 1 -> {ok, {{Player, F2, F3}, {F4, F5, F6}, {F7, F8, F9}}};
    {{_, f, _}, _, _} when Cell =:= 2 -> {ok, {{F1, Player, F3}, {F4, F5, F6}, {F7, F8, F9}}};
    {{_, _, f}, _, _} when Cell =:= 3 -> {ok, {{F1, F2, Player}, {F4, F5, F6}, {F7, F8, F9}}};
    {_, {f, _, _}, _} when Cell =:= 4 -> {ok, {{F1, F2, F3}, {Player, F5, F6}, {F7, F8, F9}}};
    {_, {_, f, _}, _} when Cell =:= 5 -> {ok, {{F1, F2, F3}, {F4, Player, F6}, {F7, F8, F9}}};
    {_, {_, _, f}, _} when Cell =:= 6 -> {ok, {{F1, F2, F3}, {F4, F5, Player}, {F7, F8, F9}}};
    {_, _, {f, _, _}} when Cell =:= 7 -> {ok, {{F1, F2, F3}, {F4, F5, F6}, {Player, F8, F9}}};
    {_, _, {_, f, _}} when Cell =:= 8 -> {ok, {{F1, F2, F3}, {F4, F5, F6}, {F7, Player, F9}}};
    {_, _, {_, _, f}} when Cell =:= 9 -> {ok, {{F1, F2, F3}, {F4, F5, F6}, {F7, F8, Player}}};
    _ -> {error,invalid_move}
  end.
