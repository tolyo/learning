-module(tic_tac_toe).

-export([new_game/0, win/1, move/3]).

new_game() -> {{f, f, f}, {f, f, f}, {f, f, f}}.

%% Реализовать функцию tic_tac_toe:win/1, которая на вход принимает игровое поле и на выходе
%% выдает:
%%    {win, x}, если выиграли крестики;
%%    {win, o}, если выиграли нолики;
%%    no_win, если никто не выиграл.
%% Например:
%%    2> G1 = {{f,f,f},{f,x,f},{o,f,f}}.
%%            {{f,f,f},{f,x,f},{o,f,f}}
%%    3> tic_tac_toe:win(G1).
%%       no_win
%%    4> G2 = {{f,f,o},{x,x,x},{o,o,f}}.
%%             {{f,f,o},{x,x,x},{o,o,f}}
%%    5> tic_tac_toe:win(G2).
%%       {win,x}
%%    6> G3 = {{f,f,o},{x,x,o},{x,o,o}}.
%%       {{f,f,o},{x,x,o},{x,o,o}}
%%    7> tic_tac_toe:win(G3).
%%       {win,o}
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


move(Cell, Player, GameState) -> {Cell, Player, GameState}.
%% BEGIN (write your solution here)

%% END
