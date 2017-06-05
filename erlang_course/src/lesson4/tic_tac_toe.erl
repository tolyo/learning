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


%% Реализовать функцию tic_tac_toe:move/3, которая на вход принимает номер ячейки, знак игрока (крестик или нолик) и игровое поле.
%% И возвращает либо обновленное игровое поле, где данный игрок сделал ход в данную ячейку, либо ошибку, если такой ход невозможен.
%% Ячейки нумеруются так:
%%   {{1,2,3},
%%    {4,5,6},
%%    {7,8,9}}
%% Например, в начальной позиции, когда все ячейки свободны, игрок ставит крестик в первую ячейку:
%%    2> G = tic_tac_toe:new_game().
%%    {{f,f,f},{f,f,f},{f,f,f}}
%%    3> tic_tac_toe:move(1, x, G).
%%    {ok,{{x,f,f},{f,f,f},{f,f,f}}}
%% Функция возвращает кортеж {ok, NewGameState}, где крестик стоит в первой ячейке.
%% А если игрок пытается поставить крестик в ячейку, которая уже занята, то функция возвращает {error, invalid_move}:
%%    2> G1 = {{f,f,f},{f,x,f},{o,f,f}}.
%%    {{f,f,f},{f,x,f},{o,f,f}}
%%    3> tic_tac_toe:move(5, o, G1).
%%    {error,invalid_move}
%% Пример полного сеанса игры:
%%    2> G = tic_tac_toe:new_game().
%%    {{f,f,f},{f,f,f},{f,f,f}}
%%    3> tic_tac_toe:move(1, x, G).
%%    {ok,{{x,f,f},{f,f,f},{f,f,f}}}
%%    4> {ok, G2} = tic_tac_toe:move(1, x, G).
%%    {ok,{{x,f,f},{f,f,f},{f,f,f}}}
%%    5> {ok, G3} = tic_tac_toe:move(2, o, G2).
%%    {ok,{{x,o,f},{f,f,f},{f,f,f}}}
%%    6> {ok, G4} = tic_tac_toe:move(4, x, G3).
%%    {ok,{{x,o,f},{x,f,f},{f,f,f}}}
%%    7> {ok, G5} = tic_tac_toe:move(7, x, G4).
%%    {ok,{{x,o,f},{x,f,f},{x,f,f}}}
%%    8> tic_tac_toe:win(G5).
%%    {win,x}
%% Определения функций даны в модуле tic_tac_toe, реализация за вами.
%% Тесты на сей раз вынесены в отдельный модуль tic_tac_toe_test. Их не возбраняется смотреть, но не нужно модифицировать :)
move(Cell, Player, GameState) -> {Cell, Player, GameState}.
%% BEGIN (write your solution here)

%% END
