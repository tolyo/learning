%%%-------------------------------------------------------------------
%%% Define an Erlang function double/1 to double the elements of a list
%%% of numbers.
%%%-------------------------------------------------------------------
%%% Define a function evens/1 that extracts the even numbers from a
%%% list of integers.
%%%-------------------------------------------------------------------
-module(funlist).

%% API
-export([double/1, evens/1, median/1, sizelist/1, nth/2, modes/1, remove/2, count/2, compare/2]).


double([]) -> [];
double([H | T]) -> [H * 2 | double(T)].

evens([]) -> [];
evens([H | T]) when H rem 2 == 0 -> [ H | evens(T) ];
evens([_ | T]) -> evens(T).

nth([H | _], X) when X == 0 -> H;
nth([_ | T], X) -> nth(T, X - 1).

sizelist([]) -> 0;
sizelist([_ | T]) -> 1 + sizelist(T).

median(X, N) when N rem 2 == 1 ->
  nth(X, trunc(N / 2));

median(X, N)  ->
  M = trunc(N / 2),
  (nth(X, M - 1) + nth(X, M)) / 2.

median([]) -> [];
median(X) -> median(X, sizelist(X)).

count([], _X) -> 1;
count([H | T], X) when H == X -> 1 + count(T, X);
count([_ | T], X) -> count(T, X).

remove([], _X) -> [];
remove([H | T], X) when H == X -> remove(T, X);
remove([H | T], X) -> [ H | remove(T, X)].

compare({X, Y}, {_, W}) when Y > W -> {X, Y};
compare({_, _}, {X, Y}) -> {X, Y}.

getmax(A, []) -> A;
getmax(A, [H | T]) -> getmax(compare(A, H), T);
getmax(A, B) -> compare(A, B).

getmax([H | T]) -> getmax(H, T).

modesRec([]) -> [];
modesRec([H | T]) ->
  [{H, count(T, H)} | modesRec(remove(T, H))].

getModes([], _) -> [];
getModes([{X, Y} | T], A) when Y == A -> [ X | getModes(T, A)];
getModes([_| T], A) -> getModes(T, A).

modes(A) ->
  X = modesRec(A),
  {_,D} = getmax(X),
  getModes(X, D).
