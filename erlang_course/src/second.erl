%% ------------------------------------------------------------------------------
%% %% Using the function square from first.erl, define a function
%% that gives the size of the hypotenuse of a
%% right-angled triangle given the lengths of the two other sides.
%% ------------------------------------------------------------------------------
%% Define functions that give the perimeter and area of a right-angled
%% triangle, given the lengths of the two short sides.
%% ------------------------------------------------------------------------------
%% Give a definition of the function maxThree which takes three integers and returns
%% the maximum of the three. You can use the max function, which gives the maximum of
%% two numbers, in writing your definition.
%%  maxThree(34,25,36) = 36
%% -----------------------------------------------------------------------------
%% Give a definition of the function howManyEqual which takes three integers and returns
%% an integer, counting how many of its three arguments are equal, so that
%%  howManyEqual(34,25,36) = 0
%%  howManyEqual(34,25,34) = 2
%%  howManyEqual(34,34,34) = 3:
%% -----------------------------------------------------------------------------
%% The Fibonacci sequence is given by 0, 1, 1, 2, 3, 5, … where subsequent values are
%% given by adding the two previous values in the sequence.
%% Give a recursive definition of the function fib/1 computing the Fibonacci numbers,
%% and give a step-by-step evaluation of fib(4).


-module(second).

-export([
  hypotenuse/2,
  perimeter/2,
  area/2,
  maxThree/3,
  howManyEqual/3,
  fib/1,
  pieces/1,
  fibTail/1,
  perfect/1,
  perimeter/1,
  bitsDirect/1,
  largestPower/1,
  bits/1
]).

-import(first, [double/1]).

hypotenuse(A, B) ->
  math:sqrt(double(A) + double(B)).

perimeter(A, B) ->
  A + B + hypotenuse(A, B).

area(A, B) ->
  (A * B) / 2.

maxThree(A, B, C) ->
  max(A, max(B, C)).

howManyEqual(A, A, A) -> 3;
howManyEqual(A, A, _) -> 2;
howManyEqual(A, _, A) -> 2;
howManyEqual(_, A, A) -> 2;
howManyEqual(_, _, _) -> 0.

fib(0) -> 0;
fib(1) -> 1;
fib(X) -> fib(X - 1) + fib(X - 2).

pieces(1) -> 2;
pieces(2) -> 4;
pieces(X) -> pieces(X - 1) + X.

fibTail(0, A, _) -> A;
fibTail(X, A, B) -> fibTail(X - 1, B, A + B).

fibTail(X) -> fibTail(X, 0, 1).

perfect(X, X, A) -> X == A;
perfect(X, I, A) when X rem I == 0 -> perfect(X, I + 1, A + I);
perfect(X, I, A) -> perfect(X, I + 1, A).

perfect(X) -> perfect(X, 1, 0).

%% Define a function perimeter/1 which takes a shape and returns the perimeter of the shape.
perimeter({square, B}) -> B * 4;
perimeter({rectangle, B, H}) -> (B + H) * 2;
perimeter({circle, R}) -> math:pi() * R * R;
perimeter({triangle, {X1, Y1}, {X2, Y2}, {X3, Y3}}) ->
  math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)) +
  math:sqrt(math:pow(X2 - X3, 2) + math:pow(Y2 - Y3, 2)) +
  math:sqrt(math:pow(X3 - X1, 2) + math:pow(Y3 - Y1, 2)).

bits(_, 0, A) -> A;
bits(X, Y, A) when X < Y -> bits(X, trunc(Y / 2), A);
bits(X, Y, A) -> bits(X - Y, trunc(Y / 2), A + 1).

bits(X) ->
  P = largestPower(X),
  bits(X, math:pow(2, P), 0).

bitsDirect(_X, 0) -> 0;

bitsDirect(X, Y) when X < Y ->
  bitsDirect(X, trunc(Y / 2));

bitsDirect(X, Y) ->
  1 + bitsDirect(X - Y, trunc(Y / 2)).

bitsDirect(X) ->
  P = largestPower(X),
  R = bitsDirect(X, math:pow(2, P)),
  R.

largestPower(X, P, A) when X/2 >= P -> largestPower(X, P * 2, A + 1);
largestPower(1, _, _) -> 0;
largestPower(0, _, _) -> 0;
largestPower(_, _, A) -> A.

largestPower(X) -> largestPower(X, 2, 1).


