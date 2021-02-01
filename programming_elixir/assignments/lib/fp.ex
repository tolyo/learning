defmodule FP do

  @moduledoc """
   Functional programming in Scala to Elixir
  """

  @doc """

  ## Examples

      iex> FP.fib(3)
      6
      iex> FP.fib(4)
      10

  """
  @spec fib(number) :: number
  def fib(0), do: 0
  def fib(1), do: 1
  def fib(n), do: n + fib(n-1)

  @doc """
    Implement isSorted , which checks whether an Array[A] is sorted according to a
    given comparison function:

    def is_sorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean

    ## Examples

      iex> FP.is_sorted([1], fn (a, b) -> a < b end)
      true
      iex> FP.is_sorted([1,3,4], fn (a, b) -> a < b end)
      true
      iex> FP.is_sorted([1,3,4], fn (a, b) -> a > b end)
      false
      iex> FP.is_sorted([1,2,3,4,6,3,4], fn (a, b) -> a < b end)
      false
      iex> FP.is_sorted([1,6,3,4], fn (a, b) -> a > b end)
      false
      iex> FP.is_sorted(["a","c","d"], fn (a, b) -> a < b end)
      true
  """
  @spec is_sorted(list(a), ((b, b) -> boolean())) :: boolean() when a: any, b: a
  def is_sorted([], _), do: true
  def is_sorted([_head], _), do: true
  def is_sorted([head | [sub_head | _] = tail], ordered) do
    case ordered.(head, sub_head) do
      true ->
        is_sorted(tail, ordered)
      false ->
        false
    end
  end

  @doc """
    Let’s look at another example, currying, 9 which converts a function f of two arguments
    into a function of one argument that partially applies f . Here again there’s only one
    implementation that compiles. Write this implementation.

    def curry[A,B,C](f: (A, B) => C): A => (B => C)

    iex> res = FP.curry(fn(x, y) -> x + y end)
    iex> res.(4).(5)
    9

  """
  @spec curry(((a, b) -> c)) :: (a1 -> (b1 -> c1)) when a: any, b: any, c: any, a1: a, b1: b, c1: c
  def curry(f) do
    fn a -> (fn b -> f.(a, b) end) end
  end

  @doc """
  Implement uncurry , which reverses the transformation of curry . Note that since =>
  associates to the right, A => (B => C) can be written as A => B => C .
  def uncurry[A,B,C](f: A => B => C): (A, B) => C

    iex> res = FP.curry(fn(x, y) -> x + y end)
    iex> res.(4).(5)
    9
    iex> unres = FP.uncurry(res)
    iex> unres.(5,6)
    11

  """
  @spec uncurry((a -> (b -> c))) :: ((a1, b1) -> c1) when a: any, b: any, c: any, a1: a, b1: b, c1: c
  def uncurry(f) do
    fn (a, b) -> f.(a).(b) end
  end

  @doc """
    Implement the higher-order function that composes two functions.
    def compose[A,B,C](f: B => C, g: A => B): A => C

    iex> res = FP.compose(fn(x) -> x + x end, fn(y) -> y * y end)
    iex> res.(2)
    8
  """
  @spec compose((b -> c), (a -> b1)) :: ((a1) -> c1) when a: any, a1: a, b: any, b1: b, c: any, c1: c
  def compose(f, b) do
    fn x -> f.(b.(x)) end
  end
end
