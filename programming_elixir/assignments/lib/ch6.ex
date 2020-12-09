defmodule Times do
  def double(n), do: n * 2
  def triple(n), do: n * 3
  def quadruple(n), do: double(n) + double(n)
end

defmodule Ch6 do

  @doc """
    Implement and run a function sum(n) that uses recursion to calculate the
    sum of the integers from 1 to n. You’ll need to write this function inside
    a module in a separate file. Then load up IEx, compile that file, and try
    your function.
  """
  def sum(0), do: 0
  def sum(1), do: 1
  def sum(n) when is_integer(n) and n > 0, do: n + sum(n-1)

  @doc """
    Write a function gcd(x,y) that finds the greatest common divisor between
    two nonnegative integers. Algebraically, gcd(x,y) is x if y is zero; it’s gcd(y,
    rem(x,y)) otherwise.
  """
  def gcd(x,0), do: x
  def gcd(x, y), do: gcd(y, rem(x, y))
end


defmodule Chop do
    @docmodule """
    I’m thinking of a number between 1 and 1000....
    The most efficient way to find the number is to guess halfway between
    the low and high numbers of the range. If our guess is too big, then the
    answer lies between the bottom of the range and one less than our guess.
    If our guess is too small, then the answer lies between one more than our
    guess and the end of the range.
    Your API will be guess(actual, range) , where range is an Elixir range. Your
    output should look similar to this:
    iex> Chop.guess(273, 1..1000)
    Is it 500
    Is it 250
    Is it 375
    Is it 312
    Is it 281
    Is it 265
    Is it 273
    273
    """
    def guess(ans, a..b) when is_integer(ans), do: helper(ans, div((b-a), 2) + a, a..b)
    defp helper(ans, guess, a..b) when ans > guess, do: guess(ans, guess..b)
    defp helper(ans, guess, a..b) when ans < guess, do: guess(ans, a..guess)
    defp helper(ans, guess, _) when ans === guess, do: guess
end
