defmodule Chop do

  def guess(a, b) do
    from..to = b
    guess = from + div((to - from), 2)
    IO.puts "Is it #{guess}"
    helper(a, guess, b)
  end

  def helper(actual, guess, b) when actual === guess do
    guess
  end


  def helper(actual, guess, b) when actual < guess do
    from.._ = b
    guess(actual, from..guess)
  end

  def helper(actual, guess, b) when actual > guess do
    _..to = b
    guess(actual, guess..to)
  end

end