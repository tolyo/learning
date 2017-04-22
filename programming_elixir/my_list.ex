defmodule MyList do
  @moduledoc false

  def max(list) do
    _max(list, 0)
  end

  defp _max([], val) do
    val
  end

  defp _max([ head | tail], val) when head > val do
    _max(tail, head)
  end
/
  defp _max([ _ | tail], val) do
    _max(tail, val)
  end

  def caesar([], _n), do: []

  def caesar([ head | tail ], n) when head + n <= ?z do
    [ head + n | caesar(tail, n) ]
  end

   def caesar([ head | tail ], n) do
      [ head + n - 26 | caesar(tail, n) ]
   end

   def span(from, from) do
     [from]
   end

   def span(from, to) do
      [ from | span(from + 1, to) ]
   end
end