defmodule Ch7 do

  defmodule MyList do
    @moduledoc false

    def sum(list), do: _sum(list, 0)

    # private methods
    defp _sum([], total), do: total
    defp _sum([head|tail], total), do: _sum(tail, head+total)

    def map([], _func), do: []
    def map([head|tail], func), do: [ func.(head)|map(tail, func) ]

    def mapsum([], _), do: []
    def mapsum(list, func) do
      list
      |> map(func)
      |> sum
    end


    def max(list) do
      _max(list, 0)
    end

    defp _max([], val) do
      val
    end

    defp _max([head|tail], val) when head > val do
      _max(tail, head)
    end

    defp _max([_|tail], val) do
      _max(tail, val)
    end

    def caesar([], _n), do: []
    def caesar([head|tail], n) when head + n <= ?z do
      [head + n|caesar(tail, n) ]
    end

    def caesar([head|tail], n) do
      [head + n - 26|caesar(tail, n) ]
    end

    def span(from, from), do: [from]
    def span(from, to), do: [from | span(from + 1, to)]

  end
end
