defmodule Sort do

  @doc """
    iex> Sort.bubble([6])
    [6]

    iex> Sort.bubble([1,2])
    [1,2]

    iex> Sort.bubble([2,1])
    [1,2]

    iex> Sort.bubble([2,1,3])
    [1,2,3]

    iex> Sort.bubble([6,2,5,3,4,1])
    [1,2,3,4,5,6]

    iex> Sort.bubble([1,2,3,4,5,6])
    [1,2,3,4,5,6]

    iex> Sort.bubble([1,3,2,3,3,5,6,3])
    [1,2,3,3,3,3,5,6]

  """
  @spec bubble([...]) :: list
  def bubble([h|t]) do
    bubble_helper([h], t, false)
  end

  defp bubble_helper(list, [], false), do: list
  defp bubble_helper([h|rest] = list, [t|tail_rest], switched) do
    case {switched, tail_rest} do
      {false, []} when h < t -> [t|list] |> Enum.reverse()
      {true, []} when h < t -> [t|list] |> Enum.reverse() |> bubble()
      {_, []} when h > t -> bubble_helper([t|rest],[h], true)
      _ when h < t -> bubble_helper([t|list], tail_rest, switched)
      _ when h > t -> bubble_helper([t|rest], [h|tail_rest], true)
      _ when h === t -> bubble_helper([t|list], tail_rest, switched)
    end
  end
end
