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
    _bubble([h], t, false)
  end

  defp _bubble(list, [], false), do: list
  defp _bubble([h|rest] = list, [t|tail_rest], switched) do
    case {switched, tail_rest} do
      {false, []} when h < t -> [t|list] |> Enum.reverse()
      {true, []} when h < t -> [t|list] |> Enum.reverse() |> bubble()
      {_, []} when h > t -> _bubble([t|rest],[h], true)
      _ when h < t -> _bubble([t|list], tail_rest, switched)
      _ when h > t -> _bubble([t|rest], [h|tail_rest], true)
      _ when h === t -> _bubble([t|list], tail_rest, switched)
    end
  end

  @doc """
    iex> Sort.selection([6])
    [6]

    iex> Sort.selection([1,2])
    [1,2]

    iex> Sort.selection([2,1])
    [1,2]

    iex> Sort.selection([2,1,3])
    [1,2,3]

    iex> Sort.selection([6,2,5,3,4,1])
    [1,2,3,4,5,6]

    iex> Sort.selection([1,2,3,4,5,6])
    [1,2,3,4,5,6]

    iex> Sort.selection([1,3,2,3,3,5,6,3])
    [1,2,3,3,3,3,5,6]

  """
  @spec selection([any]) :: list()
  def selection(list) do
    _selection(list, [])
  end

  defp _selection([], acc), do: acc |> Enum.reverse()
  defp _selection(list, acc) do
    min = min(list)
    list
    |> remove_first(min)
    |> _selection([min|acc])
  end


  @doc """
    iex> Sort.min([8,4,2,3])
    2
  """
  @spec min(list) :: any
  def min(list) do
    _min(list, nil)
  end

  defp _min(list, val) do
    case {list, val} do
      {[], nil} -> nil
      {[], val} -> val
      {[h|t], nil} -> _min(t, h)
      {[h|t], val} ->
        case val < h do
          true -> _min(t, val)
          false -> _min(t, h)
        end
    end
  end

  @doc """
    iex> Sort.remove_first([8,4,4,2,4,3], 4)
    [8,4,2,4,3]

    iex> Sort.remove_first([8,4,4,2,4,3], 8)
    [4,4,2,4,3]

    iex> Sort.remove_first([8,4,4,2,4,3], 3)
    [8,4,4,2,4]
  """
  def remove_first(list, item) do
    case {list, item} do
      {[], _} -> []
      {[h|t], item} when h === item -> t
      {[h|t], item} -> [h|remove_first(t, item)]
    end
  end

end
