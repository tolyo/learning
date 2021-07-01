defmodule Set do

  @type element :: any()
  @type tree :: element() | {tree(), element(), tree()}

  @spec member(any, tree()) :: boolean
  @doc """
    iex> Set.member(1, {1, 2, 3})
    true
  """
  def member(x, {a, y, b}) do
    cond do
      x < y -> member(x, a)
      x > y -> member(x, b)
      x === y -> true
    end
  end
  def member(a, a), do: true
  def member(_, _), do: false

  def insert(x, nil), do: {nil,  x, nil}
  def insert(x, {a, y, b} = s) do
    cond do
      x < y -> {insert(x, a), y, b}
      x > y -> {a, y, insert(x, b)}
      x == y -> s
    end
  end

end
