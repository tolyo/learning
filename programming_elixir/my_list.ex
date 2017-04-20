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

  defp _max([ head | tail], val) do
    _max(tail, val)
  end

end