defmodule MyEnum do

  def all?([], _) do
    false
  end

  def all?([ head ], func) do
    func.(head)
  end

  def all?([ head | tail ], func) do
    if (func.(head)) do
      all?(tail, func)
    else
      false
    end
  end

  def each([ head | tail ], func) do
    [ func.(head) | each(tail, func) ]
  end

  def each([head], func) do
    [ func.(head) ]
  end

  def each([], _) do
    []
  end

  def filter([ head | tail ], func) do
     if func.(head) do
       [ head | filter(tail, func)]
     else
       filter( tail , func)
     end
  end

  def filter([head], func) do
    if func.(head) do
      [ head ]
    else
      []
    end
  end

  def filter([], _) do
    []
  end

  def split(list, sep) do
    _split(list, sep, [])
  end

  def _split([ head | tail], sep, acc) do
    if head === sep do
      {[ acc | [head]], tail}
    else
      _split(tail, sep, acc ++ head)
    end
  end

  def take(list, sep) do
    _take(list, sep, 0)
  end

  def _take([ head | tail ], sep, count) do
    if sep === count do
      []
    else
      [ head | _take(tail, sep, count + 1) ]
    end
  end

  def _take([], sep, count), do: []

end