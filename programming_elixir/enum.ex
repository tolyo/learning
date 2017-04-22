defmodule MyEnum do

  def all?([], func) do
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
end