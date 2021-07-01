defmodule Stack do

  @moduledoc """
    Stack imlpementation for Okasaki
  """
  @type t :: Cons.t() | Nil

  defmodule Cons do
    @type t :: %Cons{
      head: any(),
      tail: Stack.t()
    }
    defstruct head: nil,
              tail: nil
  end

  defmodule Nil do
    @type t :: nil
  end

  @spec isEmpty(Stack.t()) :: boolean
  def isEmpty(stack) do
    case stack do
      Nil -> true
      %Cons{} -> false
    end
  end

  @spec cons(any, any) :: Stack.Cons.t()
  def cons(x, s), do: %Cons{head: x, tail: s}

  @spec head(Stack.Cons.t()) :: any
  def head(stack) do
    case stack do
      Nil -> raise "Empty"
      %Cons{} -> stack.head
    end
  end

  @spec tail(Stack.t()) :: any
  def tail(stack) do
    case stack do
      Nil -> raise "Empty"
      %Cons{} -> stack.tail
    end
  end

  @spec concat(Stack.t(), Stack.t()) :: Stack.t()
  def concat(stack1, stack2) do
    case isEmpty(stack1) do
      true -> stack2
      false -> cons(stack1.head, concat(stack1.tail, stack2))
    end
  end

  @spec update(Stack.Cons.t(), non_neg_integer, any) :: Stack.Cons.t()
  def update(Nil, _, _), do: raise "Empty"
  def update(%Cons{head: _, tail: b}, 0, item), do: %Cons{head: item, tail: b}
  def update(%Cons{head: a, tail: b}, n, item), do: %Cons{head: a, tail: update(b, n-1, item)}

  @spec suffixes(list) :: [[...]]
  def suffixes([]), do: []
  def suffixes([h|t]), do: [[h|t]] ++ suffixes(t)
end
