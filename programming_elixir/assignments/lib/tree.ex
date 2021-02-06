defmodule Tree do
  @moduledoc """
    sealed trait Tree[+A]
    case class Leaf[A](value: A) extends Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  """
  @type t :: Leaf.t() | Branch.t()

  defmodule Leaf do
    @type t :: %Leaf{
      value: any()
    }
    defstruct value: nil

    @spec new(any()) :: Leaf.t()
    def new(val), do: %Leaf{value: val}
  end

  defmodule Branch do
    @type t :: %Branch{
      left: Leaf.t(),
      right: Leaf.t()
    }
    defstruct left: nil, right: nil

    @spec new(any(), any()) :: Branch.t()
    def new(left, right), do: %Branch{left: left, right: right}
  end



  @doc """
    Write a function size that counts the number of nodes (leaves and branches) in a tree.
    iex> Tree.size(%Tree.Leaf{value: 1})
    1
    iex> Tree.size(%Tree.Branch{left: %Tree.Leaf{value: 1}, right: %Tree.Leaf{value: 1}})
    3
  """
  @spec size(Tree.t()) :: number()
  def size(%Leaf{}), do: 1
  def size(%Branch{} = b), do: 1 + Tree.size(b.left) + Tree.size(b.right)

  @doc """
    Write a function maximum that returns the maximum element in a Tree[Int] . (Note:
    In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x
    and y .)

    iex> Tree.max(%Tree.Leaf{value: 1})
    1
    iex> Tree.max(%Tree.Branch{left: %Tree.Leaf{value: 4}, right: %Tree.Leaf{value: 1}})
    4
  """
  @spec max(Tree.t()) :: number()
  def max(%Leaf{} = a), do: a.value
  def max(%Branch{} = b), do: max_helper(max(b.left), max(b.right))
  def max_helper(a, b) do
    case a > b do
      true -> a
      _ -> b
    end
  end

  @doc """
    Write a function depth that returns the maximum path length from the root of a tree
    to any leaf.
    iex> Tree.depth(%Tree.Leaf{value: 1})
    1
    iex> Tree.depth(%Tree.Branch{left: %Tree.Leaf{value: 4}, right: %Tree.Leaf{value: 1}})
    2
  """
  @spec depth(Tree.t()) :: number()
  def depth(%Leaf{}), do: 1
  def depth(%Branch{} = b), do: max_helper(1 + depth(b.left), 1 + depth(b.right))

  @doc """
    Write a function map , analogous to the method of the same name on List , that modi-
    fies each element in a tree with a given function.
    iex> Tree.map(Tree.Branch.new(Tree.Leaf.new(1), Tree.Leaf.new(2))).(fn x -> x + 1 end)
    %Tree.Branch{left: %Tree.Leaf{value: 2}, right: %Tree.Leaf{value: 3}}

  """

  @spec map(Tree.t()) :: ((any()) -> any())
  def map(tree), do: fn (f) ->
    case tree do
      %Leaf{value: x} -> %Leaf{value: f.(x)}
      %Branch{left: x, right: y} -> %Branch{left: map(x).(f), right: map(y).(f)}
    end
  end

  @doc """
    Generalize size , maximum , depth , and map , writing a new function fold that abstracts
    over their similarities. Reimplement them in terms of this more general function. Can
    you draw an analogy between this fold function and the left and right folds for List ?

    def fold[A,B](as: Tree[A], z: B)(f: (A, B) => B): B
    iex> Tree.fold(Tree.Branch.new(Tree.Branch.new(Tree.Leaf.new(1), Tree.Leaf.new(42)), Tree.Leaf.new(2)), 0).(fn (x,y) -> Tree.max_helper(x, y) end)
    42
  """

  @spec fold(Tree.t(), any()) :: ((any(), any()) -> any()) | any()
  def fold(tree, acc) do
     fn(f) ->
       case tree do
        %Leaf{value: x} -> f.(x, acc)
        %Branch{left: x, right: y} -> fold(y, (fold(x, acc).(f))).(f)
       end
     end
  end
end
